#' Set a hooks on garbage collection
#'
#' `recycle()` sets the hooks using `new_cycle()` and `new_cycle_bg()`. `cycles()`
#' displays the existing hooks. `uncycle()` cleans everything up.
#' `cycle_process()` returns an actual process object for advanced use.
#'
#' * `new_cycle()` defines a hook on garbage collection
#' * `new_cycle_bg()` is similar but the hook will be run in the background,
#'   in a different session. The output is redirected to the current session,
#'   so if necessary, divert, capture or suppress it in `f`
#' * `recycle()` activates the hooks, if calls several times it will overwrite,
#'   add, or remove hooks. Hooks are run in order when deltas match.
#' * `cycle_process()` returns a process, of class "<r_process/process/R6>"
#'   documented in the 'processx' package
#'
#' @param ... For `recycle()`: hooks to be added or replaced, defined with `cycle()` or
#'   `cycle_bg()`. Name them or they'll be names using `make.names()`.
#'   Unused in `new_cycle()` and `new_cycle_bg()`.
#' @param f The hook, a function to call whenever garbage collection is triggered.
#'   For `new_cycle_bg()` The default values of arguments are evaluated when the hook
#'   is called and copied to the newly created session.
#' @param delta Minimal number of seconds between calls to the hook.
#' @return Returns `cycles()` invisibly, called for side effects.
#' @export
recycle <- function(...) {
  cycles <- list(...)
  nms <- names2(cycles)
  nms <- make.names(nms, unique = TRUE)
  names(cycles) <- nms
  # update global cycles (create new, overwrite or remove)
  globals$cycles[names(cycles)] <- cycles
  # actually remove those set to NULL
  globals$cycles <- Filter(Negate(is.null), globals$cycles)
  # hooks need to be set a single time, so we monitor if they're set with a flag
  if (!globals$init) {
    globals$init <- TRUE
    set_hooks()
  }
  # run hooks immediately, this is a design choice, assuming that we want to
  # run right now a hook that would run every 30 min or so for instance
  gc()
  invisible(cycles())
}

set_hooks <- function() {
  # define env to be garbage collected, and register a finalizer for it
  e <- new.env()
  reg.finalizer(e, finalizer)
  gc()
  invisible(NULL)
}

# we put the finalizer in the namespace so we have access to unexported objects
finalizer <- function(e) {
  # when running load_all() without uncycle() the old hooks are still running
  # in the old namespace, we need to shut those down
  if (!identical(rlang::env_parent(), asNamespace("recycle"))) {
    inform("The old namespace still had active hooks. Now cleaned up.")
    return(uncycle())
  }

  # relevant when calling uncycle(), this interrupts the recursion and cleans
  # up our infinite hooks (that would otherwise still run even with an empty
  # list of cycles)
  if (!globals$init) return(invisible(NULL))
  # setup the next iteration
  set_hooks()

  for (nm in names(globals$cycles)) {
    # skip iteration if not enough time passed
    enough_time_passed <-
      is.null(globals$cycles[[nm]]$last) ||
      difftime(Sys.time(), globals$cycles[[nm]]$last, units = "secs") >
      globals$cycles[[nm]]$delta
    if (!enough_time_passed) next
    f <- globals$cycles[[nm]]$f

    # trigger the hook
    if (globals$cycles[[nm]]$bg) {
      # Calling the code in another session means we need to send the function
      # AND the necessary data.
      # This is done in `callr::r_bg()` by setting `package = TRUE`
      # (unfortunate name) that really means the env of the function will
      # be captured.
      # We take the lazy args, evaluate them in the cycle's env,
      # then place the cycle in an environment containing those
      f_env <- as.environment(lapply(
        globals$cycles[[nm]]$lazy_args,
        eval,
        environment(f)
      ))
      parent.env(f_env) <- environment(f)
      environment(f) <- f_env
      globals$cycles[[nm]]$process <-
        callr::r_bg(
          f,
          package = TRUE,
          supervise = TRUE,
          stdout = "",
          stderr = "2>&1"
        )
    } else {
      f()
    }

    # update last time
    globals$cycles[[nm]]$last <- Sys.time()
  }
}

#' @rdname recycle
#' @export
uncycle <- function() {
  globals$init <- FALSE
  globals$cycles <- list()
  gc()
  invisible(NULL)
}

#' @rdname recycle
#' @export
cycles <- function() {
  globals$cycles
}
