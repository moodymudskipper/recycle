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
#' @param ... For `recycle()`: hooks to be added or replaced, defined with `new_cycle()` or
#'   `new_cycle_bg()`. Name them or they'll be names using `make.names()`.
#'   Unused in `new_cycle()` and `new_cycle_bg()`.
#' @param hook A function to call whenever garbage collection is triggered.
#'   For `new_cycle_bg()` The default values of arguments are evaluated when the hook
#'   is called and copied to the newly created session.
#' @param trigger A function returning a value to compare with the previous one.
#'   the hook is called only if the output of trigger has changed.
#' @param delta Minimal number of seconds between calls to the hook.
#'
#' @section environments and run-time data:
#'
#' `hook` and `trigger` must be in the same environment, and this environment
#'   will be altered.
#'
#' 2 new environments are to consider:
#' * `args_env` : an environment for arguments evaluated at run time from the global environment.
#' * `mask` : an environment to store and fetch data, that can be used from `hook`
#'   and `trigger` using the `mask$my_variable` and `mask$my_variable <- my_value`
#'
#' The alteration depends on which function is called and if the original environment
#' is `.GlobalEnv` :
#'
#' * for `new_cycle()`, if its enclosure is `.GlobalEnv` we will change it to be:
#'   `args_env => data mask => .BaseNamespaceEnv => .GlobalEnv => ...` .
#'   This is done so we have access to global objects but yet can't override
#'   base objects.
#' * for `new_cycle()` in other cases, presuming its defined in a namespace,
#'   we will change it to be:
#'   `args_env => data mask => ns => ...`
#' * for `new_cycle_bg()`
#'  `.GlobalEnv` we will change it to be:
#'   `args_env => data mask => .BaseNamespaceEnv => .GlobalEnv => ...`.
#'   Note that `.GlobalEnv` will be empty in this situation since we start from
#'   a clean session.
#'
#' @return Returns `cycles()` invisibly, called for side effects.
#' @export
recycle <- function(...) {
  cycles <- list(...)
  nms <- names2(cycles)
  nms <- make.names(nms, unique = TRUE)
  names(cycles) <- nms
  abort_wrong_cycles(cycles)

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
  #   run right now a hook that would run every 30 min or so for instance.
  # We could make this optional
  gc()
  invisible(cycles())
}

set_hooks <- function() {
  e <- new.env()
  reg.finalizer(e, finalizer)
  gc() # not totally understood but we need this
  invisible(NULL)
}

# we put the finalizer in the namespace so we have access to unexported objects
finalizer <- function(e) {
  # to be able to use load_all()
  clean_up_obsolete_namespace_artifacts()

  # for uncycle() : interrupts the recursion
  if (!globals$init) return(invisible(NULL))
  # setup the next iteration
  set_hooks()

  for (nm in names(globals$cycles)) {
    cycle <- globals$cycles[[nm]]
    # skip iteration if not enough time passed
    if (!enough_time_passed(cycle)) next
    populate_enclosure(cycle)
    # !! side effects + return value !!
    triggered <- is.null(cycle$trigger) || run_trigger(cycle)
    if (!triggered) next
    if (cycle$bg) {
      hook_bg(cycle$hook, nm)
    } else {
      cycle$hook()
    }
    update_locked(".last_time", cycle$mask, Sys.time())
  }
}

clean_up_obsolete_namespace_artifacts <- function() {
  # when running load_all() without uncycle() the old hooks are still running
  # in the old namespace, we need to shut those down
  if (!identical(rlang::env_parent(), asNamespace("recycle"))) {
    inform("The old namespace still had active hooks. Now cleaned up.")
    uncycle()
  }
}

# `now` arg for tests
enough_time_passed <- function(cycle, now = Sys.time()) {
  is.null(cycle$mask$.last_time) ||
    difftime(now, cycle$mask$.last_time, units = "secs") > cycle$delta
}

# `global_env` arg for tests
populate_enclosure <- function(cycle, global_env = .GlobalEnv) {
  list2env(
    x = lapply(cycle$lazy_args, eval, global_env),
    envir = environment(cycle$hook)
  )
}

run_trigger <- function(cycle) {
  if (first_run(cycle)) {
    update_locked(".trigger_value", cycle$mask, cycle$trigger())
    update_locked(".last_time", cycle$mask, Sys.time())
    return(FALSE)
  }
  new_value <- cycle$trigger()
  value_unchanged <- identical(new_value, cycle$mask$.trigger_value)
  if (value_unchanged) {
    return(FALSE)
  }
  update_locked(".trigger_value", cycle$mask, new_value)
  TRUE
}

hook_bg <- function(hook, nm) {
  # Calling the code in another session means we need to send the function
  # AND the necessary data.
  # This is done in `callr::r_bg()` by setting `package = TRUE`
  # (unfortunate name) that really means the env of the function will
  # be captured.
  # We take the lazy args, evaluate them in the cycle's env,
  # then place the cycle in an environment containing those
  globals$cycles[[nm]]$process <-
    callr::r_bg(
      hook,
      package = TRUE,
      supervise = TRUE,
      stdout = "",
      stderr = "2>&1"
    )
}
