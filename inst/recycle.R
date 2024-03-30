#' Set a hook on garbage collection
#' @param f The hook, a function to call whenever garbage collection is triggered.
#'   The default values of arguments are evaluated when the hook is called, unlike
#'   `args` that is evaluated when `recycle()` is called.
#' @param delta Minimal number of seconds between calls to the hook
#' @param args Arguments to `f`
#' @param background Whether to run in the background, i.e. run in a separate session and
#' not wait for the output, uses `callr::r_bg()`
#' @param ... Additional arguments passed to `callr::r_bg()`, irrelevant if
#' `background` is `FALSE`
#'
#' @export
recycle <- function(f, delta = 1, args = list(), background = FALSE, ...) {
  # clean the previous hook
  globals$on <- FALSE
  gc()
  globals$on <- TRUE

  if (is.null(f)) return(invisible(NULL))

  # we build an environment with promises to  lazy args
  if (background) {
    pf <- parent.frame()
    lazy_args <- Filter(function(x) !identical(x, quote(expr =)), formals(f))
    formals(f) <- formals(f)[!names(formals(f)) %in% names(lazy_args)]
  }

  # initiate the last call time
  globals$last <- Sys.time() - delta - 1

  # set the hook
  recycle0(f, delta, args, background, lazy_args, pf, ...)

  # call the hook a first time
  gc()

  invisible(NULL)
}

recycle0 <- function(f, delta = 1, args = NULL, background = FALSE, lazy_args, pf, ...) {
  e <- new.env()
  reg.finalizer(e, onexit = TRUE, function(e) {
    if (globals$on) {
      recycle0(f, delta, args, background, lazy_args, pf, ...)
      enough_time_passed <-
        difftime(Sys.time(), globals$last, units = "secs") > delta
      if (enough_time_passed) {
        if (background) {
          f_env <- as.environment(lapply(lazy_args, eval, pf))
          parent.env(f_env) <- pf
          environment(f) <- f_env
          callr::r_bg(f, args = args, ..., package = TRUE)
        } else {
          do.call(f, args)
        }

        globals$last <- Sys.time()
      }
    }
  })
}

globals <- new.env()



