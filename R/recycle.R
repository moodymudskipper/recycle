#' Set a hook on garbage collection
#' @param f The hook, an argument-less function to call whenever garbage collection is triggered
#' @param delta Minimal number of seconds between calls to the hook
#' @param args Arguments to `f`, useful mostly if `background` is `TRUE`
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

  # initiate the last call time
  globals$last <- Sys.time() - delta - 1
  # set the hook
  recycle0(f, delta, args, background, ...)

  # call the hook a first time
  gc()

  invisible(NULL)
}

recycle0 <- function(f, delta = 1, args = NULL, background = FALSE, ...) {
  e <- new.env()
  reg.finalizer(e, onexit = TRUE, function(e) {
    if (globals$on) {
      recycle0(f, delta, args, background, ...)
      enough_time_passed <-
        difftime(Sys.time(), globals$last, units = "secs") > delta
      if (enough_time_passed) {
        if (background) {
          callr::r_bg(f, args = args, ...)
        } else {
          do.call(f, args)
        }

        globals$last <- Sys.time()
      }
    }
  })
}

globals <- new.env()


