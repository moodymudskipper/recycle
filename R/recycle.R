#' Set a hook on garbage collection
#' @param f An argument-less function to call whenever garbage collection is triggered
#'
#' @export
recycle <- function(f, delta = 1) {
  # clean the previous hook
  globals$on <- FALSE
  gc()
  globals$on <- TRUE

  if (is.null(f)) return(invisible(NULL))

  # initiate the last call time
  globals$last <- Sys.time() - delta - 1
  # set the hook
  recycle0(f, delta)

  # call the hook a first time
  gc()

  invisible(NULL)
}

recycle0 <- function(f, delta = 1) {
  e <- new.env()
  reg.finalizer(e, onexit = TRUE, function(e) {
    if (globals$on) {
      recycle0(f, delta)
      enough_time_passed <-
        difftime(Sys.time(), globals$last, units = "secs") > delta
      if (enough_time_passed) {
        f()
        globals$last <- Sys.time()
      }
    }
  })
}

globals <- new.env()
