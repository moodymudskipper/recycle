#' @rdname recycle
#' @export
new_cycle <- function(f, ..., delta = 1) {
  combine_checks(
    abort_not_function(f),
    abort_not_integerish(delta)
  )
  structure(
    list(f = f, delta = delta, bg = FALSE, last = NULL),
    class = "cycle"
  )
}

#' @rdname recycle
#' @export
new_cycle_bg <- function(f, ..., delta = 1) {
  combine_checks(
    abort_not_function(f),
    abort_not_integerish(delta)
  )
  lazy_args <- Filter(function(x) !identical(x, quote(expr =)), formals(f))
  formals(f) <- formals(f)[!names(formals(f)) %in% names(lazy_args)]
  structure(
    list(
      f = f,
      delta = delta,
      bg = TRUE,
      last = NULL,
      lazy_args = lazy_args,
      process = NULL
      ),
    class = "cycle"
  )
}

cycle_process <- function(name) {
  globals$cycles[[name]]$process
}

#' @export
print.cycle <- function(x, ...) {
  if (is.null(x$last)) {
    header <- "Never run"
  } else {
    header <- sprintf("Last run: %s, status: %s", x$last %||% "never", format(x$process))
  }

  fun <- if (x$bg) "new_cycle_bg" else "new_cycle"
  call_ <- call(fun, x$f, delta = x$delta)
  writeLines(c("<cycle>", header))
  print(call_)
  invisible(x)
}
