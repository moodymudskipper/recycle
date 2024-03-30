#' @rdname recycle
#' @export
new_cycle <- function(hook, ..., delta = 1, trigger = NULL) {
  combine_checks(
    abort_not_function(hook),
    abort_not_integerish(delta),
    abort_not_function(trigger, null_ok = TRUE),
    abort_not_same_env(hook, trigger)
  )
  new_cycle_impl(hook, delta, trigger, FALSE)
}

#' @rdname recycle
#' @export
new_cycle_bg <- function(hook, ..., delta = 1, trigger = NULL) {
  combine_checks(
    abort_not_function(hook),
    abort_not_integerish(delta),
    abort_not_function(trigger, null_ok = TRUE),
    abort_not_same_env(hook, trigger)
  )
  new_cycle_impl(hook, delta, trigger, TRUE)
}

new_cycle_impl <- function(hook, delta, trigger, bg) {
  mask <- initiate_mask(hook)
  environment(hook) <- new.env(parent = mask)
  if (!is.null(trigger)) environment(trigger) <- new.env(parent = mask)
  lazy_args <- Filter(function(x) !identical(x, quote(expr = )), formals(hook))
  formals(hook) <- formals(hook)[!names(formals(hook)) %in% names(lazy_args)]
  structure(
    list(
      hook = hook,
      delta = delta,
      trigger = trigger,
      bg = bg,
      mask = mask,
      lazy_args = lazy_args,
      process = NULL
    ),
    class = "cycle"
  )
}

initiate_mask <- function(hook) {
  mask <- new.env()
  if (identical(environment(hook), .GlobalEnv)) {
    parent.env(mask) <- .BaseNamespaceEnv
  } else {
    # presumably the namespace when using recycle in a package
    parent.env(mask) <- environment(hook)
  }
  # the mask env contain itself, so it's easy to manipulate through the functions
  mask$.mask <- mask
  mask$.last_time <- NULL
  mask$.trigger_value <- place_holder_trigger_value()
  mask
}

#' @rdname recycle
#' @param name name of the cycle to return the process from
#' @export
cycle_process <- function(name) {
  globals$cycles[[name]]$process
}

#' @export
print.cycle <- function(x, ...) {
  if (is.null(x$mask$.last_time)) {
    header <- "Never run"
  } else {
    if (x$bg) {
      header <- sprintf(
        "Last run: %s, %s",
        x$last %||% "never",
        sub("\n$", "", format(x$process))
      )
    } else {
      header <- sprintf("Last run: %s", x$last %||% "never")
    }
  }

  fun <- if (x$bg) "new_cycle_bg" else "new_cycle"
  call_ <- call(fun, x$hook, delta = x$delta)
  writeLines(c("<cycle>", header))
  print(call_)
  invisible(x)
}
