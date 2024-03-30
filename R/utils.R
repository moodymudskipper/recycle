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

update_locked <- function(sym, env, val) {
  # to avoid note, we're safe here because we're unlocking what we locked
  # ourselves
  get("unlockBinding")(sym, env)
  env[[sym]] <- val
  lockBinding(sym, env)
}

place_holder_trigger_value <- function() {
  structure(NA, class = "recycle_untriggered")
}

first_run <- function(cycle) {
  inherits(cycle$mask$.trigger_value, "recycle_untriggered")
}
