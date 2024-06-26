# taken from constructive
combine_checks <- function(
    ..., # unnamed expresions and named arg to forward to abort, such as `class`
    class = NULL,
    call,
    header = NULL,
    body = NULL,
    footer = NULL,
    trace = NULL,
    parent = NULL,
    use_cli_format = NULL,
    .internal = FALSE,
    .file = NULL,
    .frame = parent.frame(),
    .trace_bottom = NULL) {
  env <- parent.frame()
  dots <- eval(substitute(alist(...)))
  unnamed_dots <- dots[rlang::names2(dots) == ""]
  named_dots <- dots[rlang::names2(dots) != ""]
  named_dots <- eval(named_dots, env)
  err <- header
  for (expr in unnamed_dots) {
    new_err <- try(eval(expr, env), silent = TRUE)
    if (!missing(new_err) && inherits(new_err, "try-error")) {
      err <- c(err, "!" = attr(new_err, "condition")$message, attr(new_err, "condition")$body)
    }
  }
  if (!is.null(err)) {
    names(err)[1] <- ""
    do.call(rlang::abort, c(list(
      err,
      class = class,
      call = if (missing(call)) env else call,
      body = body,
      footer = footer,
      trace = trace,
      parent = parent,
      use_cli_format = use_cli_format,
      .internal = .internal,
      .file = .file,
      .frame = .frame,
      .trace_bottom = .trace_bottom
    ),
    named_dots))
  }
}

# adapted from constructive
describe <- function(x) {
  type <- typeof(x)
  info <- sprintf("It has type '%s'", typeof(x))
  if (!is.null(oldClass(x))) {
    cl <- sprintf("<%s>", paste(class(x), collapse = "/"))
    if (type %in% c("logical", "integer", "double", "complex", "character", "raw", "list")) {
      info <- sprintf("It has type '%s', class '%s' and length %s", typeof(x), cl, length(x))
      return(info)
    }
      info <- sprintf("It has type '%s' and class '%s'", typeof(x), cl)
      return(info)
  }
  if (type %in% c("logical", "integer", "double", "complex", "character", "raw", "list")) {
    info <- sprintf("It has type '%s' and length %s", typeof(x), length(x))
    return(info)
  }
  info <- sprintf("It has type '%s'", typeof(x))
}

abort_not_boolean <- function(x) {
  var <- as.character(substitute(x))
  if (!rlang::is_bool(x)) {
    msg <- sprintf("`%s` is not a boolean (scalar `TRUE` or `FALSE`)", var)
    abort(c(msg, i = describe(x)), call = parent.frame())
  }
}

abort_not_integerish <- function(x) {
  var <- as.character(substitute(x))
  if (!rlang::is_integerish(x)) {
    msg <- sprintf("`%s` is not an integerish numeric (such as `1` or `1L`)", var)
    abort(c(msg, i = describe(x)), call = parent.frame())
  }
}

abort_not_function <- function(x, null_ok = FALSE) {
  var <- as.character(substitute(x))
  if (!rlang::is_function(x)) {
    if (!null_ok) {
      msg <- sprintf("`%s` is not a function", var)
      abort(c(msg, i = describe(x)), call = parent.frame())
    } else if (!is.null(x)) {
      msg <- sprintf("`%s` is not a function or NULL", var)
      abort(c(msg, i = describe(x)), call = parent.frame())
    }
  }
}

abort_wrong_cycles <- function(cycles) {
  wrong_cycles <- Filter(function(x) !is.null(x) && !inherits(x, "cycle"), cycles)
  if (!length(wrong_cycles)) return(invisible(NULL))
  msg <- "`recycle()` takes only `NULL` or objects created by `new_cycle()` or `new_cycle_bg()`"
  info1 <- sprintf(
    "Args that are not cycles: %s",
    toString(sprintf("`%s`", names(wrong_cycles)))
  )
  info2 <- "Did you mean to pass them to `new_cycle()` or `new_cycle_bg()`?"
  abort(c(msg, x = info1, i = info2), call = parent.frame())
}

abort_not_same_env <- function(hook, trigger) {
  same_env_or_irrelevant <-
    is.null(trigger) || identical(environment(hook), environment(trigger))
  if (same_env_or_irrelevant) return(invisible(NULL))
  msg <- "`hook` and `trigger` must have the same environment"
  info1 <- sprintf(
    "`environment(hook)` is %s and `environment(trigger)` is %s",
    format(environment(hook)),
    format(environment(trigger))
  )
  info2 <- "Usually their enclosure is .GlobalEnv for both, or a namespace for both"
  abort(c(msg, x = info1, i = info2), call = parent.frame())
}
