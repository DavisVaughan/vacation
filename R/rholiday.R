new_rholiday <- function(name, since, until, adjust_on, adjustment, generator) {
  if (!is_string(name)) {
    abort("`name` must be a size 1 character vector.")
  }

  validate_generator(generator)
  validate_adjust_on_and_adjustment(adjust_on, adjustment)

  rschedule <- generator(since, until)
  rschedule <- rholiday_adjust(rschedule, adjust_on, adjustment)

  data <- list(
    name = name,
    since = since,
    until = until,
    rschedule = rschedule
  )

  new_rschedule(data, "rholiday")
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.rholiday <- function(x) {
  rschedule_events(x$rschedule)
}

# ------------------------------------------------------------------------------

#' @export
print.rholiday <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.rholiday <- function(x, ...) {
  name <- rholiday_name(x)
  glue("<rholiday[{name}]>")
}

# ------------------------------------------------------------------------------

is_rholiday <- function(x) {
  inherits(x, "rholiday")
}

validate_rholiday <- function(x, x_arg = "rholiday") {
  if (!is_rholiday(x)) {
    glubort("`{x_arg}` must be an rholiday.")
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

rholiday_adjust <- function(rschedule, adjust_on, adjustment) {
  if (is.null(adjust_on)) {
    return(rschedule)
  }

  radjusted(rschedule, adjust_on, adjustment)
}

rholiday_name <- function(x) {
  x$name
}

# ------------------------------------------------------------------------------

validate_generator <- function(generator) {
  if (!is_function(generator)) {
    abort("`generator` must be a function.")
  }

  fmls <- fn_fmls(generator)

  if (length(fmls) != 2L) {
    abort("`generator` must have two arguments, `since`, `until`.")
  }

  invisible(generator)
}

validate_adjust_on_and_adjustment <- function(adjust_on, adjustment) {
  adjust_on_supplied <- !is.null(adjust_on)
  adjustment_supplied <- !is.null(adjustment)

  if (xor(adjust_on_supplied, adjustment_supplied)) {
    abort("If one of `adjust_on` or `adjustment` is supplied, both must be supplied.")
  }

  if (adjust_on_supplied) {
    validate_rschedule(adjust_on, "adjust_on")
  }

  if (adjustment_supplied) {
    validate_adjustment(adjustment, "adjustment")
  }

  invisible()
}

validate_adjustment <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_function(x)) {
    glubort("Input{x_arg} must be a function.")
  }

  fmls <- fn_fmls(x)

  if (length(fmls) != 2L) {
    glubort("Input{x_arg} must have two arguments, `x` and `rschedule`.")
  }

  invisible(x)
}

is_rschedule <- function(x) {
  inherits(x, "rschedule")
}

validate_rschedule <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_rschedule(x)) {
    glubort("Input{x_arg} must be an rschedule, such as an rrule or rbundle.")
  }

  invisible(x)
}
