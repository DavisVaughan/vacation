calendar <- function(name = NULL,
                     since = "1970-01-01",
                     until = "2040-01-01",
                     adjust_on = NULL,
                     adjustment = NULL) {
  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  validate_adjust_on_and_adjustment(adjust_on, adjustment)

  new_calendar(
    name = name,
    since = since,
    until = until,
    adjust_on = adjust_on,
    adjustment = adjustment
  )
}

# ------------------------------------------------------------------------------

#' @export
rbundle_restore.calendar <- function(x, to) {
  new_calendar(
    name = to$name,
    since = to$since,
    until = to$until,
    adjust_on = to$adjust_on,
    adjustment = to$adjustment,
    rholidays = to$rholidays,

    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------

add_rholiday <- function(x,
                         rholiday_fn,
                         ...,
                         since = NULL,
                         until = NULL,
                         adjust_on = NULL,
                         adjustment = NULL) {
  ellipsis::check_dots_empty()

  validate_calendar(x, x_arg = "x")
  validate_rholiday_fn(rholiday_fn, x_arg = "rholiday_fn")

  since <- since %||% x$since
  until <- until %||% x$until
  adjust_on <- adjust_on %||% x$adjust_on
  adjustment <- adjustment %||% x$adjustment

  # Build the rholiday
  rholiday <- rholiday_fn(since, until, adjust_on, adjustment)

  # Add to both the rholiday list and the total rschedules list
  rholidays <- c(x$rholidays, list(rholiday))
  rschedules <- c(x$rschedules, list(rholiday))

  new_calendar(
    name = x$name,
    since = x$since,
    until = x$until,
    adjust_on = x$adjust_on,
    adjustment = x$adjustment,
    rholidays = rholidays,
    rschedules = rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------

new_calendar <- function(name,
                         since,
                         until,
                         adjust_on,
                         adjustment,
                         rholidays = list(),
                         rschedules = list(),
                         rdates = new_date(),
                         exdates = new_date()) {
  if (!(is.null(name) || is_string(name))) {
    abort("`name` must be a size 1 character vector or `NULL`.")
  }

  if (!is_list(rholidays)) {
    abort("`rholidays` must be a list of rholidays.")
  }

  new_rbundle(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,

    name = name,
    since = since,
    until = until,
    adjust_on = adjust_on,
    adjustment = adjustment,
    rholidays = rholidays,

    class = "calendar"
  )
}

# ------------------------------------------------------------------------------

is_calendar <- function(x) {
  inherits(x, "calendar")
}

validate_calendar <- function(x, x_arg = "calendar") {
  if (!is_calendar(x)) {
    glubort("`{x_arg}` must be a calendar.")
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

validate_rholiday_fn <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_function(x)) {
    glubort("Input{x_arg} must be a function.")
  }

  names <- fn_fmls_names(x)

  if (length(names) != 4L) {
    glubort("Input{x_arg} must have 4 arguments.")
  }

  if (!identical(names, c("since", "until", "adjust_on", "adjustment"))) {
    glubort("Input{x_arg} must have argument names: `since`, `until`, `adjust_on`, `adjustment`.")
  }

  invisible(x)
}
