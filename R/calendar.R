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
    adjustment = adjustment,
    rholidays = list()
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.calendar <- function(x) {
  rschedule_events(x$rbundle)
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

  rholiday <- rholiday_fn(since, until, adjust_on, adjustment)

  # TODO: Is this right? What if we change the since/until dates?
  if (rholiday_exists(rholiday, x)) {
    return(x)
  }

  rholidays <- c(x$rholidays, list(rholiday))

  new_calendar(
    name = x$name,
    since = x$since,
    until = x$until,
    adjust_on = x$adjust_on,
    adjustment = x$adjustment,
    rholidays = rholidays
  )
}

# ------------------------------------------------------------------------------

# Remove by name or by object that has that name
remove_rholiday <- function(x, rholiday) {
  validate_calendar(x, x_arg = "x")

  if (is_rholiday(rholiday)) {
    rholiday <- rholiday_name(rholiday)
  }
  if (!is_string(rholiday)) {
    abort("`rholiday` must be a single character name or an rholiday object.")
  }

  names <- calendar_names(x)

  indicator <- vec_in(rholiday, names)
  name_exists <- any(indicator)

  # Early return if name didn't exist
  if (!name_exists) {
    return(x)
  }

  keep <- !indicator

  rholidays <- x$rholidays
  rholidays <- rholidays[keep]

  new_calendar(
    name = x$name,
    since = x$since,
    until = x$until,
    adjust_on = x$adjust_on,
    adjustment = x$adjustment,
    rholidays = rholidays
  )
}

# ------------------------------------------------------------------------------

new_calendar <- function(name,
                         since,
                         until,
                         adjust_on,
                         adjustment,
                         rholidays) {
  if (!(is.null(name) || is_string(name))) {
    abort("`name` must be a size 1 character vector or `NULL`.")
  }

  if (!is_list(rholidays)) {
    abort("`rholidays` must be a list of rholidays.")
  }

  rbundle <- new_rbundle(rschedules = rholidays)

  data <- list(
    name = name,
    since = since,
    until = until,
    adjust_on = adjust_on,
    adjustment = adjustment,
    rholidays = rholidays,
    rbundle = rbundle
  )

  new_rschedule(data, class = "calendar")
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

rholiday_exists <- function(rholiday, calendar) {
  names <- calendar_names(calendar)
  rholiday_name(rholiday) %in% names
}

calendar_names <- function(x) {
  map_chr(x$rholidays, rholiday_name)
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
