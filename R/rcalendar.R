rcalendar <- function(name = NULL,
                      since = "1900-01-01",
                      until = "2100-01-01",
                      adjust_on = NULL,
                      adjustment = NULL) {
  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  validate_adjust_on_and_adjustment(adjust_on, adjustment)

  new_rcalendar(
    name = name,
    since = since,
    until = until,
    adjust_on = adjust_on,
    adjustment = adjustment
  )
}

# ------------------------------------------------------------------------------

#' Add an rholiday to an rcalendar
#'
#' @description
#' `add_rholiday()` adds a new holiday to a calendar. To add a holiday, pass
#' through the corresponding holiday _function_, without the parenthesis.
#' `add_rholiday()` will take care of constructing the rholiday object with
#' the correct arguments.
#'
#' Arguments passed on to the `rholiday_fn` (such as `since` and `adjust_on`)
#' default to the corresponding arguments in the rcalendar, but can be
#' overridden at the rholiday level by supplying them in `add_rholiday()`.
#'
#' @param x `[rcalendar]`
#'
#'   An rcalendar.
#'
#' @param rholiday_fn `[function]`
#'
#'   An rholiday _function_, such as `hldy_christmas`. The actual function
#'   should be supplied, and `add_rholiday()` will take care of calling it
#'   with the correct arguments.
#'
#' @param ... Not used.
#'
#' @param since `[NULL / Date(1)]`
#'
#'   If `NULL`, default to the `since` date of the rcalendar, `x`.
#'
#'   Otherwise, the lower bound on where to begin looking for the holiday.
#'
#' @param until `[NULL / Date(1)]`
#'
#'   If `NULL`, default to the `until` date of the rcalendar, `x`.
#'
#'   Otherwise, the upper bound on where to begin looking for the holiday.
#'
#' @param adjust_on `[NULL / rschedule]`
#'
#'   If `NULL`, default to the `adjust_on` of the rcalendar, `x`.
#'
#'   Otherwise, an rschedule that defines when an adjustment to the holiday
#'   should be made. For example, set to an rschedule for "on weekends", and
#'   supply an `adjustment` of `adj_nearest()` to roll the holiday to the
#'   nearest weekday.
#'
#' @param adjustment `[NULL / function]`
#'
#'   If `NULL`, default to the `adjustment` of the rcalendar, `x`.
#'
#'   Otherwise, an adjustment function to apply to problematic dates. Typically
#'   one of the pre-existing adjustment functions, like
#'   [almanac::adj_nearest()].
#'
#' @return
#' `x`, with a new rholiday.
#'
#' @export
#' @examples
#' library(almanac)
#'
#' cal <- rcalendar() %>%
#'   add_rholiday(hldy_christmas) %>%
#'   add_rholiday(hldy_new_years_day)
#'
#' cal
#'
#' alma_in("1999-12-25", cal)
#'
#'
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' # Construct a calendar that rolls weekend Christmas dates to
#' # the nearest weekday
#' cal2 <- rcalendar() %>%
#'   add_rholiday(
#'     hldy_christmas,
#'     adjust_on = on_weekends,
#'     adjustment = adj_nearest
#'   ) %>%
#'   add_rholiday(hldy_new_years_day)
#'
#' alma_search("2004-01-01", "2010-01-01", cal2)
add_rholiday <- function(x,
                         rholiday_fn,
                         ...,
                         since = NULL,
                         until = NULL,
                         adjust_on = NULL,
                         adjustment = NULL) {
  ellipsis::check_dots_empty()

  validate_rcalendar(x, x_arg = "x")
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

  new_rcalendar(
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

#' @export
rbundle_restore.rcalendar <- function(x, to) {
  new_rcalendar(
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

new_rcalendar <- function(name,
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

    class = "rcalendar"
  )
}

# ------------------------------------------------------------------------------

is_rcalendar <- function(x) {
  inherits(x, "rcalendar")
}

validate_rcalendar <- function(x, x_arg = "rcalendar") {
  if (!is_rcalendar(x)) {
    glubort("`{x_arg}` must be a rcalendar.")
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
