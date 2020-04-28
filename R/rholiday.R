#' Create a new holiday
#'
#' `new_rholiday()` constructs a new rholiday object. An rholiday is an
#' rschedule that can be used with any almanac function, like
#' [almanac::alma_events()]. rholidays can also be added to rcalendars through
#' [add_rholiday()].
#'
#' @param name `[character(1)]`
#'
#'   A required name for the holiday.
#'
#' @param since `[Date(1)]`
#'
#'   The lower bound on where to begin looking for the holiday.
#'
#' @param until `[Date(1)]`
#'
#'   The upper bound on where to end looking for the holiday.
#'
#' @param generator `[function]`
#'
#'   A function that generates an rschedule for the holiday. The function
#'   takes two arguments, `since` and `until`, which should be passed to
#'   the created rschedule.
#'
#' @param adjust_on `[NULL / rschedule]`
#'
#'   If `NULL`, no adjustment is made.
#'
#'   Otherwise, an rschedule that defines when an adjustment should be made.
#'   For example, set to an rschedule for "on weekends", and
#'   supply an `adjustment` of `adj_nearest()` to roll holidays that fall
#'   on a weekend to the nearest weekday.
#'
#' @param adjustment `[NULL / function]`
#'
#'   If `NULL`, no adjustment is made to the holiday.
#'
#'   Otherwise, an adjustment function to apply to problematic dates. Typically
#'   one of the pre-existing adjustment functions, like
#'   [almanac::adj_nearest()].
#'
#' @return
#' A rholiday.
#'
#' @export
#' @examples
#' library(almanac)
#'
#' thanksgiving_generator <- function(since, until) {
#'   yearly(since = since, until = until) %>%
#'     recur_on_ymonth("November") %>%
#'     recur_on_wday("Thursday", nth = 4)
#' }
#'
#' thanksgiving <- new_rholiday(
#'   name = "thanksgiving",
#'   since = "1950-01-01",
#'   until = "1980-01-01",
#'   generator = thanksgiving_generator
#' )
#'
#' thanksgiving
#'
#' alma_events(thanksgiving)
new_rholiday <- function(name,
                         since,
                         until,
                         generator,
                         adjust_on = NULL,
                         adjustment = NULL) {
  if (!is_string(name)) {
    abort("`name` must be a size 1 character vector.")
  }

  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  validate_adjust_on_and_adjustment(adjust_on, adjustment)
  validate_generator(generator)

  rschedule <- generator(since, until)
  rschedule <- rholiday_adjust(rschedule, adjust_on, adjustment)

  new_rschedule(
    name = name,
    since = since,
    until = until,
    rschedule = rschedule,
    class = "rholiday"
  )
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
  since <- x$since
  until <- x$until

  glue("<rholiday[{name} / {since} / {until}]>")
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

  names <- fn_fmls_names(generator)

  if (length(names) != 2L || !identical(names, c("since", "until"))) {
    abort("`generator` must have two arguments, `since` and `until`.")
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

  names <- fn_fmls_names(x)

  if (length(names) != 2L || !identical(names, c("x", "rschedule"))) {
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
