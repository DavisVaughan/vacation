# ------------------------------------------------------------------------------
# Global variables

delayedAssign("vacation_global_empty_date", vctrs::new_date())

# JS rrule can't seem to handle dates outside this range, but that's fine
delayedAssign("vacation_global_max_date", as.Date("9999-12-31"))
delayedAssign("vacation_global_min_date", as.Date("0100-01-01"))

# ------------------------------------------------------------------------------

glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

# ------------------------------------------------------------------------------

check_since <- function(since) {
  since <- vec_cast_date(since, "since")
  vec_assert(since, size = 1L)

  if (is_missing_or_infinite(since)) {
    abort("`since` must be a finite date.")
  }

  validate_date_bounds(since, x_arg = "since")

  since
}

check_until <- function(until) {
  until <- vec_cast_date(until, "until")
  vec_assert(until, size = 1L)

  if (is_missing_or_infinite(until)) {
    abort("`until` must be a finite date.")
  }

  validate_date_bounds(until, x_arg = "until")

  until
}

# ------------------------------------------------------------------------------

vec_cast_date <- function(x, x_arg = "x") {
  if (is.character(x)) {
    vec_cast_date_from_character(x, x_arg)
  } else {
    vec_cast(x, vacation_global_empty_date, x_arg = x_arg)
  }
}

vec_cast_date_from_character <- function(x, x_arg) {
  # Gives POSIXct with no time component and UTC tz
  out <- lubridate::fast_strptime(x, format = "%Y-%m-%d", tz = "UTC", lt = FALSE)

  # Rely on fast behavior of POSIXct->Date when tz="UTC"
  out <- as.Date.POSIXct(out, tz = "UTC")

  # Check for new `NA` values, these are failed parses
  lossy <- is.na(out) & !is.na(x)

  if (any(lossy)) {
    message <- lossy_to_message(lossy, x_arg)
    stop_lossy_parse(message)
  }

  out
}

lossy_to_message <- function(lossy, x_arg) {
  locations <- which(lossy)
  locations <- as.character(locations)

  if (length(locations) > 1) {
    chr_locations <- "locations"
  } else {
    chr_locations <- "location"
  }

  if (length(locations) > 5) {
    locations <- c(locations[1:5], "etc")
  }

  locations <- glue::glue_collapse(locations, sep = ", ")

  locations

  glue::glue("Failed to parse `{x_arg}` to Date at {chr_locations}: {locations}.")
}

# ------------------------------------------------------------------------------

validate_date_bounds <- function(x, ..., x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (any(x > vacation_global_max_date, na.rm = TRUE)) {
    message <- glue("Input{x_arg} cannot be larger than {vacation_global_max_date}.")
    stop_date_above_maximum(message)
  }

  if (any(x < vacation_global_min_date, na.rm = TRUE)) {
    message <- glue("Input{x_arg} cannot be smaller than {vacation_global_min_date}.")
    stop_date_below_minimum(message)
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

is_missing_or_infinite <- function(x) {
  !is.finite(x)
}
