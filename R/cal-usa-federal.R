#' Calendar - USA Federal
#'
#' @description
#' `cal_usa_federal()` is a federal calendar for the United States.
#'
#' @section Holidays:
#'
#' - Martin Luther King Jr. Day
#' - Christmas
#' - New Year's Day
#'
#' @inheritParams new_rholiday
#'
#' @param observed `[logical(1)]`
#'
#'   If the holiday falls on a weekend, should the observed date be returned?
#'   The observed date is the nearest weekday. If `FALSE`, the exact holiday
#'   date is returned.
#'
#' @return
#' An rcalendar.
#'
#' @export
#' @examples
#' library(almanac)
#'
#' cal <- cal_usa_federal()
#'
#' alma_search("2000-01-01", "2001-01-01", cal)
cal_usa_federal <- function(since = "1900-01-01",
                            until = "2100-01-01",
                            observed = TRUE) {
  if (observed) {
    adjust_on <- weekly(since = since, until = until)
    adjust_on <- recur_on_weekends(adjust_on)
    adjustment <- adj_nearest
  } else {
    adjust_on <- NULL
    adjustment <- NULL
  }

  calendar <- rcalendar(
    name = "USA-Federal",
    since = since,
    until = until,
    adjust_on = adjust_on,
    adjustment = adjustment
  )

  calendar <- add_rholiday(calendar, hldy_martin_luther_king_jr_day)
  calendar <- add_rholiday(calendar, hldy_christmas)
  calendar <- add_rholiday(calendar, hldy_new_years_day)

  calendar
}
