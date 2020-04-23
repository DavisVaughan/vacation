#' New Year's Day
#'
#' `hldy_new_years_day()` is a holiday for New Year's Day, January 1st.
#'
#' @inheritParams new_rholiday
#'
#' @return
#' A rholiday.
#'
#' @export
#' @examples
#' alma_search("1990-01-01", "1995-01-01", hldy_new_years_day())
hldy_new_years_day <- function(since = "1900-01-01",
                               until = "2100-01-01",
                               adjust_on = NULL,
                               adjustment = NULL) {
  new_rholiday(
    name = "New Year's Day",
    since = since,
    until = until,
    generator = hldy_new_years_day_generator,
    adjust_on = adjust_on,
    adjustment = adjustment
  )
}

hldy_new_years_day_generator <- function(since, until) {
  rschedule <- yearly(since, until)
  rschedule <- recur_on_ymonth(rschedule, 1L)
  rschedule <- recur_on_mday(rschedule, 1L)
  rschedule
}
