#' Christmas
#'
#' `hldy_christmas()` is a holiday for Christmas, December 25th.
#'
#' @inheritParams new_rholiday
#'
#' @return
#' A rholiday.
#'
#' @export
#' @examples
#' alma_search("1990-01-01", "1995-01-01", hldy_christmas())
#'
#' # Adjust weekend Christmas to nearest weekday
#' adjust_on <- weekly() %>%
#'   recur_on_weekends()
#'
#' christmas_adjusted <- hldy_christmas(
#'   adjust_on = adjust_on,
#'   adjustment = adj_nearest
#' )
#'
#' alma_search("1990-01-01", "1995-01-01", christmas_adjusted)
hldy_christmas <- function(since = "1900-01-01",
                           until = "2100-01-01",
                           adjust_on = NULL,
                           adjustment = NULL) {
  new_rholiday(
    name = "Christmas",
    since = since,
    until = until,
    generator = hldy_christmas_generator,
    adjust_on = adjust_on,
    adjustment = adjustment
  )
}

hldy_christmas_generator <- function(since, until) {
  rschedule <- yearly(since, until)
  rschedule <- recur_on_ymonth(rschedule, 12L)
  rschedule <- recur_on_mday(rschedule, 25L)
  rschedule
}
