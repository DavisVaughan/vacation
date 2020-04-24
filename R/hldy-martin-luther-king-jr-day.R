#' Christmas
#'
#' `hldy_martin_luther_king_jr_day()` is a holiday for Martin Luther King
#' Jr. Day, the third Monday in January.
#'
#' @inheritParams new_rholiday
#'
#' @return
#' A rholiday.
#'
#' @export
#' @examples
#' library(almanac)
#'
#' alma_search("1990-01-01", "1995-01-01", hldy_martin_luther_king_jr_day())
hldy_martin_luther_king_jr_day <- function(since = "1900-01-01",
                                           until = "2100-01-01",
                                           adjust_on = NULL,
                                           adjustment = NULL) {
  new_rholiday(
    name = "Martin Luther King Jr. Day",
    since = since,
    until = until,
    generator = hldy_martin_luther_king_jr_day_generator,
    adjust_on = adjust_on,
    adjustment = adjustment
  )
}

hldy_martin_luther_king_jr_day_generator <- function(since, until) {
  hldy_start <- as.Date("1986-01-01")

  # Completely before holiday starts
  if (since < hldy_start && until < hldy_start) {
    rbundle <- rbundle()
    return(rbundle)
  }

  # Straddling holiday start
  if (since < hldy_start && until >= hldy_start) {
    since <- hldy_start
  }

  rrule <- yearly(since = since, until = until)
  rrule <- recur_on_ymonth(rrule, 1L)
  rrule <- recur_on_wday(rrule, 1L, nth = 3L)

  rrule
}
