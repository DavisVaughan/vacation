hldy_martin_luther_king_jr_day <- function(adjust_on = NULL, adjustment = NULL) {
  new_hldy(
    "Martin Luther King Jr. Day",
    hldy_martin_luther_king_jr_day_generator,
    adjust_on,
    adjustment
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