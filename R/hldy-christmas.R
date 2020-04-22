hldy_christmas <- function(adjust_on = NULL, adjustment = NULL) {
  new_hldy(
    "Christmas",
    hldy_christmas_generator,
    adjust_on,
    adjustment
  )
}

hldy_christmas_generator <- function(since, until) {
  rrule <- yearly(since, until)
  rrule <- recur_on_ymonth(rrule, 12L)
  rrule <- recur_on_mday(rrule, 25L)
  rrule
}
