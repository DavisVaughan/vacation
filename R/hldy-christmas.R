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
