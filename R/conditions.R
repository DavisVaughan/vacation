stop_vacation <- function(message = NULL, class = NULL, ...) {
  abort(message, class = c(class, "vacation_error"), ...)
}

stop_lossy_parse <- function(message) {
  stop_vacation(message = message, class = "vacation_error_lossy_parse")
}

stop_date_below_minimum <- function(message) {
  stop_vacation(message = message, class = "vacation_error_date_below_minimum")
}

stop_date_above_maximum <- function(message) {
  stop_vacation(message = message, class = "vacation_error_date_above_maximum")
}
