#' @export
print.calendar <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.calendar <- function(x, ...) {
  header <- format_calendar_header(x)
  body <- format_calendar_body(x)

  if (is.null(body)) {
    header
  } else {
    glue(header, body, .sep = "\n")
  }
}

# ------------------------------------------------------------------------------

format_calendar_header <- function(x) {
  since <- x$since
  until <- x$until
  name <- x$name

  if (is.null(name)) {
    glue("<calendar[{since} / {until}]>")
  } else {
    glue("<calendar[{name} / {since} / {until}]>")
  }
}

format_calendar_body <- function(x) {
  rholidays <- x$rholidays

  if (length(rholidays) == 0L) {
    return(NULL)
  }

  names <- map_chr(rholidays, rholiday_name)
  names <- glue::glue("- {names}")
  names <- glue::glue_collapse(names, sep = "\n")

  names
}
