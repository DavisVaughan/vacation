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

  # The rschedules object contains user supplied ones and rholidays
  n_rschedules <- length(x$rschedules) - length(x$rholidays)
  n_rdates <- length(x$rdates)
  n_exdates <- length(x$exdates)

  if (is.null(x$name)) {
    name <- ""
  } else {
    name <- glue("{x$name} / ")
  }

  since <- glue("{since} / ")
  until <- glue("{until} / ")
  rschedules <- glue("{n_rschedules} rschedules / ")
  rdates <- glue("{n_rdates} rdates / ")
  exdates <- glue("{n_exdates} exdates")

  glue("<calendar[{name}{since}{until}{rschedules}{rdates}{exdates}]>")
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
