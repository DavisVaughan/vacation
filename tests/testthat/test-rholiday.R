# ------------------------------------------------------------------------------
# new_rholiday()

test_that("can create an rholiday", {
  generator <- function(since, until) weekly(since, until)
  x <- new_rholiday("x", "1990-01-01", "1991-01-01", generator)
  expect_s3_class(x, c("rholiday", "rschedule"), exact = TRUE)
})

test_that("rschedule_events() is defined and works", {
  generator <- function(since, until) daily(since, until)
  x <- new_rholiday("x", "1970-01-01", "1970-01-02", generator)
  expect_identical(rschedule_events(x), new_date(c(0, 1)))
})

test_that("adjustment is applied", {
  generator <- function(since, until) daily(since, until)
  adjust_on <- daily(since = "1970-01-02", until = "1970-01-02")

  x <- new_rholiday("x", "1970-01-01", "1970-01-02", generator, adjust_on, adj_following)

  expect_identical(alma_events(x), new_date(c(0, 2)))
})

test_that("name is validated", {
  generator <- function(since, until) daily(since, until)

  expect_error(
    new_rholiday(1, "1970-01-01", "1970-01-02", generator),
    "size 1 character vector"
  )
})

test_that("generator is validated", {
  generator <- function(since) daily(since)

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator),
    "must have two arguments"
  )

  generator <- function(foo, bar) daily()

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator),
    "must have two arguments"
  )
})

test_that("adjust_on and adjustment must be supplied together", {
  generator <- function(since, until) daily(since, until)

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator, adjust_on = weekly()),
    "both must be supplied"
  )

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator, adjustment = adj_nearest),
    "both must be supplied"
  )
})

test_that("`adjust_on` is validated", {
  generator <- function(since, until) daily(since, until)

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator, adjust_on = 1, adjustment = adj_nearest),
    "Input `adjust_on` must be an rschedule"
  )
})

test_that("`adjustment` is validated", {
  generator <- function(since, until) daily(since, until)

  adjustment <- function(x) x

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator, adjust_on = weekly(), adjustment = adjustment),
    "Input `adjustment` must have two arguments"
  )

  adjustment <- function(x, y) x

  expect_error(
    new_rholiday("x", "1970-01-01", "1970-01-02", generator, adjust_on = weekly(), adjustment = adjustment),
    "Input `adjustment` must have two arguments"
  )
})

# ------------------------------------------------------------------------------
# print method

test_that("rholiday has informative print method", {
  verify_output(test_path("output", "test-rholiday.txt"), {
    "# Basic print method"
    generator <- function(since, until) weekly(since, until)
    new_rholiday("hldy", "1990-01-01", "1991-01-01", generator)
  })
})


