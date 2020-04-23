test_that("is on christmas", {
  x <- hldy_christmas(since = "2000-01-01", until = "2005-01-01")

  expect <- as.Date(paste0(2000:2004, "-12-25"))

  expect_identical(
    alma_events(x),
    expect
  )
})
