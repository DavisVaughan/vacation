test_that("is on MLK Jr Day", {
  x <- hldy_martin_luther_king_jr_day(since = "2000-01-01", until = "2005-01-01")

  expect <- as.Date(c(
    "2000-01-17",
    "2001-01-15",
    "2002-01-21",
    "2003-01-20",
    "2004-01-19"
  ))

  expect_identical(
    alma_events(x),
    expect
  )
})
