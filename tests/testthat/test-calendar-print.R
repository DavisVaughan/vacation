test_that("print method for calendar is informative", {
  verify_output(test_path("output", "test-calendar-print.txt"), {
    "# basic method"
    calendar()
    calendar("USA")

    "# can add holidays"
    add_rholiday(calendar(), hldy_christmas)
    add_rholiday(add_rholiday(calendar(), hldy_christmas), hldy_martin_luther_king_jr_day)

    "# can add rschedules / rdates / exdates"
    add_rschedule(calendar(), weekly())
    add_rdate(calendar(), "2019-01-01")
    add_exdate(calendar(), "2019-01-01")
  })
})
