test_that("print method for rcalendar is informative", {
  verify_output(test_path("output", "test-rcalendar-print.txt"), {
    "# basic method"
    rcalendar()
    rcalendar("USA")

    "# can add holidays"
    add_rholiday(rcalendar(), hldy_christmas)
    add_rholiday(add_rholiday(rcalendar(), hldy_christmas), hldy_martin_luther_king_jr_day)

    "# can add rschedules / rdates / exdates"
    add_rschedule(rcalendar(), weekly())
    add_rdate(rcalendar(), "2019-01-01")
    add_exdate(rcalendar(), "2019-01-01")
  })
})
