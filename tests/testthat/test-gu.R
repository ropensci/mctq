test_that("gu() | scalar test", {
    expect_equal(gu(hms::parse_hm("08:00"), lubridate::dminutes(10)),
                 hms::parse_hm("08:10"))
    expect_equal(gu(hms::parse_hm("23:30"), lubridate::dminutes(90)),
                 hms::parse_hm("01:00"))
    expect_equal(gu(hms::parse_hm("23:30"), lubridate::as.duration(NA)),
                 hms::as_hms(NA))
})

test_that("gu() | vector test", {
    expect_equal(gu(c(hms::parse_hm("12:30"), hms::parse_hm("23:45")),
                    c(lubridate::dminutes(10), lubridate::dminutes(70))),
                 c(hms::parse_hm("12:40"), hms::parse_hm("00:55")))
})

test_that("gu() | error test", {
    expect_error(gu(1, lubridate::duration(1)),
                 "Assertion on 'se' failed")
    expect_error(gu(hms::hms(1), 1),
                 "Assertion on 'si' failed")

    expect_error(gu(hms::hms(1), c(lubridate::duration(1),
                                   lubridate::duration(1))),
                 "'se' and 'si' must have identical lengths.")
})
