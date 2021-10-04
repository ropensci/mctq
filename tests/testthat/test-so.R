test_that("so() | scalar test", {
    expect_equal(so(hms::parse_hm("23:15"), lubridate::dminutes(30)),
                 hms::parse_hm("23:45"))
    expect_equal(so(hms::parse_hm("02:45"), lubridate::dminutes(60)),
                 hms::parse_hm("03:45"))
    expect_equal(so(hms::parse_hm("00:00"), lubridate::as.duration(NA)),
                 hms::as_hms(NA))
})

test_that("so() | vector test", {
    expect_equal(so(c(hms::parse_hm("21:45"), hms::parse_hm("01:30")),
                    c(lubridate::dminutes(20), lubridate::dminutes(50))),
                 c(hms::parse_hm("22:05"), hms::parse_hm("02:20")))
})

test_that("so() | error test", {
    expect_error(so(1, lubridate::duration(1)),
                 "Assertion on 'sprep' failed")
    expect_error(so(hms::hms(1), 1),
                 "Assertion on 'slat' failed")

    expect_error(so(hms::hms(1), c(lubridate::duration(1),
                                   lubridate::duration(1))))
})
