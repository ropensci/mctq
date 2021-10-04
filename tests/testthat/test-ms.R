test_that("ms() | scalar test", {
    expect_equal(ms(hms::parse_hm("22:00"), lubridate::dhours(8)),
                 hms::parse_hm("02:00"))
    expect_equal(ms(hms::parse_hm("02:00"), lubridate::dhours(6)),
                 hms::parse_hm("05:00"))
    expect_equal(ms(hms::as_hms(NA), lubridate::dhours(6)), hms::as_hms(NA))
})

test_that("ms() | vector test", {
    expect_equal(ms(c(hms::parse_hm("23:30"), hms::parse_hm("03:30")),
                    c(lubridate::dhours(8), lubridate::dhours(10))),
                 c(hms::parse_hm("03:30"), hms::parse_hm("08:30")))
    expect_equal(ms(c(hms::parse_hm("04:15"), hms::parse_hm("21:00")),
                    c(lubridate::dhours(6.5), lubridate::as.duration(NA))),
                 c(hms::parse_hm("07:30"), hms::as_hms(NA)))
})

test_that("ms() | error test", {
    expect_error(ms(1, lubridate::duration(1)),
                 "Assertion on 'so' failed")
    expect_error(ms(hms::hms(1), 1),
                 "Assertion on 'sd' failed")
    expect_error(ms(hms::hms(1), c(lubridate::duration(1),
                                   lubridate::duration(1))))
})
