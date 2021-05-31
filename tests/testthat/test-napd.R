test_that("napd() | scalar test", {
    expect_equal(napd(hms::parse_hm("14:00"), hms::parse_hm("17:30")),
                 lubridate::dhours(3.5))
    expect_equal(napd(hms::parse_hm("23:00"), hms::parse_hm("00:00")),
                 lubridate::dhours(1))
    expect_equal(napd(hms::as_hms(NA), hms::parse_hm("15:45")),
                 lubridate::as.duration(NA))
})

test_that("napd() | vector test", {
    expect_equal(napd(c(hms::parse_hm("03:30"), hms::parse_hm("22:00")),
                      c(hms::parse_hm("04:00"), hms::parse_hm("01:00"))),
                 c(lubridate::dhours(0.5), lubridate::dhours(3)))
})

test_that("napd() | error test", {
    expect_error(napd(1, hms::hms(1)), "Assertion on 'napo' failed")
    expect_error(napd(hms::hms(1), 1), "Assertion on 'nape' failed")

    expect_error(napd(hms::hms(1), c(hms::hms(1), hms::hms(1))),
                 "'napo' and 'nape' must have identical lengths.")
})
