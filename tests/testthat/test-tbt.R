test_that("tbt() | scalar test", {
    expect_equal(tbt(hms::parse_hm("22:00"), hms::parse_hm("07:00")),
                 lubridate::dhours(9))
    expect_equal(tbt(hms::parse_hm("02:00"), hms::parse_hm("10:00")),
                 lubridate::dhours(8))
    expect_equal(tbt(hms::as_hms(NA), hms::parse_hm("00:00")),
                 lubridate::as.duration(NA))
})

test_that("tbt() | vector test", {
    expect_equal(tbt(c(hms::parse_hm("23:30"), hms::parse_hm("03:15")),
                     c(hms::parse_hm("12:00"), hms::parse_hm("10:45"))),
                 c(lubridate::duration(45000), lubridate::duration(27000)))
})

test_that("tbt() | error test", {
    expect_error(tbt(1, hms::hms(1)),
                 "Assertion on 'bt' failed")
    expect_error(tbt(hms::hms(1), 1),
                 "Assertion on 'gu' failed")

    expect_error(tbt(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})
