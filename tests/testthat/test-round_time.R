test_that("round_time() | scalar test", {
    expect_equal(round_time(lubridate::dmilliseconds(123456789)),
                 lubridate::duration(123457))
    expect_equal(round_time(lubridate::microseconds(123456789)),
                 round(lubridate::microseconds(123456789)))
    expect_equal(round_time(as.difftime(12345.6789, units = "secs")),
                 as.difftime(12346, units = "secs"))
    expect_equal(round_time(hms::as_hms(12345.6789)), hms::hms(12346))
    expect_equal(round_time(lubridate::as_datetime(12345.6789, tz = "EST")),
                 lubridate::as_datetime(12346, tz = "EST"))
    expect_equal(round_time(as.POSIXlt(lubridate::as_datetime(12345.6789,
                                                              tz = "EST"))),
                 as.POSIXlt(lubridate::as_datetime(12346, tz = "EST")))
})

test_that("round_time() | vector test", {
    expect_equal(round_time(c(hms::hms(12345.6789), hms::as_hms(98765.4321))),
                 c(hms::hms(12346), hms::as_hms(98765)))
})

test_that("round_time() | error test", {
    expect_error(round_time(1), "Assertion on 'x' failed")
})
