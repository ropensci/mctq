test_that("shorter_interval() | scalar test", {
    expect_equal(shorter_interval(hms::parse_hm("23:00"),
                                  hms::parse_hm("01:00")),
                 hms::parse_hm("02:00"))

    # `x == y`
    expect_equal(shorter_interval(lubridate::as_datetime("1985-01-15 12:00:00"),
                                  lubridate::as_datetime(
                                      "2020-09-10 12:00:00")),
                 hms::parse_hm("00:00"))

    # `inverse = TRUE` (longer interval)
    expect_equal(shorter_interval(lubridate::as_datetime("1985-01-15 12:00:00"),
                                  lubridate::as_datetime("2020-09-10 12:00:00"),
                                  inverse = TRUE),
                 hms::parse_hm("24:00"))

    # `NA` cases
    expect_equal(shorter_interval(hms::parse_hm("23:00"), hms::as_hms(NA)),
                 hms::as_hms(NA))
})

test_that("shorter_interval() | vector test", {
    expect_equal(shorter_interval(c(hms::parse_hm("22:45"),
                                    hms::parse_hm("12:00")),
                                  c(hms::parse_hm("04:15"),
                                    hms::parse_hm("09:00"))),
                 c(hms::parse_hm("05:30"), hms::parse_hm("03:00")))
})

test_that("shorter_interval() | 'class' test", {
    expect_equal(longer_interval(as.POSIXct("1988-10-05 02:00:00"),
                                 as.POSIXlt("2100-05-07 13:30:00"),
                                 "Duration"),
                 lubridate::dhours(12.5))
    expect_equal(shorter_interval(as.POSIXct("1988-10-05 02:00:00"),
                                  as.POSIXlt("2100-05-07 13:30:00"),
                                  "Period"),
                 lubridate::hours(11) + lubridate::minutes(30))
    expect_equal(longer_interval(as.POSIXct("1988-10-05 02:00:00"),
                                 as.POSIXlt("2100-05-07 13:30:00"),
                                 "difftime"),
                 lubridate::as.difftime(lubridate::dhours(12.5)))
    expect_equal(shorter_interval(as.POSIXct("1988-10-05 02:00:00"),
                                  as.POSIXlt("2100-05-07 13:30:00"),
                                  "hms"),
                 hms::parse_hm("11:30"))
    expect_equal(longer_interval(as.POSIXct("1988-10-05 02:00:00"),
                                 as.POSIXlt("2100-05-07 13:30:00"),
                                 "Interval"),
                 lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 13:30:00"),
                     lubridate::ymd_hms("1970-01-02 02:00:00")))
})

test_that("shorter_interval() | warning test", {
    expect_message(shorter_interval(hms::parse_hm("00:00"),
                                    hms::parse_hm("12:00"), "Interval",
                                    quiet = FALSE),
                   "Element\\(s\\) '1' of 'x' and 'y' have intervals equal to ")
})

test_that("shorter_interval() | error test", {
    expect_error(shorter_interval(1, hms::hms(1), "hms", TRUE),
                 "Assertion on 'x' failed")
    expect_error(shorter_interval(hms::hms(1), 1, "hms", TRUE),
                 "Assertion on 'y' failed")
    expect_error(shorter_interval(hms::hms(1), hms::hms(1), "", TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(shorter_interval(hms::hms(1), hms::hms(1), "hms", 1),
                 "Assertion on 'inverse' failed")

    expect_error(shorter_interval(hms::hms(1), c(hms::hms(1), hms::hms(1))),
                 "'x' and 'y' must have identical lengths.")
})

test_that("shorter_interval() | wrappers", {
    expect_equal(longer_interval(lubridate::parse_date_time("01:10:00", "HMS"),
                                 lubridate::parse_date_time("11:45:00", "HMS"),
                                 "hms"),
                 hms::parse_hm("13:25"))

    # `x == y`
    expect_equal(longer_interval(lubridate::as_datetime("1915-02-14 05:00:00"),
                                 lubridate::as_datetime("1970-07-01 05:00:00")),
                 hms::parse_hm("24:00"))
})
