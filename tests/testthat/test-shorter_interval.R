test_that("shorter_interval() | scalar test", {
    expect_equal(shorter_interval(hms::parse_hm("23:00"),
                                  hms::parse_hm("01:00")),
                 lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 23:00:00"),
                     lubridate::ymd_hms("1970-01-02 01:00:00")))

    # `x == y`
    expect_equal(shorter_interval(lubridate::as_datetime("1985-01-15 12:00:00"),
                                  lubridate::as_datetime(
                                      "2020-09-10 12:00:00")),
                 lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 12:00:00"),
                     lubridate::ymd_hms("1970-01-01 12:00:00")))

    # `inverse = TRUE` (longer interval)
    expect_equal(shorter_interval(lubridate::as_datetime("1985-01-15 12:00:00"),
                                  lubridate::as_datetime("2020-09-10 12:00:00"),
                                  inverse = TRUE),
                 lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 12:00:00"),
                     lubridate::ymd_hms("1970-01-02 12:00:00")))

    # `NA` cases
    expect_equal(shorter_interval(hms::parse_hm("23:00"), hms::as_hms(NA)),
                 lubridate::as.interval(NA))
})

test_that("shorter_interval() | vector test", {
    expect_equal(shorter_interval(c(hms::parse_hm("22:45"),
                                    hms::parse_hm("12:00")),
                                  c(hms::parse_hm("04:15"),
                                    hms::parse_hm("09:00"))),
                 c(lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 22:45:00"),
                     lubridate::ymd_hms("1970-01-02 04:15:00")),
                   lubridate::as.interval(
                       lubridate::ymd_hms("1970-01-01 09:00:00"),
                       lubridate::ymd_hms("1970-01-01 12:00:00"))))
})

test_that("shorter_interval() | warning test", {
    expect_message(shorter_interval(hms::parse_hm("00:00"),
                                    hms::parse_hm("12:00")),
                   "Element '1' of 'x' and 'y' have intervals equal to ")
})

test_that("shorter_interval() | error test", {
    expect_error(shorter_interval(1, hms::hms(1)),
                 "Assertion on 'x' failed")
    expect_error(shorter_interval(hms::hms(1), 1),
                 "Assertion on 'y' failed")
    expect_error(shorter_interval(hms::hms(1), hms::hms(1), 1),
                 "Assertion on 'inverse' failed")

    expect_error(shorter_interval(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})

test_that("shorter_interval() | wrappers", {
    expect_equal(longer_interval(lubridate::parse_date_time("01:10:00", "HMS"),
                                 lubridate::parse_date_time("11:45:00", "HMS")),
                 lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 11:45:00"),
                     lubridate::ymd_hms("1970-01-02 01:10:00")))

    # `x == y`
    expect_equal(longer_interval(lubridate::as_datetime("1915-02-14 05:00:00"),
                                 lubridate::as_datetime("1970-07-01 05:00:00")),
                 lubridate::as.interval(
                     lubridate::ymd_hms("1970-01-01 05:00:00"),
                     lubridate::ymd_hms("1970-01-02 05:00:00")))
})
