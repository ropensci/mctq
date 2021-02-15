test_that("shortest_interval() | scalar test", {
    x <- hms::parse_hm("23:00")
    y <- hms::parse_hm("01:00")
    object <- shortest_interval(x, y)
    expect_equal(object, hms::parse_hm("02:00"))

    # `x == y`
    x <- lubridate::as_datetime("1985-01-15 12:00:00")
    y <- lubridate::as_datetime("2020-09-10 12:00:00")
    object <- shortest_interval(x, y)
    expect_equal(object, hms::parse_hm("00:00"))

    # `inverse = TRUE` (longer interval)
    x <- lubridate::as_datetime("1985-01-15 12:00:00")
    y <- lubridate::as_datetime("2020-09-10 12:00:00")
    object <- shortest_interval(x, y, inverse = TRUE)
    expect_equal(object, hms::parse_hm("24:00"))

    # `NA` cases
    x <- hms::parse_hm("23:00")
    y <- hms::as_hms(NA)
    object <- shortest_interval(x, y)
    expect_equal(object, hms::as_hms(NA))
})

test_that("shortest_interval() | vector test", {
    x <- c(hms::parse_hm("22:45"), hms::parse_hm("12:00"))
    y <- c(hms::parse_hm("04:15"), hms::parse_hm("09:00"))
    object <- shortest_interval(x, y)
    expected <- c(hms::parse_hm("05:30"), hms::parse_hm("03:00"))
    expect_equal(object, expected)
})

test_that("shortest_interval() | `class` test", {
    x <- as.POSIXct("1988-10-05 02:00:00")
    y <- as.POSIXlt("2100-05-07 13:30:00")

    object <- longer_interval(x, y, "Duration")
    expect_equal(object, lubridate::dhours(12.5))

    object <- shortest_interval(x, y, "Period")
    expect_equal(object, lubridate::hours(11) + lubridate::minutes(30))

    object <- longer_interval(x, y, "difftime")
    expect_equal(object, lubridate::as.difftime(lubridate::dhours(12.5)))

    object <- shortest_interval(x, y, "hms")
    expect_equal(object, hms::parse_hm("11:30"))

    object <- longer_interval(x, y, "Interval")
    expected <- lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 13:30:00"),
        lubridate::ymd_hms("1970-01-02 02:00:00"))
    expect_equal(object, expected)
})

test_that("shortest_interval() | warning test", {
    expect_warning(shortest_interval(
        hms::parse_hm("00:00"), hms::parse_hm("12:00"), "Interval",
        quiet = FALSE))
})

test_that("shortest_interval() | error test", {
    # Invalid values for `x`, `y`, `class`, and `inverse`
    expect_error(shortest_interval(1, hms::hms(1), "hms", TRUE))
    expect_error(shortest_interval(hms::hms(1), 1, "hms", TRUE))
    expect_error(shortest_interval(hms::hms(1), hms::hms(1), "", TRUE))
    expect_error(shortest_interval(hms::hms(1), hms::hms(1), "hms", 1))

    # `x` and `y` have different lengths
    expect_error(shortest_interval(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})

test_that("shortest_interval() | wrappers", {
    x <- lubridate::parse_date_time("01:10:00", "HMS")
    y <- lubridate::parse_date_time("11:45:00", "HMS")
    object <- longer_interval(x, y, "hms")
    expect_equal(object, hms::parse_hm("13:25"))

    # `x == y`
    x <- lubridate::as_datetime("1915-02-14 05:00:00")
    y <- lubridate::as_datetime("1970-07-01 05:00:00")
    object <- longer_interval(x, y)
    expect_equal(object, hms::parse_hm("24:00"))
})
