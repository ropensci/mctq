test_that("shortest_interval() | shortest interval between two hours", {
    x <- hms::parse_hm("23:00")
    y <- hms::parse_hm("01:00")
    object <- shortest_interval(x, y)
    expect_equal(object, hms::parse_hms("02:00:00"))

    x <- lubridate::as_datetime("1985-01-15 12:00:00")
    y <- lubridate::as_datetime("2020-09-10 12:00:00")
    object <- shortest_interval(x, y)
    expect_equal(object, hms::parse_hms("00:00:00"))
})

test_that("x_interval() | changing the output object class", {
    x <- as.POSIXct("1988-10-05 02:00:00")
    y <- as.POSIXlt("2100-05-07 13:30:00")

    object <- shortest_interval(x, y, "Interval")
    expected <- lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 02:00:00"),
        lubridate::ymd_hms("1970-01-01 13:30:00"))
    expect_equal(object, expected)

    object <- longer_interval(x, y, "Duration")
    expect_equal(object, lubridate::dhours(12.5))

    object <- shortest_interval(x, y, "Period")
    expect_equal(object, lubridate::hours(11) + lubridate::minutes(30))

    object <- longer_interval(x, y, "POSIXct")
    expect_equal(object, lubridate::ymd_hms("1970-01-01 12:30:00"))
})

test_that("longer_interval() | longer interval between two hours", {
    x <- lubridate::parse_date_time("01:10:00", "HMS")
    y <- lubridate::parse_date_time("11:45:00", "HMS")
    object <- longer_interval(x, y)
    expect_equal(object, hms::parse_hms("13:25:00"))

    x <- lubridate::as_datetime("1915-02-14 05:00:00")
    y <- lubridate::as_datetime("1970-07-01 05:00:00")
    object <- longer_interval(x, y)
    expect_equal(object, hms::parse_hms("24:00:00"))
})
