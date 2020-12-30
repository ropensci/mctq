# convert_to_date_time() --------------------

test_that("convert_to_date_time() | character objects", {
    # See why devtools::check() is throwing a error (devtools::test() is ok)
    object <- convert_to_date_time("0130", "character")
    expect_equal(object, "01:30:00")

    object <- convert_to_date_time("30", "character", "H")
    expect_equal(object, "30:00:00")

    object <- convert_to_date_time("01/02/1930", "character", "dmy")
    expect_equal(object, "1930-02-01")

    object <- convert_to_date_time("01/02/1930 2", "character", "mdy H")
    expect_equal(object, "1930-01-02 02:00:00")

    # See why devtools::check() is throwing a error (devtools::test() is ok)
    object <- convert_to_date_time("0130", "Duration")
    expect_equal(object, lubridate::duration(1.5, "hours"))

    object <- convert_to_date_time("30:03", "Duration", "MS")
    expect_equal(object, lubridate::duration(1803))

    object <- convert_to_date_time("20", "Period")
    expect_equal(object, lubridate::period(20, "hours"))

    object <- convert_to_date_time("1999-01-01 12:45:03", "Period", "ymd HMS")
    expect_equal(object, lubridate::as.period(hms::as_hms(
        lubridate::parse_date_time("1999-01-01 12:45:03", "ymd HMS"))))

    object <- convert_to_date_time("30:03", "difftime", "MS")
    expect_equal(object, as.difftime(lubridate::duration(1803), units = "secs"))

    object <- convert_to_date_time("10:00 PM", "hms")
    expect_equal(object, hms::as_hms("22:00:00"))

    expect_warning(convert_to_date_time("1045", "Date"))

    object <- convert_to_date_time("20/09/1985", "Date", "dmy")
    expect_equal(object, as.Date("1985-09-20"))

    object <- convert_to_date_time("2020-01-01 12:31:05", "POSIXct",
                                   "ymd HMS", tz = "EST")
    expect_equal(object, lubridate::parse_date_time("2020-01-01 12:31:05",
                                            "ymd HMS", tz = "EST"))

    object <- convert_to_date_time("03/07/1982 13:00", "POSIXlt", "dmy HM")
    expect_equal(object, lubridate::parse_date_time("03/07/1982 13:00",
                                                    "dmy HM"))
})

# See why devtools::check() is throwing a error (devtools::test() is ok)
test_that("convert_to_date_time() | numeric objects", {
    object <- convert_to_date_time("0130", "numeric")
    expect_equal(object, 5400)

    object <- convert_to_date_time(1012, "Duration")
    expect_equal(object, lubridate::duration(36720))
})

test_that("convert_to_date_time() | difftime objects", {
    object <- convert_to_date_time(as.difftime("15", "%H"), "hms")
    expect_equal(object, hms::as_hms(as.difftime("15", "%H")))
})

test_that("convert_to_date_time() | Duration objects", {
    object <- convert_to_date_time(lubridate::dhours(), "hms")
    expect_equal(object, hms::hms(lubridate::dhours()))
})

test_that("convert_to_date_time() | hms objects", {
    object <- convert_to_date_time(hms::as_hms("09:30:00"), "duration")
    expect_equal(object, lubridate::dhours(9.5))
})

test_that("convert_to_date_time() | Period objects", {
    object <- convert_to_date_time(lubridate::hours(), "hms")
    expect_equal(object, hms::hms(lubridate::hours()))
})

test_that("convert_to_date_time() | date objects", {
    object <- convert_to_date_time(as.Date("1988-04-03"), "POSIXct")
    expect_equal(object, lubridate::parse_date_time("1988-04-03", "ymd"))
})

test_that("convert_to_date_time() | POSIXct objects", {
    object <- convert_to_date_time("12", "POSIXct")
    expect_equal(object, lubridate::parse_date_time("12", "H"))
})

test_that("convert_to_date_time() | POSIXlt objects", {
    object <- convert_to_date_time(lubridate::weeks(1), "POSIXlt")
    expect_equal(object, as.POSIXlt(
        lubridate::parse_date_time("0000-01-08", "ymd")))
})

test_that("convert_to_date_time() | Interval objects", {
    object <- convert_to_date_time(lubridate::as.interval(
        lubridate::dhours(1), lubridate::ymd("2020-01-01")), "hms")
    expect_equal(object, hms::hms(lubridate::dhours(1)))
})


test_that("convert_to_date_time() | Interval objects", {
    object <- convert_to_date_time(lubridate::as.interval(
        lubridate::dhours(1), lubridate::ymd("2020-01-01")), "hms")
    expect_equal(object, hms::hms(lubridate::dhours(1)))
})

# convert_to_decimal() --------------------

test_that("convert_to_decimal() | date/time objects", {
    object <- convert_to_decimal(lubridate::dminutes(23), "M")
    expect_equal(object, 23)

    object <- convert_to_decimal(lubridate::dminutes(23), "H", round = TRUE)
    expect_equal(object, round(23 / 60, 3))

    object <- convert_to_decimal(
        lubridate::as.interval(lubridate::days(25),
                 lubridate::as_date("2020-01-01")), "m")
    expect_equal(object, lubridate::ddays(25) / lubridate::dmonths())
})

test_that("convert_to_decimal() | decimal dates", {
    object <- convert_to_decimal(lubridate::as_date("2020-03-01"),
                                 "date_decimal")
    expect_equal(object,
                 lubridate::decimal_date(lubridate::as_date("2020-03-01")))
})

test_that("convert_to_decimal() | radian values", {
    object <- convert_to_decimal(convert_to_rad(16.56))
    expect_equal(object, 16.56)
})

# convert_to_rad() --------------------

test_that("convert_to_rad() | date/time objects", {
    object <- convert_to_rad(lubridate::days(1), round = TRUE)
    expect_equal(object, 6.283)
})

test_that("convert_to_rad() | decimal time values", {
    object <- convert_to_rad(15.25, round = TRUE, digits = 5)
    expect_equal(object, 3.99244)
})
