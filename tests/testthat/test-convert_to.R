# convert_to() --------------------

test_that("convert_to() | conversion from date/time objects to units", {
    object <- convert_to(lubridate::dhours(), "numeric", output_unit = "M")
    expect_equal(object, 60)

    object <- convert_to(lubridate::days(), "numeric", output_unit = "rad")
    expect_equal(object, 6.28318530717959)

    object <- convert_to(lubridate::as_datetime("1985-10-20 12:00:00"),
                         "numeric", output_unit = "d")
    expect_equal(object, 0.5)

    object <- convert_to(lubridate::as_datetime("1985-10-20 12:00:00"),
                         "numeric", output_unit = "d", ignore_date = FALSE)
    expect_equal(object, 5771.5)

    object <- convert_to(hms::parse_hm("15:45:00"), "numeric",
                         output_unit = "H")
    expect_equal(object, 15.75)

    object <- convert_to_tu(hms::parse_hm("15:45:00"), "H")
    expect_equal(object, 15.75)
})

test_that("convert_to() | conversion from units to date/time objects", {
    object <- convert_to(360, "period", input_unit = "deg")
    expect_equal(object, convert_to_ut(360, "period", "deg"))

    object <- convert_to(6.5, "posixct", input_unit = "H")
    expect_equal(object, lubridate::ymd_hms("1970-01-01 06:30:00"))

    object <- convert_to(365.25, "hms", input_unit = "d")
    expect_equal(object, hms::as_hms(as.numeric(lubridate::dhours(8766))))

    object <- convert_to(1, "posixlt", input_unit = "W")
    expect_equal(object, lubridate::as_datetime("1970-01-08"))

    object <- convert_to(1.308997, "duration", input_unit = "rad")
    expect_equal(object, lubridate::dhours(5))

    object <- convert_to_ut(1.308997, "duration", "rad")
    expect_equal(object, lubridate::dhours(5))
})

test_that("convert_to() | conversion between date/time objects", {
    object <- convert_to(lubridate::duration(120), "hms")
    expect_equal(object, hms::parse_hm("00:02"))

    object <- convert_to(hms::as_hms("13:45:05"), "POSIXct")
    expect_equal(object, lubridate::as_datetime("1970-01-01 13:45:05"))

    object <- convert_to(lubridate::period(60), "POSIXct")
    expect_equal(object, lubridate::as_datetime("1970-01-01 00:01:00"))

    object <- convert_to(lubridate::as_date("1765-10-05"), "POSIXct")
    expect_equal(object, lubridate::as_datetime("1765-10-05"))

    object <- convert_to(lubridate::ymd_hms("2020-01-01 12:31:05", tz = "EST"),
                         "POSIXct")
    expect_equal(object, lubridate::parse_date_time("2020-01-01 12:31:05",
                                                    "ymd HMS"))

    object <- convert_to(as.POSIXct(NA), "POSIXct")
    expect_equal(object, lubridate::force_tz(as.POSIXct(NA), "UTC"))

    object <- convert_to_tt(lubridate::ymd_hms("2020-01-01 12:31:05",
                                               tz = "EST"), "POSIXct")
    expect_equal(object, lubridate::parse_date_time("2020-01-01 12:31:05",
                                                    "ymd HMS"))
})

test_that("convert_to() | conversion between units", {
    object <- convert_to(1.308997, "numeric", input_unit = "rad",
                         output_unit = "H")
    expect_equal(object, 5)

    object <- convert_to(60, "numeric", input_unit = "deg", output_unit = "rad")
    expect_equal(object, 1.0471975511966)

    object <- convert_to(1, "numeric", input_unit = "m", output_unit = "y")
    expect_equal(object, 0.0833333333333333)

    object <- convert_to(0.2617994, "numeric", input_unit = "rad",
                         output_unit = "H")
    expect_equal(object, 1)

    object <- convert_to(40, "numeric", input_unit = "d", output_unit = "deg")
    expect_equal(object, 14400)

    object <- convert_to_uu(40, "d", "deg")
    expect_equal(object, 14400)
})

test_that("convert_to() | conversion from p. objects to date/time objects", {
    object <- convert_to("19:55:17", "duration", orders = "HMS")
    expect_equal(object, lubridate::dseconds(71717))

    object <- convert_to("21:00", "Period", orders = "HM")
    expect_equal(object, lubridate::hours(21))

    object <- convert_to(1, "difftime", orders = "H")
    expect_equal(object, lubridate::as.difftime(lubridate::dhours(1)))

    object <- convert_to("10:00 PM", "hms", orders = "IMp")
    expect_equal(object, hms::parse_hm("22:00"))

    object <- convert_to("2020-01-01 10:00:00", "Date", orders = "ymd HMS")
    expect_equal(object, lubridate::as_date("2020-01-01"))

    object <- convert_to(13, "POSIXct", orders = "H")
    expect_equal(object, lubridate::as_datetime("1970-01-01 13:00:00 UTC"))

    object <- convert_to("2020-01-01 12:31:05", "POSIXct", orders = "ymd HMS",
                         tz = "EST")
    expect_equal(object, lubridate::parse_date_time("2020-01-01 12:31:05",
                                                    "ymd HMS", "EST"))

    object <- convert_to("03/07/1982 13:00", "POSIXlt", orders = "dmy HM")
    expect_equal(object, as.POSIXlt(lubridate::parse_date_time(
        "03/07/1982 13:00", "dmy HM")))

    object <- convert_to_pt("03/07/1982 13:00", "POSIXlt", "dmy HM")
    expect_equal(object, as.POSIXlt(lubridate::parse_date_time(
        "03/07/1982 13:00", "dmy HM")))
})

test_that("convert_to() | conversion between units", {
    object <- convert_to("0145", "numeric", orders = "HM", output_unit = "M")
    expect_equal(object, 105)

    object <- convert_to(45, "numeric", orders = "M", output_unit = "H")
    expect_equal(object, 0.75)

    object <- convert_to(4500, "numeric", orders = "HM", output_unit = "d")
    expect_equal(object, 1.875)

    object <- convert_to("2020-03-15 02", "numeric", orders = "ymd H",
                         output_unit = "H")
    expect_equal(object, 2)

    object <- convert_to("01:00", "numeric", orders = "HM", output_unit = "rad")
    expect_equal(object, 0.261799387799149)

    object <- convert_to_pu("01:00", "HM", "rad")
    expect_equal(object, 0.261799387799149)
})
