test_that("gu() | scalar test", {
    se <- hms::parse_hm("08:00")
    si <- lubridate::dminutes(10)
    object <- gu(se, si)
    expected <- hms::parse_hm("08:10")
    expect_equal(object, expected)

    se <- hms::parse_hm("23:30")
    si <- lubridate::dminutes(90)
    object <- gu(se, si)
    expected <- hms::parse_hm("01:00")
    expect_equal(object, expected)

    se <- hms::parse_hm("23:30")
    si <- lubridate::as.duration(NA)
    object <- gu(se, si)
    expected <- hms::as_hms(NA)
    expect_equal(object, expected)
})

test_that("gu() | vector test", {
    se <- c(hms::parse_hm("12:30"), hms::parse_hm("23:45"))
    si <- c(lubridate::dminutes(10), lubridate::dminutes(70))
    object <- gu(se, si)
    expected <- c(hms::parse_hm("12:40"), hms::parse_hm("00:55"))
    expect_equal(object, expected)
})

test_that("gu() | error test", {
    # Invalid values for `se` and `si`
    expect_error(gu(1, lubridate::duration(1)))
    expect_error(gu(hms::hms(1), 1))

    # `se` and `si` have different lengths
    expect_error(gu(hms::hms(1), c(lubridate::duration(1),
                                   lubridate::duration(1))))
})
