test_that("gu() | scalar test", {
    object <- gu(hms::parse_hm("08:00"), lubridate::dminutes(10))
    expected <- hms::parse_hm("08:10")
    expect_equal(object, expected)

    object <- gu(hms::parse_hm("23:30"), lubridate::dminutes(90))
    expected <- hms::parse_hm("01:00")
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
    expect_error(gu(1, 2))
    expect_error(gu(NA, NA))
    expect_error(gu("a", lubridate::dminutes(10)))
    expect_error(gu(hms::parse_hm("11:00"), lubridate::minutes(2)))

    # `se` and `si` have different lengths
    expect_error(gu(hms::parse_hm("10:15"), c(lubridate::dminutes(15),
                                              lubridate::dminutes(90))))
})
