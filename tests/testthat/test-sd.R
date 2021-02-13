test_that("sd() | scalar test", {
    so <- hms::parse_hm("23:30")
    se <- hms::parse_hm("07:30")
    object <- sd(so, se)
    expected <- lubridate::dhours(8)
    expect_equal(object, expected)

    so <- hms::parse_hm("01:30")
    se <- hms::parse_hm("10:00")
    object <- sd(so, se)
    expected <- lubridate::dhours(8.5)
    expect_equal(object, expected)

    so <- hms::as_hms(NA)
    se <- hms::parse_hm("08:00")
    object <- sd(so, se)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("sd() | vector test", {
    so <- c(hms::parse_hm("21:00"), hms::parse_hm("02:00"))
    se <- c(hms::parse_hm("05:00"), hms::parse_hm("11:00"))
    object <- sd(so, se)
    expected <- c(lubridate::dhours(8), lubridate::dhours(9))
    expect_equal(object, expected)
})

test_that("sd() | error test", {
    # Invalid values for `so` and `se`
    expect_error(sd(1, hms::hms(1)))
    expect_error(sd(hms::hms(1), 1))

    # `so` and `se` have different lengths
    expect_error(sd(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})
