test_that("ms() | scalar test", {
    so <- hms::parse_hm("22:00")
    sd <- lubridate::dhours(8)
    object <- ms(so, sd)
    expected <- hms::parse_hm("02:00")
    expect_equal(object, expected)

    so <- hms::parse_hm("02:00")
    sd <- lubridate::dhours(6)
    object <- ms(so, sd)
    expected <- hms::parse_hm("05:00")
    expect_equal(object, expected)

    so <- hms::as_hms(NA)
    sd <- lubridate::dhours(6)
    object <- ms(so, sd)
    expected <- hms::as_hms(NA)
    expect_equal(object, expected)
})

test_that("ms() | vector test", {
    so <- c(hms::parse_hm("23:30"), hms::parse_hm("03:30"))
    sd <- c(lubridate::dhours(8), lubridate::dhours(10))
    object <- ms(so, sd)
    expected <- c(hms::parse_hm("03:30"), hms::parse_hm("08:30"))
    expect_equal(object, expected)

    so <- c(hms::parse_hm("04:15"), hms::parse_hm("21:00"))
    sd <- c(lubridate::dhours(6.5), lubridate::as.duration(NA))
    object <- ms(so, sd)
    expected <- c(hms::parse_hm("07:30"), hms::as_hms(NA))
    expect_equal(object, expected)
})

test_that("ms() | error test", {
    # Invalid values for `so` and `sd`
    expect_error(ms(1, lubridate::duration(1)))
    expect_error(ms(hms::hms(1), 1))

    # `so` and `sd` have different lengths
    expect_error(ms(hms::hms(1), c(lubridate::duration(1),
                                   lubridate::duration(1))))
})
