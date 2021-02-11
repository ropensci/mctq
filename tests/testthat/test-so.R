test_that("so() | scalar test", {
    sprep <- hms::parse_hm("23:15")
    slat <- lubridate::dminutes(30)
    object <- so(sprep, slat)
    expected <- hms::parse_hm("23:45")
    expect_equal(object, expected)

    sprep <- hms::parse_hm("02:45")
    slat <- lubridate::dminutes(60)
    object <- so(sprep, slat)
    expected <- hms::parse_hm("03:45")
    expect_equal(object, expected)

    sprep <- hms::parse_hm("00:00")
    slat <- lubridate::as.duration(NA)
    object <- so(sprep, slat)
    expected <- hms::as_hms(NA)
    expect_equal(object, expected)
})

test_that("so() | vector test", {
    sprep <- c(hms::parse_hm("21:45"), hms::parse_hm("01:30"))
    slat <- c(lubridate::dminutes(20), lubridate::dminutes(50))
    object <- so(sprep, slat)
    expected <- c(hms::parse_hm("22:05"), hms::parse_hm("02:20"))
    expect_equal(object, expected)
})

test_that("so() | error test", {
    # Invalid values for `sprep` and `slat`
    expect_error(so(1, lubridate::duration(1)))
    expect_error(so(hms::hms(1), 1))

    # `sprep` and `slat` have different lengths
    expect_error(so(hms::hms(1), c(lubridate::duration(1),
                                   lubridate::duration(1))))
})
