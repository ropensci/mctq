test_that("napd() | scalar test", {
    napo <- hms::parse_hm("14:00")
    nape <- hms::parse_hm("17:30")
    object <- napd(napo, nape)
    expected <- lubridate::dhours(3.5)
    expect_equal(object, expected)

    napo <- hms::parse_hm("23:00")
    nape <- hms::parse_hm("00:00")
    object <- napd(napo, nape)
    expected <- lubridate::dhours(1)
    expect_equal(object, expected)

    napo <- hms::as_hms(NA)
    nape <- hms::parse_hm("15:45")
    object <- napd(napo, nape)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("napd() | vector test", {
    napo <- c(hms::parse_hm("03:30"), hms::parse_hm("22:00"))
    nape <- c(hms::parse_hm("04:00"), hms::parse_hm("01:00"))
    object <- napd(napo, nape)
    expected <- c(lubridate::dhours(0.5), lubridate::dhours(3))
    expect_equal(object, expected)
})

test_that("napd() | error test", {
    # Invalid values for `napo` and `nape`
    expect_error(napd(1, hms::hms(1)))
    expect_error(napd(hms::hms(1), 1))

    # `napo` and `nape` have different lengths
    expect_error(napd(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})
