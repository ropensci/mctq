test_that("tbt() | scalar test", {
    bt <- hms::parse_hm("22:00")
    gu <- hms::parse_hm("07:00")
    object <- tbt(bt, gu)
    expected <- lubridate::dhours(9)
    expect_equal(object, expected)

    bt <- hms::parse_hm("02:00")
    gu <- hms::parse_hm("10:00")
    object <- tbt(bt, gu)
    expected <- lubridate::dhours(8)
    expect_equal(object, expected)

    bt <- hms::as_hms(NA)
    gu <- hms::parse_hm("00:00")
    object <- tbt(bt, gu)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("tbt() | vector test", {
    bt <- c(hms::parse_hm("23:30"), hms::parse_hm("03:15"))
    gu <- c(hms::parse_hm("12:00"), hms::parse_hm("10:45"))
    object <- tbt(bt, gu)
    expected <- c(lubridate::duration(45000), lubridate::duration(27000))
    expect_equal(object, expected)
})

test_that("tbt() | error test", {
    # Invalid values for `bt` and `gu`
    expect_error(tbt(1, hms::hms(1)))
    expect_error(tbt(hms::hms(1), 1))

    # `bt` and `gu` have different lengths
    expect_error(tbt(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})
