test_that("sdu() | scalar test", {
    expect_equal(sdu(hms::parse_hm("23:30"), hms::parse_hm("07:30")),
                 lubridate::dhours(8))
    expect_equal(sdu(hms::parse_hm("01:30"), hms::parse_hm("10:00")),
                 lubridate::dhours(8.5))
    expect_equal(sdu(hms::as_hms(NA), hms::parse_hm("08:00")),
                 lubridate::as.duration(NA))
})

test_that("sdu() | vector test", {
    expect_equal(sdu(c(hms::parse_hm("21:00"), hms::parse_hm("02:00")),
                    c(hms::parse_hm("05:00"), hms::parse_hm("11:00"))),
                 c(lubridate::dhours(8), lubridate::dhours(9)))
})

test_that("sdu() | error test", {
    expect_error(sdu(1, hms::hms(1)), "Assertion on 'so' failed")
    expect_error(sdu(hms::hms(1), 1), "Assertion on 'se' failed")

    expect_error(sdu(hms::hms(1), c(hms::hms(1), hms::hms(1))),
                 "'so' and 'se' must have identical lengths.")
})
