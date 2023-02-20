test_that("sdu() | scalar test", {
    expect_equal(
        sdu(so = hms::parse_hm("23:30"), se = hms::parse_hm("07:30")),
        lubridate::dhours(8)
    )

    expect_equal(
        sdu(so = hms::parse_hm("01:30"), se = hms::parse_hm("10:00")),
        lubridate::dhours(8.5)
    )

    expect_equal(
        sdu(so = hms::as_hms(NA), se = hms::parse_hm("08:00")),
        lubridate::as.duration(NA)
    )
})

test_that("sdu() | vector test", {
    expect_equal(sdu(
        so = c(hms::parse_hm("21:00"), hms::parse_hm("02:00")),
        se = c(hms::parse_hm("05:00"), hms::parse_hm("11:00"))
    ),
    c(lubridate::dhours(8), lubridate::dhours(9))
    )
})

test_that("sdu() | error test", {
    # assert_hms(so, lower = hms::hms(0))
    expect_error(sdu(so = 1, se = hms::hms(1)), "Assertion on 'so' failed")

    expect_error(
        sdu(so = hms::hms(-1), se = hms::hms(1)),
        "Assertion on 'so' failed"
    )

    # assert_hms(se, lower = hms::hms(0))
    expect_error(sdu(so = hms::hms(1), se = 1), "Assertion on 'se' failed")

    expect_error(
        sdu(so = hms::hms(1), se = hms::hms(-1)),
        "Assertion on 'se' failed"
    )

    # assert_identical(so, se, type = "length")
    expect_error(sdu(
        so = hms::hms(1), se = c(hms::hms(1), hms::hms(1)))
    )
})
