test_that("napd() | scalar test", {
    expect_equal(napd(
        napo = hms::parse_hm("14:00"), nape = hms::parse_hm("17:30")
    ),
    lubridate::dhours(3.5)
    )

    expect_equal(napd(
        napo = hms::parse_hm("23:00"), nape = hms::parse_hm("00:00")
    ),
    lubridate::dhours(1)
    )

    expect_equal(napd(
        napo = hms::as_hms(NA), nape = hms::parse_hm("15:45")
    ),
    lubridate::as.duration(NA)
    )
})

test_that("napd() | vector test", {
    expect_equal(napd(
        napo = c(hms::parse_hm("03:30"), hms::parse_hm("22:00")),
        nape = c(hms::parse_hm("04:00"), hms::parse_hm("01:00"))
    ),
    c(lubridate::dhours(0.5), lubridate::dhours(3))
    )
})

test_that("napd() | error test", {
    # assert_hms(napo, lower = hms::hms(0))
    expect_error(napd(
        napo = 1, nape = hms::hms(1)
    ),
    "Assertion on 'napo' failed"
    )

    # assert_hms(nape, lower = hms::hms(0))
    expect_error(napd(
        napo = hms::hms(1), nape = 1
    ),
    "Assertion on 'nape' failed"
    )

    # assert_identical(napo, nape, type = "length")
    expect_error(napd(
        napo = hms::hms(1), nape = c(hms::hms(1), hms::hms(1)))
    )
})
