test_that("msf_sc() | scalar test", {
    expect_equal(msf_sc(
        msf = hms::parse_hm("04:00"), sd_w = lubridate::dhours(6),
        sd_f = lubridate::dhours(8),
        sd_week = sd_week(lubridate::dhours(6), lubridate::dhours(8), 5),
        alarm_f = FALSE
    ),
    hms::parse_hms("03:17:08.571429")
    )

    expect_equal(msf_sc(
        msf = hms::parse_hm("23:30"), sd_w = lubridate::dhours(7),
        sd_f = lubridate::dhours(9),
        sd_week = sd_week(lubridate::dhours(7), lubridate::dhours(9), 3),
        alarm_f = FALSE
    ),
    hms::parse_hms("23:04:17.142857")
    )

    expect_equal(msf_sc(
        msf = hms::parse_hm("02:15"), sd_w = lubridate::dhours(9),
        sd_f = lubridate::dhours(7),
        sd_week = sd_week(lubridate::dhours(9), lubridate::dhours(7), 5),
        alarm_f = FALSE
    ),
    hms::parse_hm("02:15")
    )

    expect_equal(msf_sc(
        msf = hms::parse_hm("00:00"), sd_w = lubridate::dhours(6.15),
        sd_f = lubridate::dhours(8.25), sd_week = lubridate::as.duration(NA),
        alarm_f = FALSE
    ),
    hms::as_hms(NA)
    )

    expect_equal(msf_sc(
        msf = hms::parse_hm("01:00"), sd_w = lubridate::dhours(8),
        sd_f = lubridate::dhours(5.5), sd_week = lubridate::as.duration(NA),
        alarm_f = FALSE
    ),
    hms::parse_hm("01:00")
    )

    expect_equal(msf_sc(
        msf = hms::parse_hm("02:25"), sd_w = lubridate::as.duration(NA),
        sd_f = lubridate::dhours(9.5),
        sd_week = sd_week(lubridate::as.duration(NA),
                          lubridate::dhours(9.5), 5),
        alarm_f = FALSE
    ),
    hms::as_hms(NA)
    )

    expect_equal(msf_sc(
        msf = hms::parse_hm("21:15"), sd_w = lubridate::dhours(8),
        sd_f = lubridate::dhours(8),
        sd_week = sd_week(lubridate::dhours(8), lubridate::dhours(8), 6),
        alarm_f = TRUE
    ),
    hms::as_hms(NA)
    )
})

test_that("msf_sc() | vector test", {
    expect_equal(msf_sc(
        msf = c(hms::parse_hm("03:45"), hms::parse_hm("22:30")),
        sd_w = c(lubridate::dhours(6), lubridate::dhours(5.5)),
        sd_f = c(lubridate::dhours(7.5), lubridate::dhours(8)),
        sd_week = c(sd_week(lubridate::dhours(6), lubridate::dhours(7.5), 5),
                    sd_week(lubridate::dhours(5.5), lubridate::dhours(8), 4)),
        alarm_f = c(FALSE, FALSE)
    ),
    c(hms::parse_hms("03:12:51.428571"), hms::parse_hms("21:47:08.571429"))
    )

    expect_equal(msf_sc(
        msf = c(hms::parse_hm("03:45"), hms::parse_hm("21:30")),
        sd_w = c(lubridate::dhours(7), lubridate::dhours(6.5)),
        sd_f = c(lubridate::dhours(7), lubridate::dhours(8)),
        sd_week = c(sd_week(lubridate::dhours(7), lubridate::dhours(7), 5),
                    sd_week(lubridate::dhours(7), lubridate::dhours(8), 5)),
        alarm_f = c(FALSE, TRUE)
    ),
    c(hms::parse_hm("03:45"), hms::as_hms(NA))
    )

    expect_equal(msf_sc(
        msf = c(hms::parse_hm("01:20"), hms::as_hms(NA)),
        sd_w = c(lubridate::dhours(5), lubridate::dhours(7)),
        sd_f = c(lubridate::dhours(8.5), lubridate::dhours(9)),
        sd_week = c(lubridate::as.duration(NA),
                    sd_week(lubridate::dhours(7), lubridate::dhours(9), 3)),
        alarm_f = c(FALSE, FALSE)
    ),
    c(hms::as_hms(NA), hms::as_hms(NA))
    )
})

test_that("msf_sc() | error test", {
    # assert_hms(msf, lower = hms::hms(0))
    expect_error(msf_sc(
        msf = 1, sd_w = lubridate::duration(1), sd_f = lubridate::duration(1),
        sd_week = lubridate::duration(1), alarm_f = TRUE
    ),
    "Assertion on 'msf' failed"
    )

    expect_error(msf_sc(
        msf = hms::hms(-1), sd_w = lubridate::duration(1),
        sd_f = lubridate::duration(1), sd_week = lubridate::duration(1),
        alarm_f = TRUE
    ),
    "Assertion on 'msf' failed"
    )

    # assert_duration(sd_w, lower = lubridate::duration(0))
    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = 1, sd_f = lubridate::duration(1),
        sd_week = lubridate::duration(1), alarm_f = TRUE
    ),
    "Assertion on 'sd_w' failed"
    )

    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(-1),
        sd_f = lubridate::duration(1),
        sd_week = lubridate::duration(1), alarm_f = TRUE
    ),
    "Assertion on 'sd_w' failed"
    )

    # assert_duration(sd_f, lower = lubridate::duration(0))
    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(1), sd_f = 1,
        sd_week = lubridate::duration(1), alarm_f = TRUE
    ),
    "Assertion on 'sd_f' failed"
    )

    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(1),
        sd_f = lubridate::duration(-1), sd_week = lubridate::duration(1),
        alarm_f = TRUE
    ),
    "Assertion on 'sd_f' failed"
    )

    # assert_duration(sd_week, lower = lubridate::duration(0))
    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(1),
        sd_f = lubridate::duration(1), sd_week = 1, alarm_f = TRUE
    ),
    "Assertion on 'sd_week' failed"
    )

    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(1),
        sd_f = lubridate::duration(1), sd_week = lubridate::duration(-1),
        alarm_f = TRUE
    ),
    "Assertion on 'sd_week' failed"
    )

    # checkmate::assert_logical(alarm_f)
    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(1),
        sd_f = lubridate::duration(1), sd_week = lubridate::duration(1),
        alarm_f = 1
    ),
    "Assertion on 'alarm_f' failed"
    )

    # assert_identical(msf, sd_w, sd_f, sd_week, alarm_f, type = "length")
    expect_error(msf_sc(
        msf = hms::hms(1), sd_w = lubridate::duration(1),
        sd_f = lubridate::duration(1), sd_week = lubridate::duration(1),
        alarm_f = c(TRUE, FALSE))
    )
})

test_that("msf_sc() | wrappers", {
    expect_equal(msf_sc, chronotype)
})
