test_that("msf_sc() | scalar test", {
    msf <- hms::parse_hm("04:00")
    sd_w <- lubridate::dhours(6)
    sd_f <- lubridate::dhours(8)
    sd_week <- sd_week(sd_w, sd_f, 5)
    alarm_f <- FALSE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- hms::parse_hms("03:17:08.571429")
    expect_equal(object, expected)

    msf <- hms::parse_hm("23:30")
    sd_w <- lubridate::dhours(7)
    sd_f <- lubridate::dhours(9)
    sd_week <- sd_week(sd_w, sd_f, 3)
    alarm_f <- FALSE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- hms::parse_hms("23:04:17.142857")
    expect_equal(object, expected)

    msf <- hms::parse_hm("02:15")
    sd_w <- lubridate::dhours(9)
    sd_f <- lubridate::dhours(7)
    sd_week <- sd_week(sd_w, sd_f, 5)
    alarm_f <- FALSE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- msf
    expect_equal(object, expected)

    msf <- hms::parse_hm("00:00")
    sd_w <- lubridate::dhours(6.15)
    sd_f <- lubridate::dhours(8.25)
    sd_week <- lubridate::as.duration(NA)
    alarm_f <- FALSE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- hms::as_hms(NA)
    expect_equal(object, expected)

    msf <- hms::parse_hm("01:00")
    sd_w <- lubridate::dhours(8)
    sd_f <- lubridate::dhours(5.5)
    sd_week <- lubridate::as.duration(NA)
    alarm_f <- FALSE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- msf
    expect_equal(object, expected)

    msf <- hms::parse_hm("02:25")
    sd_w <- lubridate::as.duration(NA)
    sd_f <- lubridate::dhours(9.5)
    sd_week <- sd_week(sd_w, sd_f, 5)
    alarm_f <- FALSE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- hms::as_hms(NA)
    expect_equal(object, expected)

    msf <- hms::parse_hm("21:15")
    sd_w <- lubridate::dhours(8)
    sd_f <- lubridate::dhours(8)
    sd_week <- sd_week(sd_w, sd_f, 6)
    alarm_f <- TRUE
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- hms::as_hms(NA)
    expect_equal(object, expected)
})

test_that("msf_sc() | vector test", {
    msf <- c(hms::parse_hm("03:45"), hms::parse_hm("22:30"))
    sd_w <- c(lubridate::dhours(6), lubridate::dhours(5.5))
    sd_f <- c(lubridate::dhours(7.5), lubridate::dhours(8))
    sd_week <- c(sd_week(sd_w[1], sd_f[1], 5), sd_week(sd_w[2], sd_f[2], 4))
    alarm_f <- c(FALSE, FALSE)
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- c(hms::parse_hms("03:12:51.428571"),
                  hms::parse_hms("21:47:08.571429"))
    expect_equal(object, expected)

    msf <- c(hms::parse_hm("03:45"), hms::parse_hm("21:30"))
    sd_w <- c(lubridate::dhours(7), lubridate::dhours(6.5))
    sd_f <- c(lubridate::dhours(7), lubridate::dhours(8))
    sd_week <- c(sd_week(sd_w[1], sd_f[1], 5), sd_week(sd_w[2], sd_f[2], 5))
    alarm_f <- c(FALSE, TRUE)
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- c(hms::parse_hm("03:45"), hms::as_hms(NA))
    expect_equal(object, expected)

    msf <- c(hms::parse_hm("01:20"), hms::as_hms(NA))
    sd_w <- c(lubridate::dhours(5), lubridate::dhours(7))
    sd_f <- c(lubridate::dhours(8.5), lubridate::dhours(9))
    sd_week <- c(lubridate::as.duration(NA), sd_week(sd_w[2], sd_f[2], 3))
    alarm_f <- c(FALSE, FALSE)
    object <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- c(hms::as_hms(NA), hms::as_hms(NA))
    expect_equal(object, expected)
})

test_that("msf_sc() | error test", {
    # Invalid values for `msf`, `sd_w`, `sd_f`, `sd_week`, and `alarm_f`
    expect_error(msf_sc(1, lubridate::duration(1), lubridate::duration(1),
                        lubridate::duration(1), TRUE))
    expect_error(msf_sc(hms::hms(1), 1, lubridate::duration(1),
                        lubridate::duration(1), TRUE))
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1), 1,
                        lubridate::duration(1), TRUE))
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1),
                        lubridate::duration(1), 1, TRUE))
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1),
                        lubridate::duration(1), lubridate::duration(1), 1))

    # `msf`, `sd_w`, `sd_f`, `sd_week`, and `alarm_f` have different lengths
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1),
                        lubridate::duration(1), lubridate::duration(1),
                        c(TRUE, FALSE)))
})

test_that("msf_sc() | wrappers", {
    msf <- hms::parse_hm("03:25")
    sd_w <- lubridate::dhours(7)
    sd_f <- lubridate::dhours(8)
    sd_week <- sd_week(sd_w, sd_f, 2)
    alarm_f <- FALSE
    object <- chronotype(msf, sd_w, sd_f, sd_week, alarm_f)
    expected <- hms::parse_hms("03:16:25.714286")
    expect_equal(object, expected)
})
