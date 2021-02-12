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

test_that("sd24() | scalar test", {
    sd <- lubridate::dhours(5.5)
    napd <- lubridate::dhours(2)
    nap <- TRUE
    object <- sd24(sd, napd, nap)
    expected <- lubridate::dhours(7.5)
    expect_equal(object, expected)

    sd <- lubridate::dhours(7.5)
    napd <- lubridate::dhours(0.5)
    nap <- TRUE
    object <- sd24(sd, napd, nap)
    expected <- lubridate::dhours(8)
    expect_equal(object, expected)

    sd <- lubridate::dhours(9)
    napd <- lubridate::as.duration(NA)
    nap <- FALSE
    object <- sd24(sd, napd, nap)
    expected <- lubridate::dhours(9)
    expect_equal(object, expected)

    sd <- lubridate::as.duration(NA)
    napd <- lubridate::dhours(1.45)
    nap <- TRUE
    object <- sd24(sd, napd, nap)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("sd24() | vector test", {
    sd <- c(lubridate::dhours(7.5), lubridate::dhours(3))
    napd <- c(lubridate::dhours(3.4), lubridate::dhours(2.5))
    nap <- c(FALSE, TRUE)
    object <- sd24(sd, napd, nap)
    expected <- c(lubridate::dhours(7.5), lubridate::dhours(5.5))
    expect_equal(object, expected)
})

test_that("sd24() | error test", {
    # Invalid values for `sd` and `napd`
    expect_error(sd24(1, lubridate::as.duration(1), TRUE))
    expect_error(sd24(lubridate::as.duration(1), 1, TRUE))
    expect_error(sd24(lubridate::as.duration(1), lubridate::as.duration(1), ""))

    # `sd` and `napd` have different lengths
    expect_error(sd24(lubridate::as.duration(1), c(lubridate::as.duration(1),
                                                   lubridate::as.duration(1)),
                      TRUE))
})

test_that("sd_week() | scalar test", {
    sd_w <- lubridate::dhours(6.5)
    sd_f <- lubridate::dhours(9.5)
    wd <- 5
    object <- sd_week(sd_w, sd_f, wd)
    expected <- lubridate::as.duration(
        stats::weighted.mean(c(sd_w, sd_f),c(wd, fd(wd))))
    expect_equal(object, expected)

    sd_w <- lubridate::dhours(6.5)
    sd_f <- lubridate::dhours(5.5)
    wd <- 4
    object <- sd_week(sd_w, sd_f, wd)
    expected <- lubridate::as.duration(
        stats::weighted.mean(c(sd_w, sd_f),c(wd, fd(wd))))
    expect_equal(object, expected)

    sd_w <- lubridate::as.duration(NA)
    sd_f <- lubridate::dhours(7)
    wd <- 5
    object <- sd_week(sd_w, sd_f, wd)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("sd_week() | vector test", {
    sd_w <- c(lubridate::dhours(4.5), lubridate::dhours(6.5))
    sd_f <- c(lubridate::dhours(11.5), lubridate::dhours(NA))
    wd <- c(6, 1)
    object <- sd_week(sd_w, sd_f, wd)
    expected_1 <- stats::weighted.mean(c(sd_w[1], sd_f[1]), c(wd[1], fd(wd[1])))
    expected_2 <- lubridate::as.duration(NA)
    expected <- c(lubridate::duration(expected_1),
                  lubridate::duration(expected_2))
    expect_equal(object, expected)

    sd_w <- c(lubridate::dhours(6.5), lubridate::dhours(8))
    sd_f <- c(lubridate::dhours(9), lubridate::dhours(5.5))
    wd <- c(5, 4)
    object <- sd_week(sd_w, sd_f, wd)
    expected_1 <- stats::weighted.mean(c(sd_w[1], sd_f[1]), c(wd[1], fd(wd[1])))
    expected_2 <- stats::weighted.mean(c(sd_w[2], sd_f[2]), c(wd[2], fd(wd[2])))
    expected <- c(lubridate::duration(expected_1),
                  lubridate::duration(expected_2))
    expect_equal(object, expected)
})

test_that("sd_week() | error test", {
    # Invalid values for `sd_w`, `sd_f`, and `wd`
    expect_error(sd_week(1, lubridate::duration(1), 1))
    expect_error(sd_week(lubridate::duration(1), 1, 1))
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), "a"))
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), 1.5))
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), -1))
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), 8))

    # `sd_w`, `sd_f`, and `wd` have different lengths
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1),
                         c(1, 1)))
})

test_that("sd_overall() | scalar test", {
    sd_w <- lubridate::dhours(6.5)
    sd_f <- lubridate::dhours(11)
    n_w <- 4
    n_f <- 2
    object <- sd_overall(sd_w, sd_f, n_w, n_f)
    expected <- lubridate::as.duration(
        stats::weighted.mean(c(sd_w, sd_f),c(n_w, n_f)))
    expect_equal(object, expected)

    sd_w <- lubridate::dhours(8.5)
    sd_f <- lubridate::dhours(6.5)
    n_w <- 6
    n_f <- 1
    object <- sd_overall(sd_w, sd_f, n_w, n_f)
    expected <- lubridate::as.duration(
        stats::weighted.mean(c(sd_w, sd_f),c(n_w, fd(n_w))))
    expect_equal(object, expected)

    sd_w <- lubridate::as.duration(NA)
    sd_f <- lubridate::dhours(7)
    n_w <- 2
    n_f <- 2
    object <- sd_overall(sd_w, sd_f, n_w, n_f)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("sd_overall() | vector test", {
    sd_w <- c(lubridate::dhours(3.9), lubridate::dhours(5))
    sd_f <- c(lubridate::dhours(12), lubridate::dhours(NA))
    n_w <- c(6, 10)
    n_f <- c(1, 7)
    object <- sd_overall(sd_w, sd_f, n_w, n_f)
    expected_1 <- stats::weighted.mean(c(sd_w[1], sd_f[1]), c(n_w[1], n_f[1]))
    expected_2 <- lubridate::as.duration(NA)
    expected <- c(lubridate::duration(expected_1),
                  lubridate::duration(expected_2))
    expect_equal(object, expected)

    sd_w <- c(lubridate::dhours(4.5), lubridate::dhours(7))
    sd_f <- c(lubridate::dhours(10.5), lubridate::dhours(3))
    n_w <- c(4, 5)
    n_f <- c(1, 2)
    object <- sd_overall(sd_w, sd_f, n_w, n_f)
    expected_1 <- stats::weighted.mean(c(sd_w[1], sd_f[1]), c(n_w[1], n_f[1]))
    expected_2 <- stats::weighted.mean(c(sd_w[2], sd_f[2]), c(n_w[2], n_f[2]))
    expected <- c(lubridate::duration(expected_1),
                  lubridate::duration(expected_2))
    expect_equal(object, expected)
})

test_that("sd_overall() | error test", {
    # Invalid values for `sd_w`, `sd_f`, `n_w`, and `n_f`
    expect_error(sd_overall(1, lubridate::duration(1), 1, 1))
    expect_error(sd_overall(lubridate::duration(1), 1, 1, 1))
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            "a", 1))
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1.5, 1))
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            -1, 1))
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1, "a"))
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1, 1.5))
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1, -1))

    # `sd_w`, `sd_f`, `n_w`, and `n_f` have different lengths
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                         c(1, 1), 1))
})
