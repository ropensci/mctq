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
