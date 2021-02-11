test_that("sloss_week() | scalar test", {
    sd_w <- lubridate::dhours(5.5)
    sd_f <- lubridate::dhours(9)
    wd <- 5
    object <- sloss_week(sd_w, sd_f, wd)
    expected <- lubridate::duration(18000)
    expect_equal(object, expected)

    sd_w <- lubridate::dhours(8)
    sd_f <- lubridate::dhours(6.5)
    wd <- 4
    object <- sloss_week(sd_w, sd_f, wd)
    expected <- lubridate::duration(9257.14285714286)
    expect_equal(object, expected)

    sd_w <- lubridate::as.duration(7)
    sd_f <- lubridate::dhours(7)
    wd <- as.numeric(NA)
    object <- sloss_week(sd_w, sd_f, wd)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("sloss_week() | vector test", {
    sd_w <- c(lubridate::dhours(9), lubridate::dhours(10.5))
    sd_f <- c(lubridate::dhours(12), lubridate::dhours(8.5))
    wd <- c(4, 5)
    object <- sloss_week(sd_w, sd_f, wd)
    expected <- c(lubridate::duration(18514.2857142857),
                  lubridate::duration(10285.7142857143))
    expect_equal(object, expected)

    sd_w <- c(lubridate::dhours(NA), lubridate::dhours(8.7))
    sd_f <- c(lubridate::dhours(7.5), lubridate::dhours(9.2))
    wd <- c(5, 6)
    object <- sloss_week(sd_w, sd_f, wd)
    expected <- c(lubridate::as.duration(NA),
                  lubridate::duration(1542.85714285713))
    expect_equal(object, expected)
})

test_that("sloss_week() | error test", {
    # Invalid values for `sd_w`, `sd_f`, and `wd`
    expect_error(sloss_week(1, lubridate::duration(1), 1))
    expect_error(sloss_week(lubridate::duration(1), 1, 1))
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            "a"))
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            1.5))
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            -1))
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1), 8))

    # `sd_w`, `sd_f`, and `wd` have different lengths
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            c(1, 1)))
})
