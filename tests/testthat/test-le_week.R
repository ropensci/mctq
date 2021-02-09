test_that("le_week() | scalar test", {
    le_w <- lubridate::dhours(2.3)
    le_f <- lubridate::dhours(4.5)
    wd <- 5
    object <- le_week(le_w, le_f, wd)
    expected <- lubridate::as.duration(
        stats::weighted.mean(c(le_w, le_f),c(wd, fd(wd))))
    expect_equal(object, expected)

    le_w <- lubridate::dhours(5.25)
    le_f <- lubridate::dhours(1.25)
    wd <- 3
    object <- le_week(le_w, le_f, wd)
    expected <- lubridate::as.duration(
        stats::weighted.mean(c(le_w, le_f),c(wd, fd(wd))))
    expect_equal(object, expected)

    le_w <- lubridate::as.duration(NA)
    le_f <- lubridate::dhours(2.35)
    wd <- 2
    object <- le_week(le_w, le_f, wd)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("le_week() | vector test", {
    le_w <- c(lubridate::dhours(2.4), lubridate::dhours(0.5))
    le_f <- c(lubridate::dhours(2.4), lubridate::dhours(NA))
    wd <- c(3, 7)
    object <- le_week(le_w, le_f, wd)
    expected_1 <- stats::weighted.mean(c(le_w[1], le_f[1]), c(wd[1], fd(wd[1])))
    expected_2 <- lubridate::as.duration(NA)
    expected <- c(lubridate::duration(expected_1),
                  lubridate::duration(expected_2))
    expect_equal(object, expected)

    le_w <- c(lubridate::dhours(1.8), lubridate::dhours(5.4))
    le_f <- c(lubridate::dhours(6.7), lubridate::dhours(1.2))
    wd <- c(5, 6)
    object <- le_week(le_w, le_f, wd)
    expected_1 <- stats::weighted.mean(c(le_w[1], le_f[1]), c(wd[1], fd(wd[1])))
    expected_2 <- stats::weighted.mean(c(le_w[2], le_f[2]), c(wd[2], fd(wd[2])))
    expected <- c(lubridate::duration(expected_1),
                  lubridate::duration(expected_2))
    expect_equal(object, expected)
})

test_that("le_week() | error test", {
    # Invalid values for `le_w`, `le_f`, and `wd`
    expect_error(le_week(1, lubridate::dhours(), 1))
    expect_error(le_week(lubridate::dhours(), 1, 1))
    expect_error(le_week(lubridate::dhours(), lubridate::dhours(), "a"))
    expect_error(le_week(lubridate::dhours(), lubridate::dhours(), 1.5))
    expect_error(le_week(lubridate::dhours(), lubridate::dhours(), -1))
    expect_error(le_week(lubridate::dhours(), lubridate::dhours(), 8))

    # `le_w`, `le_f`, and `wd` have different lengths
    expect_error(le_week(lubridate::dhours(), lubridate::dhours(), c(1, 1)))
})
