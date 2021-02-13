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
