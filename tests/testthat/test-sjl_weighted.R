test_that("sjl_weighted() | scalar test", {
    sjl <- list(sjl_m = lubridate::dhours(0.5),
                sjl_e = lubridate::dhours(4),
                sjl_n = lubridate::dhours(2))
    n_w <- list(n_w_m = 5, n_w_e = 1, n_w_n = 2)
    object <- sjl_weighted(sjl, n_w)
    x <- c(sjl[["sjl_m"]][1], sjl[["sjl_e"]][1], sjl[["sjl_n"]][1])
    w <- c(n_w[["n_w_m"]][1], n_w[["n_w_e"]][1], n_w[["n_w_n"]][1])
    expected <- lubridate::as.duration(stats::weighted.mean(x, w))
    expect_equal(object, expected)

    sjl <- list(sjl_m = lubridate::as.duration(NA),
                sjl_e = lubridate::dhours(1),
                sjl_n = lubridate::dhours(3))
    n_w <- list(n_w_m = 4, n_w_e = 3, n_w_n = 2)
    object <- sjl_weighted(sjl, n_w)
    x <- c(sjl[["sjl_m"]][1], sjl[["sjl_e"]][1], sjl[["sjl_n"]][1])
    w <- c(n_w[["n_w_m"]][1], n_w[["n_w_e"]][1], n_w[["n_w_n"]][1])
    expected <- lubridate::as.duration(stats::weighted.mean(x, w))
    expect_equal(object, expected)
})

test_that("sjl_weighted() | vector test", {
    sjl <- list(sjl_m = c(lubridate::dhours(8), lubridate::dhours(1.78)),
                sjl_e = c(lubridate::dhours(5.5), lubridate::as.duration(NA)),
                sjl_n = c(lubridate::dhours(3.2), lubridate::dhours(5.45)))
    n_w <- list(n_w_m = c(2, 4), n_w_e = c(1, 2), n_w_n = c(7, 2))
    object <- sjl_weighted(sjl, n_w)
    i <- 1
    x <- c(sjl[["sjl_m"]][i], sjl[["sjl_e"]][i], sjl[["sjl_n"]][i])
    w <- c(n_w[["n_w_m"]][i], n_w[["n_w_e"]][i], n_w[["n_w_n"]][i])
    expected_1 <- lubridate::as.duration(stats::weighted.mean(x, w))
    i <- 2
    x <- c(sjl[["sjl_m"]][i], sjl[["sjl_e"]][i], sjl[["sjl_n"]][i])
    w <- c(n_w[["n_w_m"]][i], n_w[["n_w_e"]][i], n_w[["n_w_n"]][i])
    expected_2 <- lubridate::as.duration(stats::weighted.mean(x, w))
    expected <- c(expected_1, expected_2)
    expect_equal(object, expected)
})

test_that("sjl_weighted() | error test", {
    # Invalid values for `sjl`, and `n_w`
    expect_error(sjl_weighted(1, list(n_w_m = 1)))
    expect_error(sjl_weighted(list(sjl_m = lubridate::duration(1)), 1.5))
    expect_error(sjl_weighted(list(sjl_m = lubridate::duration(1)),
                              list(n_w_m = "")))
    expect_error(sjl_weighted(list(sjl_m = lubridate::duration(1)),
                              list(n_w_m = 1.5)))

    # `sjl` and `n_w` have different element lengths
    expect_error(sjl_weighted(list(sjl_m = lubridate::duration(1)),
                              list(n_w_m = 1, n_w_e = 1)))

    # `sjl` and `n_w` have different object lengths inside elements
    expect_error(sjl_weighted(list(sjl_m = lubridate::duration(1)),
                              list(n_w_m = c(1, 1))))
})
