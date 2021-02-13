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
