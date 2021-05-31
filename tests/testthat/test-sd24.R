test_that("sd24() | scalar test", {
    expect_equal(sd24(lubridate::dhours(5.5), lubridate::dhours(2), TRUE),
                 lubridate::dhours(7.5))
    expect_equal(sd24(lubridate::dhours(7.5), lubridate::dhours(0.5), TRUE),
                 lubridate::dhours(8))
    expect_equal(sd24(lubridate::dhours(9), lubridate::as.duration(NA), FALSE),
                 lubridate::dhours(9))
    expect_equal(sd24(lubridate::as.duration(NA), lubridate::dhours(1.45),
                      TRUE),
                 lubridate::as.duration(NA))
})

test_that("sd24() | vector test", {
    expect_equal(sd24(c(lubridate::dhours(7.5), lubridate::dhours(3)),
                      c(lubridate::dhours(3.4), lubridate::dhours(2.5)),
                      c(FALSE, TRUE)),
                 c(lubridate::dhours(7.5), lubridate::dhours(5.5)))
})

test_that("sd24() | error test", {
    expect_error(sd24(1, lubridate::as.duration(1), TRUE),
                 "Assertion on 'sd' failed")
    expect_error(sd24(lubridate::as.duration(1), 1, TRUE),
                 "Assertion on 'napd' failed")
    expect_error(sd24(lubridate::as.duration(1), lubridate::as.duration(1),
                      ""),
                 "Assertion on 'nap' failed")

    expect_error(sd24(lubridate::as.duration(1), c(lubridate::as.duration(1),
                                                   lubridate::as.duration(1)),
                      TRUE),
                 "'sd', 'napd', and 'nap' must have identical lengths.")
})
