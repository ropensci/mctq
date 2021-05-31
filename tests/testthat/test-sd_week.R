test_that("sd_week() | scalar test", {
    expect_equal(sd_week(lubridate::dhours(6.5), lubridate::dhours(9.5), 5),
                 lubridate::as.duration(
                     stats::weighted.mean(c(lubridate::dhours(6.5),
                                            lubridate::dhours(9.5)),
                                          c(5, 2))))
    expect_equal(sd_week(lubridate::dhours(6.5), lubridate::dhours(5.5), 4),
                 lubridate::as.duration(
                     stats::weighted.mean(c(lubridate::dhours(6.5),
                                            lubridate::dhours(5.5)),
                                          c(4, 3))))
    expect_equal(sd_week(lubridate::as.duration(NA), lubridate::dhours(7), 5),
                 lubridate::as.duration(NA))
})

test_that("sd_week() | vector test", {
    expect_equal(sd_week(c(lubridate::dhours(4.5), lubridate::dhours(6.5)),
                         c(lubridate::dhours(11.5), lubridate::dhours(NA)),
                         c(6, 1)),
                 c(lubridate::duration(
                     stats::weighted.mean(c(lubridate::dhours(4.5),
                                            lubridate::dhours(11.5)),
                                          c(6, 1))),
                   lubridate::duration(lubridate::as.duration(NA))))
    expect_equal(sd_week(c(lubridate::dhours(6.5), lubridate::dhours(8)),
                         c(lubridate::dhours(9), lubridate::dhours(5.5)),
                         c(5, 4)),
                 c(lubridate::duration(
                     stats::weighted.mean(c(lubridate::dhours(6.5),
                                            lubridate::dhours(9)),
                                          c(5, 2))),
                   lubridate::duration(
                       stats::weighted.mean(c(lubridate::dhours(8),
                                              lubridate::dhours(5.5)),
                                            c(4, 3)))))
})

test_that("sd_week() | error test", {
    expect_error(sd_week(1, lubridate::duration(1), 1),
                 "Assertion on 'sd_w' failed")
    expect_error(sd_week(lubridate::duration(1), 1, 1),
                 "Assertion on 'sd_f' failed")
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), "a"),
                 "Assertion on 'wd' failed")
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), 1.5),
                 "Assertion on 'wd' failed")
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), -1),
                 "Assertion on 'wd' failed")
    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1), 8),
                 "Assertion on 'wd' failed")

    expect_error(sd_week(lubridate::duration(1), lubridate::duration(1),
                         c(1, 1)),
                 "'sd_w', 'sd_f', and 'wd' must have identical lengths.")
})
