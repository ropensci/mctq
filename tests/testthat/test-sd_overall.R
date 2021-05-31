test_that("sd_overall() | scalar test", {
    expect_equal(sd_overall(lubridate::dhours(6.5), lubridate::dhours(11), 4,
                            2),
                 lubridate::as.duration(
                     stats::weighted.mean(c(lubridate::dhours(6.5),
                                            lubridate::dhours(11)),
                                          c(4, 2))))
    expect_equal(sd_overall(lubridate::dhours(8.5), lubridate::dhours(6.5), 6,
                            1),
                 lubridate::as.duration(
                     stats::weighted.mean(c(lubridate::dhours(8.5),
                                            lubridate::dhours(6.5)),
                                          c(6, 1))))
    expect_equal(sd_overall(lubridate::as.duration(NA), lubridate::dhours(7), 2,
                            2),
                 lubridate::as.duration(NA))
})

test_that("sd_overall() | vector test", {
    expect_equal(sd_overall(c(lubridate::dhours(3.9), lubridate::dhours(5)),
                            c(lubridate::dhours(12), lubridate::dhours(NA)),
                            c(6, 10),
                            c(1, 7)),
                 c(lubridate::duration(
                     stats::weighted.mean(c(lubridate::dhours(3.9),
                                            lubridate::dhours(12)),
                                          c(6, 1))),
                   lubridate::as.duration(NA)))
    expect_equal(sd_overall(c(lubridate::dhours(4.5), lubridate::dhours(7)),
                            c(lubridate::dhours(10.5), lubridate::dhours(3)),
                            c(4, 5),
                            c(1, 2)),
                 c(lubridate::duration(
                     stats::weighted.mean(c(lubridate::dhours(4.5),
                                            lubridate::dhours(10.5)),
                                          c(4, 1))),
                   lubridate::duration(
                       stats::weighted.mean(c(lubridate::dhours(7),
                                              lubridate::dhours(3)),
                                            c(5, 2)))))
})

test_that("sd_overall() | error test", {
    expect_error(sd_overall(1, lubridate::duration(1), 1, 1),
                 "Assertion on 'sd_w' failed")
    expect_error(sd_overall(lubridate::duration(1), 1, 1, 1),
                 "Assertion on 'sd_f' failed")
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            "a", 1),
                 "Assertion on 'n_w' failed")
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1.5, 1),
                 "Assertion on 'n_w' failed")
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            -1, 1),
                 "Assertion on 'n_w' failed")
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1, "a"),
                 "Assertion on 'n_f' failed")
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1, 1.5),
                 "Assertion on 'n_f' failed")
    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            1, -1),
                 "Assertion on 'n_f' failed")

    expect_error(sd_overall(lubridate::duration(1), lubridate::duration(1),
                            c(1, 1), 1),
                 "'sd_w', 'sd_f', 'n_w', and 'n_f' must have identical ")
})
