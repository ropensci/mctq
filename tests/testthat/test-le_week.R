test_that("le_week() | scalar test", {
    expect_equal(le_week(lubridate::dhours(2.3), lubridate::dhours(4.5), 5),
                 lubridate::as.duration(
                     stats::weighted.mean(c(lubridate::dhours(2.3),
                                            lubridate::dhours(4.5)),
                                          c(5, 2))))
    expect_equal(le_week(lubridate::dhours(5.25), lubridate::dhours(1.25), 3),
                 lubridate::as.duration(
                     stats::weighted.mean(c(lubridate::dhours(5.25),
                                            lubridate::dhours(1.25)),
                                          c(3, 4))))
    expect_equal(le_week(lubridate::as.duration(NA),
                         lubridate::dhours(2.35), 2),
                 lubridate::as.duration(NA))
})

test_that("le_week() | vector test", {
    expect_equal(le_week(c(lubridate::dhours(2.4), lubridate::dhours(0.5)),
                         c(lubridate::dhours(2.4), lubridate::dhours(NA)),
                         c(3, 7)),
                 c(lubridate::duration(
                     stats::weighted.mean(c(lubridate::dhours(2.4),
                                            lubridate::dhours(2.4)),
                                          c(3, 4))),
                   lubridate::as.duration(NA)))
    expect_equal(le_week(c(lubridate::dhours(1.8), lubridate::dhours(5.4)),
                         c(lubridate::dhours(6.7), lubridate::dhours(1.2)),
                         c(5, 6)),
                 c(lubridate::duration(
                     stats::weighted.mean(c(lubridate::dhours(1.8),
                                            lubridate::dhours(6.7)),
                                          c(5, 2))),
                     lubridate::duration(
                         stats::weighted.mean(c(lubridate::dhours(5.4),
                                                lubridate::dhours(1.2)),
                                              c(6, 1)))))
})

test_that("le_week() | error test", {
    expect_error(le_week(1, lubridate::duration(1), 1),
                 "Assertion on 'le_w' failed")
    expect_error(le_week(lubridate::duration(1), 1, 1),
                 "Assertion on 'le_f' failed")
    expect_error(le_week(lubridate::duration(1), lubridate::duration(1), "a"),
                 "Assertion on 'wd' failed")
    expect_error(le_week(lubridate::duration(1), lubridate::duration(1), 1.5),
                 "Assertion on 'wd' failed")
    expect_error(le_week(lubridate::duration(1), lubridate::duration(1), -1),
                 "Assertion on 'wd' failed")
    expect_error(le_week(lubridate::duration(1), lubridate::duration(1), 8),
                 "Assertion on 'wd' failed")

    expect_error(le_week(lubridate::duration(1), lubridate::duration(1),
                         c(1, 1)))
})
