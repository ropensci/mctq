test_that("sloss_week() | scalar test", {
    expect_equal(sloss_week(lubridate::dhours(5.5), lubridate::dhours(9), 5),
                 lubridate::duration(18000))
    expect_equal(sloss_week(lubridate::dhours(8), lubridate::dhours(6.5), 4),
                 lubridate::duration(9257.14285714286))
    expect_equal(sloss_week(lubridate::as.duration(7), lubridate::dhours(7),
                            as.numeric(NA)),
                 lubridate::as.duration(NA))
})

test_that("sloss_week() | vector test", {
    expect_equal(sloss_week(c(lubridate::dhours(9), lubridate::dhours(10.5)),
                            c(lubridate::dhours(12), lubridate::dhours(8.5)),
                            c(4, 5)),
                 c(lubridate::duration(18514.2857142857),
                   lubridate::duration(10285.7142857143)))
    expect_equal(sloss_week(c(lubridate::dhours(NA), lubridate::dhours(8.7)),
                            c(lubridate::dhours(7.5), lubridate::dhours(9.2)),
                            c(5, 6)),
                 c(lubridate::as.duration(NA),
                   lubridate::duration(1542.85714285713)))
})

test_that("sloss_week() | error test", {
    expect_error(sloss_week(1, lubridate::duration(1), 1),
                 "Assertion on 'sd_w' failed")
    expect_error(sloss_week(lubridate::duration(1), 1, 1),
                 "Assertion on 'sd_f' failed")
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            "a"),
                 "Assertion on 'wd' failed")
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            1.5),
                 "Assertion on 'wd' failed")
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            -1),
                 "Assertion on 'wd' failed")
    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1), 8),
                 "Assertion on 'wd' failed")

    expect_error(sloss_week(lubridate::duration(1), lubridate::duration(1),
                            c(1, 1)),
                 "'sd_w', 'sd_f', and 'wd' must have identical lengths.")
})
