test_that("sloss_week() | scalar test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(sloss_week(
        sd_w = lubridate::dhours(5.5), sd_f = lubridate::dhours(9), wd = 5
    ),
    lubridate::duration(18000)
    )

    expect_equal(sloss_week(
        sd_w = lubridate::dhours(8), sd_f = lubridate::dhours(6.5), wd = 4
    ),
    lubridate::duration(9257.14285714286)
    )

    expect_equal(sloss_week(
        sd_w = lubridate::as.duration(7), sd_f = lubridate::dhours(7),
        wd = as.numeric(NA)
    ),
    lubridate::as.duration(NA)
    )
})

test_that("sloss_week() | vector test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(sloss_week(
        sd_w = c(lubridate::dhours(9), lubridate::dhours(10.5)),
        sd_f = c(lubridate::dhours(12), lubridate::dhours(8.5)),
        wd = c(4, 5)
    ),
    c(lubridate::duration(18514.2857142857),
      lubridate::duration(10285.7142857143)
    ))

    expect_equal(sloss_week(
        sd_w = c(lubridate::dhours(NA), lubridate::dhours(8.7)),
        sd_f = c(lubridate::dhours(7.5), lubridate::dhours(9.2)),
        wd = c(5, 6)
    ),
    c(lubridate::as.duration(NA),
      lubridate::duration(1542.85714285713)
    ))
})

test_that("sloss_week() | error test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # assert_duration(sd_w, lower = lubridate::duration(0))
    expect_error(sloss_week(
        sd_w = 1, sd_f = lubridate::duration(1), wd = 1
    ),
    "Assertion on 'sd_w' failed"
    )

    expect_error(sloss_week(
        sd_w = lubridate::duration(-1), sd_f = lubridate::duration(1), wd = 1
    ),
    "Assertion on 'sd_w' failed"
    )

    # assert_duration(sd_f, lower = lubridate::duration(0))
    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = 1, wd = 1
    ),
    "Assertion on 'sd_f' failed"
    )

    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(-1), wd = 1
    ),
    "Assertion on 'sd_f' failed"
    )

    # assert_numeric_(wd)
    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = "a"
    ),
    "Assertion on 'wd' failed"
    )

    # checkmate::assert_integerish(wd, lower = 0, upper = 7)
    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = 1.5
    ),
    "Assertion on 'wd' failed"
    )

    #
    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = -1
    ),
    "Assertion on 'wd' failed"
    )

    #
    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = 8
    ),
    "Assertion on 'wd' failed"
    )

    # assert_identical(sd_w, sd_f, wd, type = "length")
    expect_error(sloss_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1),
        wd = c(1, 1)
    ))
})
