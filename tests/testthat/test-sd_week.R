test_that("sd_week() | scalar test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(sd_week(
        sd_w = lubridate::dhours(6.5), sd_f = lubridate::dhours(9.5), wd = 5
    ),
    lubridate::as.duration(stats::weighted.mean(
        c(lubridate::dhours(6.5), lubridate::dhours(9.5)), c(5, 2))
    ))

    expect_equal(sd_week(
        sd_w = lubridate::dhours(6.5), sd_f = lubridate::dhours(5.5), wd = 4
    ),
    lubridate::as.duration(stats::weighted.mean(
        c(lubridate::dhours(6.5), lubridate::dhours(5.5)), c(4, 3))
    ))

    expect_equal(sd_week(
        sd_w = lubridate::as.duration(NA), sd_f = lubridate::dhours(7), wd = 5
    ),
    lubridate::as.duration(NA)
    )
})

test_that("sd_week() | vector test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(sd_week(
        sd_w = c(lubridate::dhours(4.5), lubridate::dhours(6.5)),
        sd_f = c(lubridate::dhours(11.5), lubridate::dhours(NA)),
        wd = c(6, 1)
    ),
    c(lubridate::duration(stats::weighted.mean(
        c(lubridate::dhours(4.5), lubridate::dhours(11.5)), c(6, 1))),
      lubridate::duration(lubridate::as.duration(NA))
    ))

    expect_equal(sd_week(
        sd_w = c(lubridate::dhours(6.5), lubridate::dhours(8)),
        sd_f = c(lubridate::dhours(9), lubridate::dhours(5.5)),
        wd = c(5, 4)
    ),
    c(lubridate::duration(stats::weighted.mean(
        c(lubridate::dhours(6.5), lubridate::dhours(9)), c(5, 2))),
      lubridate::duration(stats::weighted.mean(
          c(lubridate::dhours(8), lubridate::dhours(5.5)), c(4, 3)))
    ))
})

test_that("sd_week() | error test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # assert_duration(sd_w, lower = lubridate::duration(0))
    expect_error(sd_week(
        sd_w = 1, sd_f = lubridate::duration(1), wd = 1
    ),
    "Assertion on 'sd_w' failed"
    )

    expect_error(sd_week(
        sd_w = lubridate::duration(-1), sd_f = lubridate::duration(1), wd = 1
    ),
    "Assertion on 'sd_w' failed"
    )

    # assert_duration(sd_f, lower = lubridate::duration(0))
    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = 1, wd = 1
    ),
    "Assertion on 'sd_f' failed"
    )

    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(-1), wd = 1
    ),
    "Assertion on 'sd_f' failed"
    )

    # assert_numeric_(wd)
    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = "a"
    ),
    "Assertion on 'wd' failed"
    )

    # checkmate::assert_integerish(wd, lower = 0, upper = 7)
    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = 1.5
    ),
    "Assertion on 'wd' failed"
    )

    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = -1
    ),
    "Assertion on 'wd' failed"
    )

    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), wd = 8
    ),
    "Assertion on 'wd' failed"
    )

    # assert_identical(sd_w, sd_f, wd, type = "length")
    expect_error(sd_week(
        sd_w = lubridate::duration(1), sd_f = lubridate::duration(1),
        wd = c(1, 1))
    )
})
