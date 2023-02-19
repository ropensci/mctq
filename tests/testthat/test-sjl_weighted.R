test_that("sjl_weighted() | scalar test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(sjl_weighted(
        sjl = list(
            sjl_m = lubridate::dhours(0.5),
            sjl_e = lubridate::dhours(4),
            sjl_n = lubridate::dhours(2)
        ),
        n_w = list(n_w_m = 5, n_w_e = 1, n_w_n = 2)
    ),
    lubridate::as.duration(stats::weighted.mean(
        c(lubridate::dhours(0.5), lubridate::dhours(4), lubridate::dhours(2)),
        c(5, 1, 2)
    ))
    )

    expect_equal(sjl_weighted(
        sjl = list(
            sjl_m = lubridate::as.duration(NA),
            sjl_e = lubridate::dhours(1),
            sjl_n = lubridate::dhours(3)
        ),
        n_w = list(n_w_m = 4, n_w_e = 3, n_w_n = 2)
    ),
    lubridate::as.duration(stats::weighted.mean(
        c(lubridate::as.duration(NA), lubridate::dhours(1),
          lubridate::dhours(3)),
        c(4, 3, 2)
    ))
    )
})

test_that("sjl_weighted() | vector test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(sjl_weighted(
        sjl = list(
            sjl_m = c(lubridate::dhours(8), lubridate::dhours(1.78)),
            sjl_e = c(lubridate::dhours(5.5), lubridate::as.duration(NA)),
            sjl_n = c(lubridate::dhours(3.2), lubridate::dhours(5.45))),
        n_w = list(n_w_m = c(2, 4), n_w_e = c(1, 2), n_w_n = c(7, 2))
    ),
    c(lubridate::as.duration(stats::weighted.mean(
        c(lubridate::dhours(8), lubridate::dhours(5.5), lubridate::dhours(3.2)),
        c(2, 1, 7)
        )),
      lubridate::as.duration(NA)
    )
    )
})

test_that("sjl_weighted() | error test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # checkmate::assert_list(sjl, len = length(n_w))
    expect_error(sjl_weighted(
        sjl = 1, n_w = list(n_w_m = 1)
    ),
    "Assertion on 'sjl' failed"
    )

    # checkmate::assert_list(n_w, len = length(sjl))
    expect_error(sjl_weighted(
        sjl = list(sjl_m = lubridate::duration(1)), n_w = 1.5
    ),
    "Assertion on 'n_w' failed"
    )

    # lapply(sjl, assert_duration)
    expect_error(sjl_weighted(
        sjl = list(sjl_m = lubridate::duration(1)), n_w = list(n_w_m = "")
    ),
    "Assertion on 'X\\[\\[i\\]\\]' failed"
    )

    # lapply(n_w, checkmate::assert_integerish, lower = 0)
    expect_error(sjl_weighted(
        sjl = list(sjl_m = lubridate::duration(1)), n_w = list(n_w_m = 1.5)
    ),
    "Assertion on 'X\\[\\[i\\]\\]' failed"
    )

    # mapply(assert_identical, sjl, n_w, MoreArgs = list(type = "length"))
    # 'sjl' and 'n_w' have different element lengths
    expect_error(sjl_weighted(
        sjl = list(sjl_m = lubridate::duration(1)),
        n_w = list(n_w_m = 1, n_w_e = 1)
    ),
    "Assertion on 'sjl' failed"
    )

    # mapply(assert_identical, sjl, n_w, MoreArgs = list(type = "length"))
    # 'sjl' and 'n_w' have different object lengths inside elements
    expect_error(sjl_weighted(
        sjl = list(sjl_m = lubridate::duration(1)), n_w = list(n_w_m = c(1, 1))
    ))
})
