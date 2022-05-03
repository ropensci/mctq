test_that("sjl_sc() | scalar test", {
    expect_equal(sjl_sc(
        so_w = hms::parse_hm("00:00"), se_w = hms::parse_hm("08:00"),
        so_f = hms::parse_hm("01:00"), se_f = hms::parse_hm("09:00"),
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(1)
    )

    expect_equal(sjl_sc(
        so_w = hms::parse_hm("01:00"), se_w = hms::parse_hm("07:00"),
        so_f = hms::parse_hm("01:00"), se_f = hms::parse_hm("09:00"),
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(0)
    )

    expect_equal(sjl_sc(
        so_w = hms::parse_hm("00:30"), se_w = hms::parse_hm("07:30"),
        so_f = hms::parse_hm("01:00"), se_f = hms::parse_hm("09:00"),
        abs = FALSE, method = "shorter"
    ),
    lubridate::dminutes(30)
    )

    # Negative sjl_sc()
    expect_equal(sjl_sc(
        so_w = hms::parse_hm("02:00"), se_w = hms::parse_hm("10:00"),
        so_f = hms::parse_hm("00:00"), se_f = hms::parse_hm("08:00"),
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(-2)
    )

    # sjl_sc = | se_f - se_w |
    expect_equal(sjl_sc(
        so_w = hms::parse_hm("22:00"), se_w = hms::parse_hm("06:00"),
        so_f = hms::parse_hm("01:00"), se_f = hms::parse_hm("06:00"),
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(0)
    )

    expect_equal(sjl_sc(
        so_w = hms::as_hms(NA), se_w = hms::parse_hm("08:00"),
        so_f = hms::parse_hm("00:00"), se_f = hms::parse_hm("08:00"),
        abs = FALSE, method = "shorter"
    ),
    lubridate::as.duration(NA)
    )

    expect_equal(sjl_sc(
        so_w = hms::parse_hm("00:00"), se_w = hms::parse_hm("08:00"),
        so_f = hms::parse_hm("00:00"), se_f = hms::as_hms(NA),
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(0)
    )
})

test_that("sjl_sc() | vector test", {
    expect_equal(sjl_sc(
        so_w = c(hms::parse_hm("00:00"), hms::parse_hm("01:00")),
        se_w = c(hms::parse_hm("08:00"), hms::parse_hm("07:00")),
        so_f = c(hms::parse_hm("01:00"), hms::parse_hm("01:00")),
        se_f = c(hms::parse_hm("09:00"), hms::parse_hm("09:00")),
        abs = FALSE, method = "shorter"
    ),
    c(lubridate::dhours(1), lubridate::dhours(0))
    )
})

test_that("sjl_sc() | method test", {
    so_w <- hms::parse_hm("22:00")
    se_w <- hms::parse_hm("00:00")
    so_f <- hms::parse_hm("00:00")
    se_f <- hms::parse_hm("08:00")

    expect_equal(sjl_sc(
        so_w = so_w, se_w = se_w, so_f = so_f, se_f = se_f,
        abs = FALSE, method = "difference"
    ),
    lubridate::dhours(-22)
    )

    expect_equal(sjl_sc(
        so_w = so_w, se_w = se_w, so_f = so_f, se_f = se_f,
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(2)
    )

    expect_equal(sjl_sc(
        so_w = so_w, se_w = se_w, so_f = so_f, se_f = se_f,
        abs = FALSE, method = "longer"
    ),
    lubridate::dhours(-22)
    )

    so_w <- hms::parse_hm("02:00")
    se_w <- hms::parse_hm("10:00")
    so_f <- hms::parse_hm("03:00")
    se_f <- hms::parse_hm("11:00")

    expect_equal(sjl_sc(
        so_w = so_w, se_w = se_w, so_f = so_f, se_f = se_f,
        abs = FALSE, method = "difference"
    ),
    lubridate::dhours(1)
    )

    expect_equal(sjl_sc(
        so_w = so_w, se_w = se_w, so_f = so_f, se_f = se_f,
        abs = FALSE, method = "shorter"
    ),
    lubridate::dhours(1)
    )

    expect_equal(sjl_sc(
        so_w = so_w, se_w = se_w, so_f = so_f, se_f = se_f,
        abs = FALSE, method = "longer"
    ),
    lubridate::dhours(-23)
    )
})

test_that("sjl_sc() | error test", {
    # assert_hms(so_w, lower = hms::hms(0))
    expect_error(sjl_sc(
        so_w = 1, so_f = hms::hms(1), se_w = hms::hms(1), se_f = hms::hms(1),
        abs = TRUE, method = "shorter"
    ),
    "Assertion on 'so_w' failed"
    )

    # assert_hms(so_f, lower = hms::hms(0))
    expect_error(sjl_sc(
        so_w = hms::hms(1), so_f = 1, se_w = hms::hms(1), se_f = hms::hms(1),
        abs = TRUE, method = "shorter"
    ),
    "Assertion on 'so_f' failed"
    )

    # assert_hms(se_w, lower = hms::hms(0))
    expect_error(sjl_sc(
        so_w = hms::hms(1), so_f = hms::hms(1), se_w = 1, se_f = hms::hms(1),
        abs = TRUE, method = "shorter"
    ),
    "Assertion on 'se_w' failed"
    )

    # assert_hms(se_f, lower = hms::hms(0))
    expect_error(sjl_sc(
        so_w = hms::hms(1), so_f = hms::hms(1), se_w = hms::hms(1), se_f = 1,
        abs = TRUE, method = "shorter"
    ),
    "Assertion on 'se_f' failed"
    )

    # assert_identical(so_w, se_w, so_f, se_f, type = "length")
    expect_error(sjl_sc(
        so_w = hms::hms(1), so_f = hms::hms(1), se_w = hms::hms(1),
        se_f = c(hms::hms(1), hms::hms(1)),
        abs = TRUE, method = "shorter"
    ))

    # checkmate::assert_flag(abs)
    expect_error(sjl_sc(
        so_w = hms::hms(1), so_f = hms::hms(1), se_w = hms::hms(1),
        se_f = hms::hms(1), abs = 1, method = "shorter"
    ),
    "Assertion on 'abs' failed"
    )

    # checkmate::assert_choice(method, method_choices)
    expect_error(sjl_sc(
        so_w = hms::hms(1), so_f = hms::hms(1), se_w = hms::hms(1),
        se_f = hms::hms(1), abs = TRUE, method = 1
    ),
    "Assertion on 'method' failed"
    )
})

test_that("sjl_sc_rel() | wrappers", {
    expect_equal(sjl_sc_rel(
        so_w = hms::parse_hm("00:30"), se_w = hms::parse_hm("07:30"),
        so_f = hms::parse_hm("01:00"), se_f = hms::parse_hm("09:00")
    ),
    lubridate::dminutes(30)
    )

    expect_equal(sjl_sc_rel(
        so_w = hms::parse_hm("02:00"), se_w = hms::parse_hm("10:00"),
        so_f = hms::parse_hm("00:00"), se_f = hms::parse_hm("08:00")
    ),
    lubridate::dhours(-2)
    )
})
