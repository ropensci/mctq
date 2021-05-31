test_that("sjl() | scalar test", {
    expect_equal(sjl(hms::parse_hm("02:30"),
                     hms::parse_hm("04:30"),
                     TRUE,
                     "shorter"),
                 lubridate::dhours(2))
    expect_equal(sjl(hms::parse_hm("23:00"),
                     hms::parse_hm("02:00"),
                     TRUE,
                     "shorter"),
                 lubridate::dhours(3))
    expect_equal(sjl(hms::parse_hm("05:00"),
                     hms::parse_hm("03:00"),
                     FALSE,
                     "shorter"),
                 lubridate::dhours(-2))
    expect_equal(sjl(hms::as_hms(NA),
                     hms::parse_hm("00:00"),
                     FALSE,
                     "shorter"),
                 lubridate::as.duration(NA))
})

test_that("sjl() | vector test", {
    expect_equal(sjl(c(hms::parse_hm("11:00"), hms::parse_hm("22:00")),
                     c(hms::parse_hm("18:30"), hms::parse_hm("17:30")),
                     FALSE,
                     "shorter"),
                 c(lubridate::dhours(7.5), lubridate::dhours(-4.5)))
})

test_that("sjl() | `method` test", {
    expect_equal(sjl(hms::parse_hm("08:00"),
                     hms::parse_hm("12:00"),
                     FALSE,
                     "difference"),
                 lubridate::dhours(4))
    expect_equal(sjl(hms::parse_hm("08:00"),
                     hms::parse_hm("12:00"),
                     FALSE,
                     "shorter"),
                 lubridate::dhours(4))
    expect_equal(sjl(hms::parse_hm("08:00"),
                     hms::parse_hm("12:00"),
                     FALSE,
                     "longer"),
                 lubridate::dhours(-20))

    expect_equal(sjl(hms::parse_hm("12:00"),
                     hms::parse_hm("08:00"),
                     FALSE,
                     "difference"),
                 lubridate::dhours(-4))
    expect_equal(sjl(hms::parse_hm("12:00"),
                     hms::parse_hm("08:00"),
                     FALSE,
                     "shorter"),
                 lubridate::dhours(-4))
    expect_equal(sjl(hms::parse_hm("12:00"),
                     hms::parse_hm("08:00"),
                     FALSE,
                     "longer"),
                 lubridate::dhours(20))

    expect_equal(sjl(hms::parse_hm("23:00"),
                     hms::parse_hm("01:00"),
                     FALSE,
                     "difference"),
                 lubridate::dhours(-22))
    expect_equal(sjl(hms::parse_hm("23:00"),
                     hms::parse_hm("01:00"),
                     FALSE,
                     "shorter"),
                 lubridate::dhours(2))
    expect_equal(sjl(hms::parse_hm("23:00"),
                     hms::parse_hm("01:00"),
                     FALSE,
                     "longer"),
                 lubridate::dhours(-22))

    expect_equal(sjl(hms::parse_hm("01:00"),
                     hms::parse_hm("23:00"),
                     FALSE,
                     "difference"),
                 lubridate::dhours(22))
    expect_equal(sjl(hms::parse_hm("01:00"),
                     hms::parse_hm("23:00"),
                     FALSE,
                     "shorter"),
                 lubridate::dhours(-2))
    expect_equal(sjl(hms::parse_hm("01:00"),
                     hms::parse_hm("23:00"),
                     FALSE,
                     "longer"),
                 lubridate::dhours(22))
})

test_that("sjl() | error test", {
    expect_error(sjl(1, hms::hms(1), TRUE, "shorter"),
                 "Assertion on 'msw' failed")
    expect_error(sjl(hms::hms(1), 1, TRUE, "shorter"),
                 "Assertion on 'msf' failed")
    expect_error(sjl(hms::hms(1), hms::hms(1), "", "shorter"),
                 "Assertion on 'abs' failed")
    expect_error(sjl(hms::hms(1), hms::hms(1), TRUE, 1),
                 "Assertion on 'method' failed")

    expect_error(sjl(hms::hms(1), c(hms::hms(1), hms::hms(1))),
                 "'msw' and 'msf' must have identical lengths.")
})

test_that("sjl() | wrappers", {
    expect_equal(sjl_rel(hms::parse_hm("10:00"),
                         hms::parse_hm("12:00"),
                         "shorter"),
                 lubridate::dhours(2))
    expect_equal(sjl_rel(hms::parse_hm("03:30"),
                         hms::parse_hm("03:00"),
                         "shorter"),
                 lubridate::dhours(-0.5))
})
