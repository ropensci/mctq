test_that("msl() | scalar test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(msl(hms::parse_hm("22:00"), lubridate::dhours(8)),
                 hms::parse_hm("02:00"))
    expect_equal(msl(hms::parse_hm("02:00"), lubridate::dhours(6)),
                 hms::parse_hm("05:00"))
    expect_equal(msl(hms::as_hms(NA), lubridate::dhours(6)), hms::as_hms(NA))
})

test_that("msl() | vector test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_equal(msl(c(hms::parse_hm("23:30"), hms::parse_hm("03:30")),
                     c(lubridate::dhours(8), lubridate::dhours(10))),
                 c(hms::parse_hm("03:30"), hms::parse_hm("08:30")))
    expect_equal(msl(c(hms::parse_hm("04:15"), hms::parse_hm("21:00")),
                     c(lubridate::dhours(6.5), lubridate::as.duration(NA))),
                 c(hms::parse_hm("07:30"), hms::as_hms(NA)))
})

test_that("msl() | error test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    expect_error(msl(1, lubridate::duration(1)),
                 "Assertion on 'so' failed")
    expect_error(msl(hms::hms(1), 1),
                 "Assertion on 'sd' failed")
    expect_error(msl(hms::hms(1), c(lubridate::duration(1),
                                    lubridate::duration(1))))
})
