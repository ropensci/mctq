test_that("sjl() | scalar test", {
    msw <- hms::parse_hm("02:30")
    msf <- hms::parse_hm("04:30")
    abs <- TRUE
    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(2)
    expect_equal(object, expected)

    msw <- hms::parse_hm("23:00")
    msf <- hms::parse_hm("02:00")
    abs <- TRUE
    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(3)
    expect_equal(object, expected)

    msw <- hms::parse_hm("05:00")
    msf <- hms::parse_hm("03:00")
    abs <- FALSE
    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-2)
    expect_equal(object, expected)

    msw <- hms::as_hms(NA)
    msf <- hms::parse_hm("00:00")
    abs <- FALSE
    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::as.duration(NA)
    expect_equal(object, expected)
})

test_that("sjl() | vector test", {
    msw <- c(hms::parse_hm("11:00"), hms::parse_hm("22:00"))
    msf <- c(hms::parse_hm("18:30"), hms::parse_hm("17:30"))
    abs <- FALSE
    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- c(lubridate::dhours(7.5), lubridate::dhours(-4.5))
    expect_equal(object, expected)
})

test_that("sjl() | `method` test", {
    msw <- hms::parse_hm("08:00")
    msf <- hms::parse_hm("12:00")
    abs <- FALSE
    method <- "difference"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(4)
    expect_equal(object, expected)

    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(4)
    expect_equal(object, expected)

    method <- "longer"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-20)
    expect_equal(object, expected)

    msw <- hms::parse_hm("12:00")
    msf <- hms::parse_hm("08:00")
    abs <- FALSE
    method <- "difference"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-4)
    expect_equal(object, expected)

    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-4)
    expect_equal(object, expected)

    method <- "longer"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(20)
    expect_equal(object, expected)


    msw <- hms::parse_hm("23:00")
    msf <- hms::parse_hm("01:00")
    abs <- FALSE
    method <- "difference"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-22)
    expect_equal(object, expected)

    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(2)
    expect_equal(object, expected)

    method <- "longer"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-22)
    expect_equal(object, expected)

    msw <- hms::parse_hm("01:00")
    msf <- hms::parse_hm("23:00")
    abs <- FALSE
    method <- "difference"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(22)
    expect_equal(object, expected)

    method <- "shortest"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(-2)
    expect_equal(object, expected)

    method <- "longer"
    object <- sjl(msw, msf, abs, method)
    expected <- lubridate::dhours(22)
    expect_equal(object, expected)
})

test_that("sjl() | error test", {
    # Invalid values for `msw`, `msf`, `abs`, and `method`
    expect_error(sjl(1, hms::hms(1), TRUE, "shortest"))
    expect_error(sjl(hms::hms(1), 1, TRUE, "shortest"))
    expect_error(sjl(hms::hms(1), hms::hms(1), "", "shortest"))
    expect_error(sjl(hms::hms(1), hms::hms(1), TRUE, 1))

    # `msw` and `msf` have different lengths
    expect_error(sjl(hms::hms(1), c(hms::hms(1), hms::hms(1))))
})

test_that("sjl() | wrappers", {
    msw <- hms::parse_hm("10:00")
    msf <- hms::parse_hm("12:00")
    method <- "shortest"
    object <- sjl_rel(msw, msf, method)
    expected <- lubridate::dhours(2)
    expect_equal(object, expected)

    msw <- hms::parse_hm("03:30")
    msf <- hms::parse_hm("03:00")
    method <- "shortest"
    object <- sjl_rel(msw, msf, method)
    expected <- lubridate::dhours(-0.5)
    expect_equal(object, expected)
})
