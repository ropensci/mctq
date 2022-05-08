test_that("cycle_time() | general test", {
    ## Nonexistent method error
    ## The error message may change depending on the user's 'locale' settings.
    expect_error(cycle_time(list()))
})

test_that("cycle_time.numeric() | scalar test", {
    expect_equal(cycle_time(
        time = 6, cycle = 24, reverse = FALSE
    ),
    6
    )

    expect_equal(cycle_time(
        time = as.numeric(NA), cycle = 24, reverse = FALSE
    ),
    as.numeric(NA)
    )

    expect_equal(cycle_time(
        time = 24, cycle = 24, reverse = FALSE
    ),
    0
    )

    expect_equal(cycle_time(
        time = -25, cycle = 24, reverse = FALSE
    ),
    -1
    )

    expect_equal(cycle_time(
        time = -25, cycle = 24, reverse = TRUE
    ),
    23
    )
})

test_that("cycle_time.numeric() | vector test", {
    expect_equal(cycle_time(
        time = c(1, 36), cycle = 24, reverse = FALSE
    ),
    c(1, 12)
    )

    expect_equal(cycle_time(
        time = c(NA, 25), cycle = 24, reverse = FALSE
    ),
    c(NA, 1)
    )

    expect_equal(cycle_time(
        time = c(-36, 6), cycle = 24, reverse = FALSE
    ),
    c(-12, 6)
    )

    expect_equal(cycle_time(
        time = c(-30, 6), cycle = 24, reverse = TRUE
    ),
    c(18, 6)
    )
})

test_that("cycle_time.Duration() | scalar test", {
    expect_equal(cycle_time(
        time = lubridate::dhours(6),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    lubridate::dhours(6)
    )

    expect_equal(cycle_time(
        time = lubridate::as.duration(NA),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    lubridate::as.duration(NA)
    )

    expect_equal(cycle_time(
        time = lubridate::dhours(24),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    lubridate::dhours(0)
    )

    expect_equal(cycle_time(
        time = lubridate::dhours(-36),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    lubridate::dhours(-12)
    )

    expect_equal(cycle_time(
        time = lubridate::dhours(-36),
        cycle = lubridate::ddays(),
        reverse = TRUE
    ),
    lubridate::dhours(12)
    )
})

test_that("cycle_time.Duration() | vector test", {
    expect_equal(cycle_time(
        time = c(lubridate::dhours(1), lubridate::dhours(48)),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    c(lubridate::dhours(1), lubridate::dhours(0))
    )

    expect_equal(cycle_time(
        time = c(lubridate::as.duration(NA),
                 lubridate::dhours(25)),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    c(lubridate::as.duration(NA), lubridate::dhours(1))
    )

    expect_equal(cycle_time(
        time = c(lubridate::dhours(-25), lubridate::dhours(25)),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    c(lubridate::dhours(-1), lubridate::dhours(1))
    )

    expect_equal(cycle_time(
        time = c(lubridate::dhours(-25), lubridate::dhours(25)),
        cycle = lubridate::ddays(),
        reverse = TRUE
    ),
    c(lubridate::dhours(23), lubridate::dhours(1))
    )
})

test_that("cycle_time.difftime() | scalar test", {
    expect_equal(cycle_time(
        time = as.difftime(6, units = "mins"),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    as.difftime(6, units = "mins")
    )

    expect_equal(cycle_time(
        time = as.difftime(24, units = "hours"),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    as.difftime(0, units = "hours")
    )

    expect_equal(cycle_time(
        time = as.difftime(36, units = "hours"),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    as.difftime(12, units = "hours")
    )
})

test_that("cycle_time.difftime() | vector test", {
    expect_equal(cycle_time(
        time = c(as.difftime(1, units = "hours"),
                 as.difftime(48, units = "hours")),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    c(as.difftime(1, units = "hours"),
      as.difftime(0, units = "hours"))
    )
})

test_that("cycle_time.hms() | scalar test", {
    expect_equal(cycle_time(
        time = hms::parse_hm("06:00"),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    hms::parse_hm("06:00")
    )

    expect_equal(cycle_time(
        time = hms::parse_hm("24:00"),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    hms::parse_hm("00:00")
    )

    expect_equal(cycle_time(
        time = hms::hms(129600),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    hms::parse_hm("12:00")
    )
})

test_that("cycle_time.hms() | vector test", {
    expect_equal(cycle_time(
        time = c(hms::parse_hm("01:00"), hms::hms(172800)),
        cycle = lubridate::ddays(),
        reverse = FALSE
    ),
    c(hms::parse_hm("01:00"), hms::parse_hm("00:00"))
    )
})

test_that("cycle_time_build() | error test", {
    # checkmate::assert_multi_class(cycle, c("numeric", "Duration"))
    expect_error(cycle_time_build(
        time = 1, cycle = "a", reverse = FALSE
    ),
    "Assertion on 'cycle' failed"
    )

    expect_error(cycle_time_build(
        time = 1, cycle = lubridate::as_datetime(1), reverse = FALSE
    ),
    "Assertion on 'cycle' failed"
    )

    # checkmate::assert_number(as.numeric(cycle), lower = 0, null.ok = FALSE)

    expect_error(cycle_time_build(
        time = 1, cycle = -1, reverse = FALSE
    ),
    "Assertion on 'as.numeric\\(cycle\\)' failed"
    )

    # checkmate::assert_flag(reverse)
    expect_error(cycle_time_build(
        time = 1, cycle = 1, reverse = 1
    ),
    "Assertion on 'reverse' failed"
    )
})
