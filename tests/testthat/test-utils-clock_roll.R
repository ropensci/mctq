test_that("clock_roll() | general test", {
    # Nonexistent method error
    expect_error(clock_roll(list()))
})

test_that("clock_roll.Duration() | general test", {
    expect_equal(clock_roll(lubridate::dhours(6)), lubridate::dhours(6))
    expect_equal(clock_roll(lubridate::dhours(24)), lubridate::dhours(0))
    expect_equal(clock_roll(lubridate::dhours(36)), lubridate::dhours(12))

    expect_equal(clock_roll(c(lubridate::dhours(1), lubridate::dhours(48))),
                 c(lubridate::dhours(1), lubridate::dhours(0)))
})

test_that("clock_roll.Period() | general test", {
    expect_equal(clock_roll(lubridate::hours(6)), lubridate::hours(6))
    expect_equal(clock_roll(lubridate::hours(24)), lubridate::hours(0))
    expect_equal(clock_roll(lubridate::hours(36)), lubridate::hours(12))

    expect_equal(clock_roll(c(lubridate::hours(1), lubridate::hours(48))),
                 c(lubridate::hours(1), lubridate::hours(0)))
})

test_that("clock_roll.difftime() | general test", {
    expect_equal(clock_roll(as.difftime(6, units = "mins")),
                 as.difftime(6, units = "mins"))
    expect_equal(clock_roll(as.difftime(24, units = "hours")),
                 as.difftime(0, units = "hours"))
    expect_equal(clock_roll(as.difftime(36, units = "hours")),
                 as.difftime(12, units = "hours"))

    expect_equal(clock_roll(c(as.difftime(1, units = "hours"),
                              as.difftime(48, units = "hours"))),
                 c(as.difftime(1, units = "hours"),
                   as.difftime(0, units = "hours")))
})

test_that("clock_roll.hms() | general test", {
    expect_equal(clock_roll(hms::parse_hm("06:00")), hms::parse_hm("06:00"))
    expect_equal(clock_roll(hms::parse_hm("24:00")), hms::parse_hm("00:00"))
    expect_equal(clock_roll(hms::hms(129600)), hms::parse_hm("12:00"))

    expect_equal(clock_roll(c(hms::parse_hm("01:00"), hms::hms(172800))),
                 c(hms::parse_hm("01:00"), hms::parse_hm("00:00")))
})
