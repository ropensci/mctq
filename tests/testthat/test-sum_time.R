test_that("sum_time() | linear test", {
    expect_equal(sum_time(c(lubridate::dhours(1), lubridate::dminutes(30)),
                          lubridate::hours(1),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          lubridate::as_datetime("1970-01-01 20:00:00"),
                          as.POSIXlt(lubridate::as_datetime(
                              "1970-01-01 01:00:00")),
                          lubridate::as.interval(lubridate::dhours(1),
                                                 as.Date("1970-01-01")),
                          cycle = NULL,
                          na_rm = FALSE),
                 hms::hms(as.numeric(lubridate::dhours(30)))) # 30:00:00
    expect_equal(sum_time(hms::as_hms(NA), # !
                          c(lubridate::dhours(1), lubridate::dminutes(30)),
                          lubridate::hours(1),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          lubridate::as_datetime("1970-01-01 20:00:00"),
                          as.POSIXlt(lubridate::as_datetime(
                              "1970-01-01 01:00:00")),
                          lubridate::as.interval(lubridate::dhours(1),
                                                 as.Date("1970-01-01")),
                          cycle = NULL,
                          na_rm = FALSE), # !
                 hms::as_hms(NA))
    expect_equal(sum_time(hms::as_hms(NA), # !
                          c(lubridate::dhours(1), lubridate::dminutes(30)),
                          lubridate::hours(1),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          lubridate::as_datetime("1970-01-01 20:00:00"),
                          as.POSIXlt(lubridate::as_datetime(
                              "1970-01-01 01:00:00")),
                          lubridate::as.interval(lubridate::dhours(1),
                                                 as.Date("1970-01-01")),
                          cycle = NULL,
                          na_rm = TRUE), # !
                 hms::hms(as.numeric(lubridate::dhours(30)))) # 30:00:00
})

test_that("sum_time() | circular test", {
    expect_equal(sum_time(c(lubridate::dhours(1), lubridate::dminutes(30)),
                          lubridate::hours(1),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          lubridate::as_datetime("1970-01-01 20:00:00"),
                          as.POSIXlt(lubridate::as_datetime(
                              "1970-01-01 01:00:00")),
                          lubridate::as.interval(lubridate::dhours(1),
                                                 as.Date("1970-01-01")),
                          cycle = lubridate::ddays(), # !
                          na_rm = FALSE),
                 hms::parse_hm("06:00")) # 06:00 | 30 - 24
    expect_equal(sum_time(hms::as_hms(NA),
                          c(lubridate::dhours(1), lubridate::dminutes(30)),
                          lubridate::hours(1),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          lubridate::as_datetime("1970-01-01 20:00:00"),
                          as.POSIXlt(lubridate::as_datetime(
                              "1970-01-01 01:00:00")),
                          lubridate::as.interval(lubridate::dhours(1),
                                                 as.Date("1970-01-01")),
                          cycle = lubridate::ddays(),
                          na_rm = TRUE),
                 hms::parse_hm("06:00"))
})

test_that("build_sum() | error test", {
    expect_error(build_sum(1, vectorize = FALSE, cycle = NULL, na_rm = FALSE),
                 "Assertion on 'x' failed")
    expect_error(build_sum(hms::hms(1), vectorize = "", cycle = NULL,
                           na_rm = TRUE),
                 "Assertion on 'vectorize' failed")
    expect_error(build_sum(hms::hms(1), vectorize = FALSE, cycle = "",
                           na_rm = FALSE),
                 "Assertion on 'cycle' failed")
    expect_error(build_sum(hms::hms(1), vectorize = FALSE, cycle = NULL,
                           na_rm = ""),
                 "Assertion on 'na_rm' failed")

    expect_error(build_sum(hms::hms(1), c(hms::hms(1), hms::hms(1)),
                          vectorize = TRUE),
                 "All values in '...' must ")
})
