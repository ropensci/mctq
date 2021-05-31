test_that("sum_time() | non-vectorized test", {
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
                          class = "duration",
                          circular = FALSE,
                          vectorize = FALSE,
                          na.rm = FALSE),
                 lubridate::dhours(30)) # 30:00:00
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
                          class = "hms",
                          circular = TRUE, # !
                          vectorize = FALSE,
                          na.rm = FALSE),
                 hms::parse_hm("06:00")) # 06:00 | 30 - 24
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
                          class = "period",
                          circular = FALSE,
                          vectorize = FALSE,
                          na.rm = FALSE), # !
                 lubridate::as.period(NA))
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
                          class = "period",
                          circular = FALSE,
                          vectorize = FALSE,
                          na.rm = TRUE), # !
                 lubridate::as.period(hms::hms(108000))) # 30:00:00
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
                          class = "difftime",
                          circular = TRUE, # !
                          vectorize = FALSE,
                          na.rm = TRUE),
                 lubridate::as.difftime(21600, units = "secs"))
})

test_that("sum_time()| vectorized test", {
    expect_equal(sum_time(c(lubridate::dhours(1), lubridate::dminutes(30)),
                          c(lubridate::hours(1), lubridate::hours(1)),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          c(lubridate::as_datetime("1970-01-01 20:00:00"),
                            lubridate::as_datetime("1970-01-01 10:00:00")),
                          as.POSIXlt(
                              c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                lubridate::as_datetime("1970-01-01 02:00:00"))),
                          c(lubridate::as.interval(lubridate::dhours(4),
                                                   as.Date("1970-01-01")),
                            lubridate::as.interval(lubridate::dhours(1),
                                                   as.Date("1970-01-01"))),
                          class = "duration",
                          circular = FALSE,
                          vectorize = TRUE,
                          na.rm = FALSE),
                 c(lubridate::dhours(30),
                   lubridate::dhours(17))) # 30:00:00 | 17:00:00
    expect_equal(sum_time(c(lubridate::dhours(1), lubridate::dminutes(30)),
                          c(lubridate::hours(1), lubridate::hours(1)),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          c(lubridate::as_datetime("1970-01-01 20:00:00"),
                            lubridate::as_datetime("1970-01-01 10:00:00")),
                          as.POSIXlt(
                              c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                lubridate::as_datetime("1970-01-01 02:00:00"))),
                          c(lubridate::as.interval(lubridate::dhours(4),
                                                   as.Date("1970-01-01")),
                            lubridate::as.interval(lubridate::dhours(1),
                                                   as.Date("1970-01-01"))),
                          class = "hms",
                          circular = TRUE, # !
                          vectorize = TRUE,
                          na.rm = FALSE),
                 c(hms::parse_hm("06:00"),
                   hms::parse_hm("17:00"))) # 06:00:00 | 17:00:00
    expect_equal(sum_time(c(hms::as_hms(NA), hms::as_hms(NA)), # !
                          c(lubridate::dhours(1), lubridate::dminutes(30)),
                          c(lubridate::hours(1), lubridate::hours(1)),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          c(lubridate::as_datetime("1970-01-01 20:00:00"),
                            lubridate::as_datetime("1970-01-01 10:00:00")),
                          as.POSIXlt(
                              c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                lubridate::as_datetime("1970-01-01 02:00:00"))),
                          c(lubridate::as.interval(lubridate::dhours(4),
                                                   as.Date("1970-01-01")),
                            lubridate::as.interval(lubridate::dhours(1),
                                                   as.Date("1970-01-01"))),
                          class = "period",
                          circular = FALSE,
                          vectorize = TRUE,
                          na.rm = FALSE), # !
                 c(lubridate::as.period(NA),
                   lubridate::as.period(NA)))
    expect_equal(sum_time(c(hms::as_hms(NA), hms::as_hms(NA)), # !
                          c(lubridate::dhours(1), lubridate::dminutes(30)),
                          c(lubridate::hours(1), lubridate::hours(1)),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          c(lubridate::as_datetime("1970-01-01 20:00:00"),
                            lubridate::as_datetime("1970-01-01 10:00:00")),
                          as.POSIXlt(
                              c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                lubridate::as_datetime("1970-01-01 02:00:00"))),
                          c(lubridate::as.interval(lubridate::dhours(4),
                                                   as.Date("1970-01-01")),
                            lubridate::as.interval(lubridate::dhours(1),
                                                   as.Date("1970-01-01"))),
                          class = "period",
                          circular = FALSE,
                          vectorize = TRUE,
                          na.rm = TRUE), # !
                 c(lubridate::as.period(hms::hms(108000)),
                   lubridate::as.period(hms::hms(61200)))) # 30:00:00 | 17:00:00
    expect_equal(sum_time(c(hms::as_hms(NA), hms::as_hms(NA)),
                          c(lubridate::dhours(1), lubridate::dminutes(30)),
                          c(lubridate::hours(1), lubridate::hours(1)),
                          c(as.difftime(1, units = "hours"),
                            as.difftime(30, units = "mins")),
                          c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                          c(lubridate::as_datetime("1970-01-01 20:00:00"),
                            lubridate::as_datetime("1970-01-01 10:00:00")),
                          as.POSIXlt(
                              c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                lubridate::as_datetime("1970-01-01 02:00:00"))),
                          c(lubridate::as.interval(lubridate::dhours(4),
                                                   as.Date("1970-01-01")),
                            lubridate::as.interval(lubridate::dhours(1),
                                                   as.Date("1970-01-01"))),
                          class = "difftime",
                          circular = TRUE, # !
                          vectorize = TRUE,
                          na.rm = TRUE),
                 c(lubridate::as.difftime(21600, units = "secs"),
                   lubridate::as.difftime(61200, units = "secs")))
})

test_that("sum_time() | error test", {
    expect_error(sum_time(1, class = "hms", circular = TRUE,
                          vectorize = TRUE, na.rm = TRUE),
                 "Assertion on 'x' failed")
    expect_error(sum_time(hms::hms(1), class = 1, circular = TRUE,
                          vectorize = TRUE, na.rm = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(sum_time(hms::hms(1), class = "hms", circular = "",
                          vectorize = TRUE, na.rm = TRUE),
                 "Assertion on 'circular' failed")
    expect_error(sum_time(hms::hms(1), class = "hms", circular = TRUE,
                          vectorize = "", na.rm = TRUE),
                 "Assertion on 'vectorize' failed")
    expect_error(sum_time(hms::hms(1), class = "hms", circular = TRUE,
                          vectorize = TRUE, na.rm = ""),
                 "Assertion on 'na.rm' failed")

    expect_error(sum_time(hms::hms(1), c(hms::hms(1), hms::hms(1)),
                          vectorize = TRUE),
                 "When 'vectorize' is 'TRUE', all values in '...' must ")
})
