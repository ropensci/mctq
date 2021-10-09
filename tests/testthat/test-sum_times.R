test_that("vct_sum_time()| linear test", {
    expect_equal(vct_sum_time(c(lubridate::dhours(1), lubridate::dminutes(30)),
                           c(lubridate::hours(1), lubridate::hours(1)),
                           c(as.difftime(1, units = "hours"),
                             as.difftime(30, units = "mins")),
                           c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                           c(lubridate::as_datetime("1970-01-01 20:00:00"),
                             lubridate::as_datetime("1970-01-01 10:00:00")),
                           as.POSIXlt(
                               c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                 lubridate::as_datetime(
                                     "1970-01-01 02:00:00"))),
                           c(lubridate::as.interval(lubridate::dhours(4),
                                                    as.Date("1970-01-01")),
                             lubridate::as.interval(lubridate::dhours(1),
                                                    as.Date("1970-01-01"))),
                           cycle = NULL,
                           na_rm = FALSE),
                 c(hms::hms(108000),
                   hms::hms(61200))) # 30:00:00 | 17:00:00
    expect_equal(vct_sum_time(c(hms::as_hms(NA), hms::as_hms(NA)), # !
                           c(lubridate::dhours(1), lubridate::dminutes(30)),
                           c(lubridate::hours(1), lubridate::hours(1)),
                           c(as.difftime(1, units = "hours"),
                             as.difftime(30, units = "mins")),
                           c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                           c(lubridate::as_datetime("1970-01-01 20:00:00"),
                             lubridate::as_datetime("1970-01-01 10:00:00")),
                           as.POSIXlt(
                               c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                 lubridate::as_datetime(
                                     "1970-01-01 02:00:00"))),
                           c(lubridate::as.interval(lubridate::dhours(4),
                                                    as.Date("1970-01-01")),
                             lubridate::as.interval(lubridate::dhours(1),
                                                    as.Date("1970-01-01"))),
                           cycle = NULL,
                           na_rm = FALSE), # !
                 c(hms::as_hms(NA), hms::as_hms(NA)))
    expect_equal(vct_sum_time(c(hms::as_hms(NA), hms::as_hms(NA)), # !
                           c(lubridate::dhours(1), lubridate::dminutes(30)),
                           c(lubridate::hours(1), lubridate::hours(1)),
                           c(as.difftime(1, units = "hours"),
                             as.difftime(30, units = "mins")),
                           c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                           c(lubridate::as_datetime("1970-01-01 20:00:00"),
                             lubridate::as_datetime("1970-01-01 10:00:00")),
                           as.POSIXlt(
                               c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                 lubridate::as_datetime(
                                     "1970-01-01 02:00:00"))),
                           c(lubridate::as.interval(lubridate::dhours(4),
                                                    as.Date("1970-01-01")),
                             lubridate::as.interval(lubridate::dhours(1),
                                                    as.Date("1970-01-01"))),
                           cycle = NULL,
                           na_rm = TRUE), # !
                 c(hms::hms(108000),
                   hms::hms(61200))) # 30:00:00 | 17:00:00
})

test_that("vct_sum_time()| circular test", {
    expect_equal(vct_sum_time(c(lubridate::dhours(1), lubridate::dminutes(30)),
                           c(lubridate::hours(1), lubridate::hours(1)),
                           c(as.difftime(1, units = "hours"),
                             as.difftime(30, units = "mins")),
                           c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                           c(lubridate::as_datetime("1970-01-01 20:00:00"),
                             lubridate::as_datetime("1970-01-01 10:00:00")),
                           as.POSIXlt(
                               c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                 lubridate::as_datetime(
                                     "1970-01-01 02:00:00"))),
                           c(lubridate::as.interval(lubridate::dhours(4),
                                                    as.Date("1970-01-01")),
                             lubridate::as.interval(lubridate::dhours(1),
                                                    as.Date("1970-01-01"))),
                           cycle = lubridate::ddays(), # !
                           na_rm = FALSE),
                 c(hms::parse_hm("06:00"),
                   hms::parse_hm("17:00"))) # 06:00:00 | 17:00:00
    expect_equal(vct_sum_time(c(hms::as_hms(NA), hms::as_hms(NA)),
                           c(lubridate::dhours(1), lubridate::dminutes(30)),
                           c(lubridate::hours(1), lubridate::hours(1)),
                           c(as.difftime(1, units = "hours"),
                             as.difftime(30, units = "mins")),
                           c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
                           c(lubridate::as_datetime("1970-01-01 20:00:00"),
                             lubridate::as_datetime("1970-01-01 10:00:00")),
                           as.POSIXlt(
                               c(lubridate::as_datetime("1970-01-01 01:00:00"),
                                 lubridate::as_datetime(
                                     "1970-01-01 02:00:00"))),
                           c(lubridate::as.interval(lubridate::dhours(4),
                                                    as.Date("1970-01-01")),
                             lubridate::as.interval(lubridate::dhours(1),
                                                    as.Date("1970-01-01"))),
                           cycle = lubridate::ddays(), # !
                           na_rm = TRUE),
                 c(hms::hms(21600), hms::hms(61200)))
})
