test_that("assign_date() | scalar test", {
  expect_equal(assign_date(hms::parse_hm("02:10"), hms::parse_hm("05:30")),
               lubridate::interval(
                 lubridate::as_datetime("1970-01-01 02:10:00"),
                 lubridate::as_datetime("1970-01-01 05:30:00")))
})

test_that("assign_date() | vector test", {
  expect_equal(assign_date(c(hms::parse_hm("05:40"), hms::parse_hm("21:30")),
                           c(hms::parse_hm("18:05"), hms::parse_hm("10:30"))),
               c(lubridate::interval(
                 lubridate::as_datetime("1970-01-01 05:40:00"),
                 lubridate::as_datetime("1970-01-01 18:05:00")),
                 lubridate::interval(
                   lubridate::as_datetime("1970-01-01 21:30:00"),
                   lubridate::as_datetime("1970-01-02 10:30:00"))))
})

test_that("assign_date() | `ambiguity` test", {
    expect_equal(assign_date(lubridate::as_datetime("1985-01-15 12:00:00"),
                             lubridate::as_datetime("2020-09-10 12:00:00"),
                             ambiguity = 0),
                 lubridate::interval(
                   lubridate::as_datetime("1970-01-01 12:00:00"),
                   lubridate::as_datetime("1970-01-01 12:00:00")))

    expect_equal(assign_date(lubridate::as_datetime("1985-01-15 12:00:00"),
                             lubridate::as_datetime("2020-09-10 12:00:00"),
                             ambiguity = 24),
                 lubridate::interval(
                   lubridate::as_datetime("1970-01-01 12:00:00"),
                   lubridate::as_datetime("1970-01-02 12:00:00")))

    expect_equal(assign_date(lubridate::as_datetime("1985-01-15 12:00:00"),
                             lubridate::as_datetime("2020-09-10 12:00:00"),
                             ambiguity = NA),
                 lubridate::as.interval(NA))
})

test_that("assign_date() | `return` test", {
    expect_equal(assign_date(hms::parse_hm("22:15"),
                             hms::parse_hm("00:00"),
                             return = "list",
                             start_name = "start",
                             end_name = "end"),
                 list(start = lubridate::as_datetime("1970-01-01 22:15:00"),
                      end = lubridate::as_datetime("1970-01-02 00:00:00")))

    expect_equal(assign_date(hms::parse_hm("01:10"),
                             hms::parse_hm("11:45"),
                             return = "list",
                             start_name = "start",
                             end_name = "end"),
                 list(start = lubridate::as_datetime("1970-01-01 01:10:00"),
                      end = lubridate::as_datetime("1970-01-01 11:45:00")))

    expect_equal(assign_date(lubridate::parse_date_time("01:10:00", "HMS"),
                             lubridate::parse_date_time("11:45:00", "HMS"),
                             return = "start"),
                 lubridate::as_datetime("1970-01-01 01:10:00"))

    expect_equal(assign_date(lubridate::parse_date_time("01:10:00", "HMS"),
                             lubridate::parse_date_time("11:45:00", "HMS"),
                             return = "end"),
                 lubridate::as_datetime("1970-01-01 11:45:00"))

    expect_equal(assign_date(lubridate::parse_date_time("21:45:00", "HMS"),
                             lubridate::parse_date_time("03:20:00", "HMS"),
                             return = "start"),
                 lubridate::as_datetime("1970-01-01 21:45:00"))

    expect_equal(assign_date(lubridate::parse_date_time("21:45:00", "HMS"),
                             lubridate::parse_date_time("03:20:00", "HMS"),
                             return = "end"),
                 lubridate::as_datetime("1970-01-02 03:20:00"))
})

test_that("assign_date() | `start_name` and `end_name` test", {
    checkmate::expect_names(names(assign_date(hms::parse_hm("23:00"),
                                              hms::parse_hm("01:00"),
                                              return = "list")),
                            identical.to = c('hms::parse_hm("23:00")',
                                             'hms::parse_hm("01:00")'))
})

test_that("assign_date() | error test", {
  expect_error(assign_date(1, hms::hms(1)), "Assertion on 'start' failed")
  expect_error(assign_date(hms::hms(1), 1), "Assertion on 'end' failed")
  expect_error(assign_date(hms::hms(1), c(hms::hms(1), hms::hms(1))),
               "'start' and 'end' must have identical lengths")
  expect_error(assign_date(hms::hms(1), hms::hms(1) , return = "x"),
               "Assertion on 'tolower\\(return\\)' failed")
  expect_error(assign_date(hms::hms(1), hms::hms(1) , ambiguity = "x"),
               "Assertion on 'ambiguity' failed")
  expect_error(assign_date(hms::hms(1), hms::hms(1) , start_name = 1),
               "Assertion on 'start_name' failed")
  expect_error(assign_date(hms::hms(1), hms::hms(1) , end_name = 1),
               "Assertion on 'end_name' failed")
})
