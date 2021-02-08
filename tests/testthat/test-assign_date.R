test_that("assign_date() | scalar test", {
  start <- hms::parse_hms("02:10:00")
  end <- hms::parse_hms("05:30:00")
  object <- assign_date(start, end)
  expected <- lubridate::interval(
    lubridate::as_datetime("1970-01-01 02:10:00"),
    lubridate::as_datetime("1970-01-01 05:30:00"))
  expect_equal(object, expected)
})

test_that("assign_date() | vector test", {
  start <- c(hms::parse_hm("05:40"), hms::parse_hm("21:30"))
  end <- c(hms::parse_hm("18:05"), hms::parse_hm("10:30"))
  object <- assign_date(start, end)
  expected <- c(
    lubridate::interval(
      lubridate::as_datetime("1970-01-01 05:40:00"),
      lubridate::as_datetime("1970-01-01 18:05:00")),
    lubridate::interval(
      lubridate::as_datetime("1970-01-01 21:30:00"),
      lubridate::as_datetime("1970-01-02 10:30:00")))
  expect_equal(object, expected)
})

test_that("assign_date() | `ambiguity` test", {
    start <- lubridate::as_datetime("1985-01-15 12:00:00")
    end <- lubridate::as_datetime("2020-09-10 12:00:00")
    object <- assign_date(start, end, ambiguity = 0)
    expected <- lubridate::interval(
      lubridate::as_datetime("1970-01-01 12:00:00"),
      lubridate::as_datetime("1970-01-01 12:00:00"))
    expect_equal(object, expected)

    object <- assign_date(start, end, ambiguity = 24)
    expected <- lubridate::interval(
      lubridate::as_datetime("1970-01-01 12:00:00"),
      lubridate::as_datetime("1970-01-02 12:00:00"))
    expect_equal(object, expected)

    object <- assign_date(start, end, ambiguity = NA)
    expected <- lubridate::as.interval(NA)
    expect_equal(object, expected)
})

test_that("assign_date() | `return` test", {
    start <- hms::parse_hms("22:15:00")
    end <- hms::parse_hms("00:00:00")
    object <- assign_date(start, end, return = "list")
    expected <- list(start = lubridate::as_datetime("1970-01-01 22:15:00"),
                     end = lubridate::as_datetime("1970-01-02 00:00:00"))
    expect_equal(object, expected)

    start <- hms::parse_hms("01:10:00")
    end <- hms::parse_hms("11:45:00")
    object <- assign_date(start, end, return = "list")
    expected <- list(start = lubridate::as_datetime("1970-01-01 01:10:00"),
                     end = lubridate::as_datetime("1970-01-01 11:45:00"))
    expect_equal(object, expected)

    start <- lubridate::parse_date_time("01:10:00", "HMS")
    end <- lubridate::parse_date_time("11:45:00", "HMS")
    object <- assign_date(start, end, return = "start")
    expect_equal(object, lubridate::as_datetime("1970-01-01 01:10:00"))

    object <- assign_date(start, end, return = "end")
    expect_equal(object, lubridate::as_datetime("1970-01-01 11:45:00"))

    start <- lubridate::parse_date_time("21:45:00", "HMS")
    end <- lubridate::parse_date_time("03:20:00", "HMS")
    object <- assign_date(start, end, return = "start")
    expect_equal(object, lubridate::as_datetime("1970-01-01 21:45:00"))

    object <- assign_date(start, end, return = "end")
    expect_equal(object, lubridate::as_datetime("1970-01-02 03:20:00"))
})

test_that("assign_date() | `start_name` and `end_name` test", {
    x <- hms::parse_hm("23:00")
    y <- hms::parse_hm("01:00")
    object <- assign_date(x, y, return = "list")
    checkmate::expect_names(names(object), identical.to = c("x", "y"))
})

test_that("assign_date() | error test", {
  # Invalid values for `start` and `end`
  expect_error(assign_date(1, 2))
  expect_error(assign_date(NA, hms::parse_hm("01:00")))
  expect_error(assign_date(lubridate::as_datetime(60), NA))
  expect_error(assign_date(hms::parse_hm("11:00"), lubridate::minutes(2)))

  # `start` and `end` have different lengths
  expect_error(assign_date(hms::parse_hm("05:20"),
                           c(hms::parse_hm("22:05"), hms::parse_hm("05:25"))))

  # Invalid values for `return`, `ambiguity`, `start_name` and `end_name`
  start <- hms::parse_hms("07:25:00")
  end <- hms::parse_hms("01:05:00")
  expect_error(assign_date(start, end , return = "x"))
  expect_error(assign_date(start, end , ambiguity = "x"))
  expect_error(assign_date(start, end , start_name = 1))
  expect_error(assign_date(start, end , end_name = 1))
})
