test_that("assign_date() | default test", {
  bt_w <- hms::parse_hms("02:10:00")
  sprep_w <- hms::parse_hms("05:30:00")
  object <- assign_date(bt_w, sprep_w)
  expected <- lubridate::interval(
    lubridate::as_datetime("0000-01-01 02:10:00"),
    lubridate::as_datetime("0000-01-01 05:30:00"))
  expect_equal(object, expected)
})

test_that("assign_date() | `ambiguity` test", {
    start <- lubridate::as_datetime("1985-01-15 12:00:00")
    end <- lubridate::as_datetime("2020-09-10 12:00:00")
    object <- assign_date(start, end, ambiguity = 24)
    expected <- lubridate::interval(
      lubridate::as_datetime("0000-01-01 12:00:00"),
      lubridate::as_datetime("0000-01-02 12:00:00"))
    expect_equal(object, expected)
})

test_that("assign_date() | `return` test", {
    bt_w <- hms::parse_hms("22:15:00")
    sprep_w <- hms::parse_hms("00:00:00")
    object <- assign_date(bt_w, sprep_w, return = "list")
    expected <- list(bt_w = lubridate::as_datetime("0000-01-01 22:15:00"),
                     sprep_w = lubridate::as_datetime("0000-01-02 00:00:00"))
    expect_equal(object, expected)

    so_w <- hms::parse_hms("01:10:00")
    se_w <- hms::parse_hms("11:45:00")
    object <- assign_date(so_w, se_w, return = "list")
    expected <- list(so_w = lubridate::as_datetime("0000-01-01 01:10:00"),
                     se_w = lubridate::as_datetime("0000-01-01 11:45:00"))
    expect_equal(object, expected)

    start <- lubridate::parse_date_time("01:10:00", "HMS")
    end <- lubridate::parse_date_time("11:45:00", "HMS")
    object <- assign_date(start, end, return = "start")
    expect_equal(object, lubridate::as_datetime("0000-01-01 01:10:00"))
})

test_that("assign_date() | `start_name` and `end_name` test", {
    x <- hms::parse_hm("23:00")
    y <- hms::parse_hm("01:00")
    object <- assign_date(x, y, return = "list")
    checkmate::expect_names(names(object), identical.to = c("x", "y"))
})
