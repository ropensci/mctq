test_that("sum_time() | linear test", {
  expect_equal(sum_time(
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    lubridate::dhours(1),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    lubridate::as_datetime("1970-01-01 20:00:00"),
    as.POSIXlt(lubridate::as_datetime("1970-01-01 01:00:00")),
    lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01")),
    cycle = NULL, reverse = FALSE, na_rm = FALSE
  ),
  lubridate::dhours(30)
  )

  expect_equal(sum_time(
    hms::as_hms(NA), # !
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    lubridate::dhours(1),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    lubridate::as_datetime("1970-01-01 20:00:00"),
    as.POSIXlt(lubridate::as_datetime("1970-01-01 01:00:00")),
    lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01")),
    cycle = NULL, reverse = FALSE,
    na_rm = FALSE # !
  ),
  lubridate::as.duration(NA)
  )

  expect_equal(sum_time(
    hms::as_hms(NA), # !
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    lubridate::dhours(1),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    lubridate::as_datetime("1970-01-01 20:00:00"),
    as.POSIXlt(lubridate::as_datetime("1970-01-01 01:00:00")),
    lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01")),
    cycle = NULL, reverse = FALSE,
    na_rm = TRUE # !
  ),
  lubridate::dhours(30)
  )
})

test_that("sum_time() | circular test", {
  expect_equal(sum_time(
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    lubridate::dhours(1),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    lubridate::as_datetime("1970-01-01 20:00:00"),
    as.POSIXlt(lubridate::as_datetime("1970-01-01 01:00:00")),
    lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01")),
    cycle = lubridate::ddays(), # !
    reverse = FALSE, na_rm = FALSE
  ),
  lubridate::dhours(6) # 30 - 24
  )

  expect_equal(sum_time(
    hms::as_hms(NA),
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    lubridate::dhours(1),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    lubridate::as_datetime("1970-01-01 20:00:00"),
    as.POSIXlt(lubridate::as_datetime("1970-01-01 01:00:00")),
    lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01")),
    cycle = lubridate::ddays(), reverse = FALSE, na_rm = TRUE
  ),
  lubridate::dhours(6)
  )

  expect_equal(sum_time(
    c(lubridate::dhours(-1), lubridate::dhours(-2)),
    c(lubridate::dhours(-1), lubridate::dhours(-21)),
    cycle = lubridate::ddays(), reverse = FALSE, na_rm = FALSE
  ),
  lubridate::dhours(-1)
  )

  expect_equal(sum_time(
    c(lubridate::dhours(-1), lubridate::dhours(-2)),
    c(lubridate::dhours(-1), lubridate::dhours(-21)),
    cycle = lubridate::ddays(), reverse = TRUE, na_rm = FALSE
  ),
  lubridate::dhours(23)
  )
})

test_that("vct_sum_time()| linear test", {
  expect_equal(vct_sum_time(
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    c(lubridate::dhours(1), lubridate::dhours(1)),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    c(lubridate::as_datetime("1970-01-01 20:00:00"),
      lubridate::as_datetime("1970-01-01 10:00:00")
    ),
    as.POSIXlt(
      c(lubridate::as_datetime("1970-01-01 01:00:00"),
        lubridate::as_datetime("1970-01-01 02:00:00"))
    ),
    c(lubridate::as.interval(lubridate::dhours(4), as.Date("1970-01-01")),
      lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01"))
    ),
    cycle = NULL, reverse = FALSE, na_rm = FALSE
  ),
  c(lubridate::dhours(30), lubridate::dhours(17))
  )

  expect_equal(vct_sum_time(
    c(hms::as_hms(NA), hms::as_hms(NA)), # !
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    c(lubridate::dhours(1), lubridate::dhours(1)),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    c(lubridate::as_datetime("1970-01-01 20:00:00"),
      lubridate::as_datetime("1970-01-01 10:00:00")
    ),
    as.POSIXlt(
      c(lubridate::as_datetime("1970-01-01 01:00:00"),
        lubridate::as_datetime("1970-01-01 02:00:00"))
    ),
    c(lubridate::as.interval(lubridate::dhours(4), as.Date("1970-01-01")),
      lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01"))
    ),
    cycle = NULL, reverse = FALSE,
    na_rm = FALSE
  ), # !
  c(lubridate::as.duration(NA), lubridate::as.duration(NA))
  )

  expect_equal(vct_sum_time(
    c(hms::as_hms(NA), hms::as_hms(NA)), # !
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    c(lubridate::dhours(1), lubridate::dhours(1)),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    c(lubridate::as_datetime("1970-01-01 20:00:00"),
      lubridate::as_datetime("1970-01-01 10:00:00")
    ),
    as.POSIXlt(
      c(lubridate::as_datetime("1970-01-01 01:00:00"),
        lubridate::as_datetime("1970-01-01 02:00:00"))
    ),
    c(lubridate::as.interval(lubridate::dhours(4), as.Date("1970-01-01")),
      lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01"))
    ),
    cycle = NULL, reverse = FALSE,
    na_rm = TRUE
  ), # !
  c(lubridate::dhours(30), lubridate::dhours(17))
  )
})

test_that("vct_sum_time()| circular test", {
  expect_equal(vct_sum_time(
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    c(lubridate::dhours(1), lubridate::dhours(1)),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    c(lubridate::as_datetime("1970-01-01 20:00:00"),
      lubridate::as_datetime("1970-01-01 10:00:00")
    ),
    as.POSIXlt(
      c(lubridate::as_datetime("1970-01-01 01:00:00"),
        lubridate::as_datetime("1970-01-01 02:00:00"))
    ),
    c(lubridate::as.interval(lubridate::dhours(4), as.Date("1970-01-01")),
      lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01"))
    ),
    cycle = lubridate::ddays(), # !
    reverse = FALSE, na_rm = FALSE
  ),
  c(lubridate::dhours(6), lubridate::dhours(17))
  )

  expect_equal(vct_sum_time(
    c(hms::as_hms(NA), hms::as_hms(NA)),
    c(lubridate::dhours(1), lubridate::dminutes(30)),
    c(lubridate::dhours(1), lubridate::dhours(1)),
    c(as.difftime(1, units = "hours"), as.difftime(30, units = "mins")),
    c(hms::parse_hm("02:00"), hms::parse_hm("02:00")),
    c(lubridate::as_datetime("1970-01-01 20:00:00"),
      lubridate::as_datetime("1970-01-01 10:00:00")
    ),
    as.POSIXlt(
      c(lubridate::as_datetime("1970-01-01 01:00:00"),
        lubridate::as_datetime("1970-01-01 02:00:00"))
    ),
    c(lubridate::as.interval(lubridate::dhours(4), as.Date("1970-01-01")),
      lubridate::as.interval(lubridate::dhours(1), as.Date("1970-01-01"))
    ),
    cycle = lubridate::ddays(), # !
    reverse = FALSE, na_rm = TRUE
  ),
  c(lubridate::dhours(6), lubridate::dhours(17))
  )

  expect_equal(vct_sum_time(
    c(lubridate::dhours(-24), lubridate::dhours(-1)),
    c(lubridate::dhours(-6), lubridate::dhours(-48)),
    cycle = lubridate::ddays(), reverse = FALSE, na_rm = FALSE
  ),
  c(lubridate::dhours(-6), lubridate::dhours(-1))
  )

  expect_equal(vct_sum_time(
    c(lubridate::dhours(-24), lubridate::dhours(-1)),
    c(lubridate::dhours(-6), lubridate::dhours(-48)),
    cycle = lubridate::ddays(), reverse = TRUE, na_rm = FALSE
  ),
  c(lubridate::dhours(18), lubridate::dhours(23))
  )
})

test_that("sum_time_build() | error test", {
  # lapply(out, checkmate::assert_multi_class, classes)
  expect_error(sum_time_build(
    1, vectorize = FALSE, cycle = NULL, reverse = TRUE, na_rm = FALSE
  ),
  "Assertion on 'X\\[\\[i\\]\\]' failed"
  )

  expect_error(sum_time_build(
    lubridate::hours(), vectorize = FALSE, cycle = NULL, reverse = TRUE,
    na_rm = FALSE
  ),
  "Assertion on 'X\\[\\[i\\]\\]' failed"
  )

  # checkmate::assert_flag(vectorize)
  expect_error(sum_time_build(
    hms::hms(1), vectorize = "", cycle = NULL, reverse = TRUE, na_rm = TRUE
  ),
  "Assertion on 'vectorize' failed"
  )

  # checkmate::assert_multi_class(cycle, c("numeric", "Duration"),
  #                               null.ok = TRUE)
  expect_error(sum_time_build(
    hms::hms(1), vectorize = FALSE, cycle = "", reverse = TRUE,
    na_rm = FALSE
  ),
  "Assertion on 'cycle' failed"
  )

  # checkmate::assert_number(cycle, lower = 0, null.ok = TRUE)

  expect_error(sum_time_build(
    hms::hms(1), vectorize = FALSE, cycle = -1, reverse = TRUE,
    na_rm = FALSE
  ),
  "Assertion on 'cycle' failed"
  )

  expect_error(sum_time_build(
    hms::hms(1), vectorize = FALSE, cycle = lubridate::dhours(-1),
    reverse = TRUE, na_rm = FALSE
  ),
  "Assertion on 'cycle' failed"
  )

  # checkmate::assert_flag(reverse)

  expect_error(sum_time_build(
    hms::hms(1), vectorize = FALSE, cycle = NULL, reverse = 1, na_rm = TRUE
  ),
  "Assertion on 'reverse' failed"
  )

  # checkmate::assert_flag(na_rm)
  expect_error(sum_time_build(
    hms::hms(1), vectorize = FALSE, cycle = NULL, reverse = TRUE,
    na_rm = ""
  ),
  "Assertion on 'na_rm' failed"
  )

  # if (isTRUE(vectorize) &&
  #     !(length(unique(vapply(out, length, integer(1)))) == 1)) {
  expect_error(sum_time_build(
    hms::hms(1), c(hms::hms(1), hms::hms(1)), vectorize = TRUE,
    reverse = TRUE, na_rm = TRUE
  ),
  "All values in '...' must "
  )
})
