test_that("sjl() | scalar test", {
  expect_equal(sjl(
    msw = hms::parse_hm("02:30"), msf = hms::parse_hm("04:30"),
    abs = TRUE, method = "shorter"
  ),
  lubridate::dhours(2)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("23:00"), msf = hms::parse_hm("02:00"),
    abs = TRUE, method = "shorter"
  ),
  lubridate::dhours(3)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("05:00"), msf = hms::parse_hm("03:00"),
    abs = FALSE, method = "shorter"
  ),
  lubridate::dhours(-2)
  )

  expect_equal(sjl(
    msw = hms::as_hms(NA), msf = hms::parse_hm("00:00"),
    abs = FALSE, method = "shorter"
  ),
  lubridate::as.duration(NA)
  )
})

test_that("sjl() | vector test", {
  expect_equal(sjl(
    msw = c(hms::parse_hm("11:00"), hms::parse_hm("22:00")),
    msf = c(hms::parse_hm("18:30"), hms::parse_hm("17:30")),
    abs = FALSE, method = "shorter"
  ),
  c(lubridate::dhours(7.5), lubridate::dhours(-4.5))
  )
})

test_that("sjl() | `method` test", {
  expect_equal(sjl(
    msw = hms::parse_hm("08:00"), msf = hms::parse_hm("12:00"),
    abs = FALSE, method = "difference"
  ),
  lubridate::dhours(4)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("08:00"), msf = hms::parse_hm("12:00"),
    abs = FALSE, method = "shorter"
  ),
  lubridate::dhours(4)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("08:00"), msf = hms::parse_hm("12:00"),
    abs = FALSE, method = "longer"
  ),
  lubridate::dhours(-20)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("12:00"), msf = hms::parse_hm("08:00"),
    abs = FALSE, method = "difference"
  ),
  lubridate::dhours(-4)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("12:00"), msf = hms::parse_hm("08:00"),
    abs = FALSE, method = "shorter"
  ),
  lubridate::dhours(-4)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("12:00"), msf = hms::parse_hm("08:00"),
    abs = FALSE, method = "longer"
  ),
  lubridate::dhours(20)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("23:00"), msf = hms::parse_hm("01:00"),
    abs = FALSE, method = "difference"
  ),
  lubridate::dhours(-22)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("23:00"), msf = hms::parse_hm("01:00"),
    abs = FALSE, method = "shorter"
  ),
  lubridate::dhours(2)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("23:00"), msf = hms::parse_hm("01:00"),
    abs = FALSE, method = "longer"
  ),
  lubridate::dhours(-22)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("01:00"), msf = hms::parse_hm("23:00"),
    abs = FALSE, method = "difference"
  ),
  lubridate::dhours(22)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("01:00"), msf = hms::parse_hm("23:00"),
    abs = FALSE, method = "shorter"
  ),
  lubridate::dhours(-2)
  )

  expect_equal(sjl(
    msw = hms::parse_hm("01:00"),  msf = hms::parse_hm("23:00"),
    abs = FALSE, method = "longer"
  ),
  lubridate::dhours(22)
  )
})

test_that("sjl() | error test", {
  # assert_hms(msw, lower = hms::hms(0))
  expect_error(sjl(
    msw = 1, msf = hms::hms(1), abs = TRUE, method = "shorter"
  ),
  "Assertion on 'msw' failed"
  )

  expect_error(sjl(
    msw = hms::hms(-1), msf = hms::hms(1), abs = TRUE, method = "shorter"
  ),
  "Assertion on 'msw' failed"
  )


  # assert_hms(msf, lower = hms::hms(0))
  expect_error(sjl(
    msw = hms::hms(1), msf = 1, abs = TRUE, method = "shorter"
  ),
  "Assertion on 'msf' failed"
  )

  expect_error(sjl(
    msw = hms::hms(1), msf = hms::hms(-1), abs = TRUE, method = "shorter"
  ),
  "Assertion on 'msf' failed"
  )

  # assert_identical(msw, msf, type = "length")
  expect_error(sjl(
    msw = hms::hms(1), msf = c(hms::hms(1), hms::hms(1)), abs = TRUE,
    method = "shorter"
  ))

  #
  expect_error(sjl(
    msw = hms::hms(1), msf = hms::hms(1), abs = "", method = "shorter"
  ),
  "Assertion on 'abs' failed"
  )

  #
  expect_error(sjl(
    msw = hms::hms(1), msf = hms::hms(1), abs = TRUE, 1
  ),
  "Assertion on 'method' failed"
  )
})

test_that("sjl_rel() | general test", {
  expect_equal(sjl_rel(
    msw = hms::parse_hm("10:00"), msf = hms::parse_hm("12:00"),
    method = "shorter"
  ),
  lubridate::dhours(2)
  )

  expect_equal(sjl_rel(
    msw = hms::parse_hm("03:30"), msf = hms::parse_hm("03:00"),
    method = "shorter"
  ),
  lubridate::dhours(-0.5)
  )

  expect_equal(sjl_rel(
    msw = hms::parse_hm("22:00"), msf = hms::parse_hm("03:00"),
    method = "shorter"
  ),
  lubridate::dhours(5)
  )
})
