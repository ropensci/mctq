test_that("so() | scalar test", {
  expect_equal(so(
    sprep = hms::parse_hm("23:15"), slat = lubridate::dminutes(30)
  ),
  hms::parse_hm("23:45")
  )

  expect_equal(so(
    sprep = hms::parse_hm("02:45"), slat = lubridate::dminutes(60)
  ),
  hms::parse_hm("03:45")
  )

  expect_equal(so(
    sprep = hms::parse_hm("00:00"), slat = lubridate::as.duration(NA)
  ),
  hms::as_hms(NA)
  )
})

test_that("so() | vector test", {
  expect_equal(so(
    sprep = c(hms::parse_hm("21:45"), hms::parse_hm("01:30")),
    slat = c(lubridate::dminutes(20), lubridate::dminutes(50))
  ),
  c(hms::parse_hm("22:05"), hms::parse_hm("02:20"))
  )
})

test_that("so() | error test", {
  # assert_hms(sprep, lower = hms::hms(0))
  expect_error(so(
    sprep = 1, slat = lubridate::duration(1)
  ),
  "Assertion on 'sprep' failed"
  )

  expect_error(so(
    sprep = hms::hms(-1), slat = lubridate::duration(1)
  ),
  "Assertion on 'sprep' failed"
  )

  # assert_duration(slat, lower = lubridate::duration(0))
  expect_error(so(
    sprep = hms::hms(1), slat = 1
  ),
  "Assertion on 'slat' failed"
  )

  expect_error(so(
    sprep = hms::hms(1), slat = lubridate::duration(-1)
  ),
  "Assertion on 'slat' failed"
  )

  # assert_identical(sprep, slat, type = "length")
  expect_error(so(
    sprep = hms::hms(1),
    slat = c(lubridate::duration(1), lubridate::duration(1))
  ))
})
