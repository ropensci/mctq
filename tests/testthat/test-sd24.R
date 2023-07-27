test_that("sd24() | scalar test", {
  expect_equal(sd24(
    sd = lubridate::dhours(5.5), napd = lubridate::dhours(2),
    nap = TRUE
  ),
  lubridate::dhours(7.5)
  )

  expect_equal(sd24(
    sd = lubridate::dhours(7.5), napd = lubridate::dhours(0.5),
    nap = TRUE
  ),
  lubridate::dhours(8)
  )

  expect_equal(sd24(
    sd = lubridate::dhours(9), napd = lubridate::as.duration(NA),
    nap = FALSE
  ),
  lubridate::dhours(9)
  )

  expect_equal(sd24(
    sd = lubridate::as.duration(NA), napd = lubridate::dhours(1.45),
    nap = TRUE
  ),
  lubridate::as.duration(NA)
  )
})

test_that("sd24() | vector test", {
  expect_equal(sd24(
    sd = c(lubridate::dhours(7.5), lubridate::dhours(3)),
    napd = c(lubridate::dhours(3.4), lubridate::dhours(2.5)),
    nap = c(FALSE, TRUE)
  ),
  c(lubridate::dhours(7.5), lubridate::dhours(5.5))
  )
})

test_that("sd24() | error test", {
  # assert_duration(sd, lower = lubridate::duration(0))
  expect_error(sd24(
    sd = 1, napd = lubridate::as.duration(1), nap = TRUE
  ),
  "Assertion on 'sd' failed"
  )

  expect_error(sd24(
    sd = lubridate::as.duration(-1), napd = lubridate::as.duration(1),
    nap = TRUE
  ),
  "Assertion on 'sd' failed"
  )

  # assert_duration(napd, lower = lubridate::duration(0))
  expect_error(sd24(
    sd = lubridate::as.duration(1), napd = 1, nap = TRUE
  ),
  "Assertion on 'napd' failed"
  )

  expect_error(sd24(
    sd = lubridate::as.duration(1), napd = lubridate::as.duration(-1),
    nap = TRUE
  ),
  "Assertion on 'napd' failed"
  )

  # checkmate::assert_logical(nap)
  expect_error(sd24(
    sd = lubridate::as.duration(1), napd = lubridate::as.duration(1),
    nap = ""
  ),
  "Assertion on 'nap' failed"
  )

  # assert_identical(sd, napd, nap, type = "length")
  expect_error(
    sd24(
      sd = lubridate::as.duration(1),
      napd = c(lubridate::as.duration(1), lubridate::as.duration(1)),
      nap = TRUE
    )
  )
})
