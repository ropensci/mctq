test_that("le_week() | scalar test", {
  expect_equal(le_week(
    le_w = lubridate::dhours(2.3), le_f = lubridate::dhours(4.5), wd = 5
  ),
  lubridate::as.duration(stats::weighted.mean(
    c(lubridate::dhours(2.3), lubridate::dhours(4.5)),  c(5, 2)
  ))
  )

  expect_equal(le_week(
    le_w = lubridate::dhours(5.25), le_f = lubridate::dhours(1.25), wd = 3
  )
  ,
  lubridate::as.duration(stats::weighted.mean(
    c(lubridate::dhours(5.25), lubridate::dhours(1.25)), c(3, 4)
  ))
  )

  expect_equal(le_week(
    le_w = lubridate::as.duration(NA), le_f = lubridate::dhours(2.35),
    wd = 2
  ),
  lubridate::as.duration(NA)
  )
})

test_that("le_week() | vector test", {
  expect_equal(le_week(
    le_w = c(lubridate::dhours(2.4), lubridate::dhours(0.5)),
    le_f = c(lubridate::dhours(2.4), lubridate::dhours(NA)),
    wd = c(3, 7)
  ),
  c(lubridate::duration(stats::weighted.mean(
    c(lubridate::dhours(2.4), lubridate::dhours(2.4)), c(3, 4)
  )),
  lubridate::as.duration(NA))
  )

  expect_equal(le_week(
    le_w = c(lubridate::dhours(1.8), lubridate::dhours(5.4)),
    le_f = c(lubridate::dhours(6.7), lubridate::dhours(1.2)),
    wd = c(5, 6)
  ),
  c(lubridate::duration(stats::weighted.mean(
    c(lubridate::dhours(1.8), lubridate::dhours(6.7)), c(5, 2)
  )),
  lubridate::duration(stats::weighted.mean(
    c(lubridate::dhours(5.4), lubridate::dhours(1.2)), c(6, 1)
  ))
  )
  )
})

test_that("le_week() | error test", {
  # assert_duration(le_w, lower = lubridate::duration(0))
  expect_error(le_week(
    le_w = 1, le_f = lubridate::duration(1), wd = 1
  ),
  "Assertion on 'le_w' failed"
  )

  expect_error(le_week(
    le_w = lubridate::duration(-1), le_f = lubridate::duration(1), wd = 1
  ),
  "Assertion on 'le_w' failed"
  )

  # assert_duration(le_f, lower = lubridate::duration(0))
  expect_error(le_week(
    le_w = lubridate::duration(1), le_f = 1, wd = 1
  ),
  "Assertion on 'le_f' failed"
  )

  expect_error(le_week(
    le_w = lubridate::duration(1), le_f = lubridate::duration(-1), wd = 1
  ),
  "Assertion on 'le_f' failed"
  )

  # assert_numeric_(wd)
  expect_error(le_week(
    le_w = lubridate::duration(1), le_f = lubridate::duration(1), wd = "a"
  ),
  "Assertion on 'wd' failed"
  )

  # checkmate::assert_integerish(wd, lower = 0, upper = 7)
  expect_error(le_week(
    le_w = lubridate::duration(1), le_f = lubridate::duration(1), wd = 1.5
  ),
  "Assertion on 'wd' failed"
  )

  expect_error(le_week(
    le_w = lubridate::duration(1), le_f = lubridate::duration(1), wd = -1
  ),
  "Assertion on 'wd' failed"
  )

  expect_error(le_week(
    le_w = lubridate::duration(1), le_f = lubridate::duration(1), wd = 8
  ),
  "Assertion on 'wd' failed"
  )

  # assert_identical(le_w, le_f, wd, type = "length")
  le_week(
    le_w = lubridate::duration(1),
    le_f = lubridate::duration(1),
    wd = c(1, 1)
  ) |>
    expect_error()
})
