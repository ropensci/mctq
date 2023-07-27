test_that("sd_overall() | scalar test", {
  expect_equal(sd_overall(
    sd_w = lubridate::dhours(6.5), sd_f = lubridate::dhours(11), n_w = 4,
    n_f = 2
  ),
  lubridate::as.duration(stats::weighted.mean(
    c(lubridate::dhours(6.5), lubridate::dhours(11)), c(4, 2)
  ))
  )

  expect_equal(sd_overall(
    sd_w = lubridate::dhours(8.5), sd_f = lubridate::dhours(6.5), n_w = 6,
    n_f = 1
  ),
  lubridate::as.duration(stats::weighted.mean(
    c(lubridate::dhours(8.5), lubridate::dhours(6.5)), c(6, 1)
  ))
  )

  expect_equal(sd_overall(
    sd_w = lubridate::as.duration(NA), sd_f = lubridate::dhours(7), n_w = 2,
    n_f = 2
  ),
  lubridate::as.duration(NA)
  )
})

test_that("sd_overall() | vector test", {
  sd_overall(
    sd_w = c(lubridate::dhours(3.9), sd_f = lubridate::dhours(5)),
    c(lubridate::dhours(12), lubridate::dhours(NA)),
    n_w = c(6, 10), n_f = c(1, 7)
  ) |>
    expect_equal(
      c(
        lubridate::duration(stats::weighted.mean(
          c(lubridate::dhours(3.9), lubridate::dhours(12)), c(6, 1)
        )),
        lubridate::as.duration(NA)
      )
    )

  sd_overall(
    sd_w = c(lubridate::dhours(4.5), lubridate::dhours(7)),
    sd_f = c(lubridate::dhours(10.5), lubridate::dhours(3)),
    n_w = c(4, 5),
    n_f = c(1, 2)
  ) |>
    expect_equal(
      c(
        lubridate::duration(stats::weighted.mean(
          c(lubridate::dhours(4.5), lubridate::dhours(10.5)), c(4, 1)
        )),
        lubridate::duration(stats::weighted.mean(
          c(lubridate::dhours(7), lubridate::dhours(3)), c(5, 2)
        ))
      )
    )
})

test_that("sd_overall() | error test", {
  # assert_duration(sd_w, lower = lubridate::duration(0))
  expect_error(sd_overall(
    sd_w = 1, sd_f = lubridate::duration(1), n_w = 1, n_f = 1
  ),
  "Assertion on 'sd_w' failed"
  )

  # assert_duration(sd_f, lower = lubridate::duration(0))
  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = 1, n_w = 1, n_f = 1
  ),
  "Assertion on 'sd_f' failed"
  )

  # assert_numeric_(n_w)
  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), n_w = "a",
    n_f = 1
  ),
  "Assertion on 'n_w' failed"
  )

  # checkmate::assert_integerish(n_w, lower = 0)
  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), n_w = 1.5,
    n_f = 1
  ),
  "Assertion on 'n_w' failed"
  )

  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), n_w = -1,
    n_f = 1
  ),
  "Assertion on 'n_w' failed"
  )

  # assert_numeric_(n_f)
  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), n_w = 1,
    n_f = "a"
  ),
  "Assertion on 'n_f' failed"
  )

  # checkmate::assert_integerish(n_f, lower = 0)
  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), n_w = 1,
    n_f = 1.5
  ),
  "Assertion on 'n_f' failed"
  )

  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1), n_w = 1,
    n_f = -1
  ),
  "Assertion on 'n_f' failed"
  )

  # assert_identical(sd_w, sd_f, n_w, n_f, type = "length")
  expect_error(sd_overall(
    sd_w = lubridate::duration(1), sd_f = lubridate::duration(1),
    n_w = c(1, 1), n_f = 1
  ))
})
