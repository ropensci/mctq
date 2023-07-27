test_that("pretty_mctq() | general test", {
  data <- data.frame(
    a = 1,
    b = lubridate::duration(1.12345),
    c = hms::hms(1.12345)
  )

  object <- pretty_mctq(data = data, round = TRUE, hms = TRUE)

  expect_true(hms::is_hms(object$b), TRUE)
  expect_true(hms::is_hms(object$c), TRUE)

  object <- pretty_mctq(data, round = TRUE, hms = FALSE)

  expect_equal(object$b, lubridate::duration(as.integer(data$b)))
  expect_equal(object$c, hms::hms(as.integer(data$c)))
})

test_that("pretty_mctq() | error test", {
  # checkmate::assert_data_frame(data)
  expect_error(pretty_mctq(
    data = 1, round = TRUE, hms = TRUE
  ),
  "Assertion on 'data' failed"
  )

  # checkmate::assert_flag(round)
  expect_error(pretty_mctq(
    data = datasets::iris, round = "a", hms = TRUE
  ),
  "Assertion on 'round' failed"
  )

  # checkmate::assert_flag(hms)
  expect_error(pretty_mctq(
    data = datasets::iris, round = TRUE, hms = ""
  ),
  "Assertion on 'hms' failed"
  )
})
