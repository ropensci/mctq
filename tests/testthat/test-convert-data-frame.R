test_that("convert.data.frame() | convert test", {
    expect_equal(convert(dplyr::tibble(a = c(1, 1), b = c("1", "1")),
                         "hms", cols = "a", input_unit = "H", quiet = TRUE),
                 dplyr::tibble(
                     a = c(hms::parse_hm("01:00"), hms::parse_hm("01:00")),
                     b = c("1", "1")))

    expect_equal(convert(dplyr::tibble(a = c(1, 1), b = c("1", "1")),
                         "duration", where = is.character, input_unit = "H",
                         quiet = TRUE),
                 dplyr::tibble(
                     a = c(1, 1),
                     b = c(lubridate::dhours(), lubridate::dhours())))
})

test_that("convert.data.frame() | error test", {
    expect_error(convert(data.frame(), 1, tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert(data.frame(), "hms", where = 1, tz = "", quiet = TRUE),
                 "Assertion on 'where' failed")
    expect_error(convert(data.frame(), "hms", where = is.numeric, tz = 1,
                         quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert(data.frame(), "hms", where = is.numeric, tz = "",
                         quiet = ""),
                 "Assertion on 'quiet' failed")
    expect_error(convert(data.frame(), "hms"),
                 "'cols' and 'where' cannot both be 'NULL'.")
})
