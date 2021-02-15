test_that("convert.data.frame() | convert test", {
    x <- data.frame(a = c(1, 1), b = c("1", "1"))
    quiet <- TRUE

    object <- convert(x, "hms", cols = "a", input_unit = "H", quiet = quiet)
    expected <- data.frame(
        a = c(hms::parse_hm("01:00"), hms::parse_hm("01:00")),
        b = c("1", "1"))
    expect_equal(object, expected)

    object <- convert(x, "duration", where = is.character, input_unit = "H",
                      quiet = quiet)
    expected <- data.frame(
        a = c(1, 1),
        b = c(lubridate::dhours(), lubridate::dhours()))
    expect_equal(object, expected)
})

test_that("convert.data.frame() | error test", {
    x <- data.frame(a = c(1, 1), b = c("1", "1"))

    # Invalid values for `class, `where`, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", where = 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", where = is.numeric, tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", where = is.numeric, tz = "", quiet = ""))

    # "'cols' and 'where' cannot both be 'NULL'."
    expect_error(convert(x, "hms"))
})
