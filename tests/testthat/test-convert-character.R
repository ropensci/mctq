test_that("convert.character() | convert test", {
    x <- "1"
    quiet <- TRUE

    expect_equal(convert(x, "character", quiet = quiet), "1")
    expect_equal(convert(x, "integer", quiet = quiet), 1L)
    expect_equal(convert(x, "double", quiet = quiet), 1)
    expect_equal(convert(x, "numeric", quiet = quiet), 1)
})

test_that("convert.character() | transform test", {
    x <- "1"

    object <- convert(x, "numeric", input_unit = "H", output_unit = "M",
                      quiet = TRUE)
    expect_identical(object, 60)

    object <- convert(x, "hms", orders = "H")
    expect_identical(object, hms::parse_hm("01:00"))
})

test_that("convert.character() | error test", {
    x <- "1"

    # Invalid values for `class, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", tz = "", quiet = ""))

    # "You must assign a value to 'orders' or 'input_unit' [...]"
    classes <- c("Duration", "Period", "difftime", "hms", "Date", "POSIXct",
                 "POSIXlt")
    for (i in classes) expect_error(convert(x, i))
})
