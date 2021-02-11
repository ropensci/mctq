test_that("raw_data() | general test", {
    checkmate::expect_character(raw_data(), any.missing = FALSE,
                                all.missing = FALSE)
    checkmate::expect_character(raw_data(raw_data()[1]), len = 1,
                                any.missing = FALSE, all.missing = FALSE)

    expect_equal(raw_data(NULL), dir(system.file("extdata", package = "mctq")))
    expect_equal(raw_data(raw_data()[1]),
                 system.file("extdata", raw_data()[1], package = "mctq",
                             mustWork = TRUE))
})

test_that("raw_data() | error test", {
    # Invalid value for `file`
    expect_error(raW_data(file = 1))
})
