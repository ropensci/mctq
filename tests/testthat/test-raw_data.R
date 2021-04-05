test_that("raw_data() | general test", {
    checkmate::expect_character(raw_data(), any.missing = FALSE,
                                all.missing = FALSE)
    checkmate::expect_string(raw_data(raw_data()[1]), na.ok = FALSE)
})

test_that("raw_data() | error test", {
    # Invalid value for `file`
    expect_error(raW_data(file = 1))
})
