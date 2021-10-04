test_that("dialog_line() | general test", {
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # is_interactive <- mctq:::is_interactive
    # require_namespace <- mctq:::require_namespace
    # read_line <- mctq:::read_line

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            dialog_line(1))
    }

    # mock()
    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line(1, abort = TRUE))
    }

    # mock()
    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            require_namespace = function(...) TRUE,
            read_line = function(...) TRUE,
            dialog_line(1, combined_styles = "red", space_above = TRUE,
                        space_below = TRUE))
    }

    # mock()
    expect_equal(utils::capture.output(mock()), c("", "", "[1] TRUE"))
})

test_that("dialog_line() | error test", {
    expect_error(dialog_line(), "Assertion on 'list\\(...\\)' failed")
    expect_error(dialog_line(1, space_above = ""),
                 "Assertion on 'space_above' failed")
    expect_error(dialog_line(1, space_below = ""),
                 "Assertion on 'space_below' failed")
    expect_error(dialog_line(1, abort = ""),
                 "Assertion on 'abort' failed")
})
