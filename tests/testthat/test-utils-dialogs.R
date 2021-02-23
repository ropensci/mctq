# Don't forget to run devtools::load_all(".") and uncomment the variables
# before trying to run the tests interactively.

test_that("dialog_line() | general test", {
    # is_interactive <- mctq:::is_interactive
    # is_namespace_loaded <- mctq:::is_namespace_loaded
    # read_line <- mctq:::read_line
    # dialog_line <- mctq:::dialog_line

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            dialog_line(1))
    }

    # x <- mock()
    expect_equal(mock(), 999)

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line(1, abort = TRUE))
    }

    # x <- mock()
    expect_equal(mock(), 999)

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            read_line = function(...) TRUE,
            dialog_line(1, space_above = TRUE, space_below = TRUE))
    }

    # x <- mock()
    expect_equal(utils::capture.output(mock()), c("", "", "[1] TRUE"))
})

test_that("dialog_line() | error test", {
    # Invalid values for `...`, `combined_styles`, `space_above`,
    # `space_below`, and `abort`
    expect_error(dialog_line())
    expect_error(dialog_line(1, combined_styles = ""))
    expect_error(dialog_line(1, space_above = ""))
    expect_error(dialog_line(1, space_below = ""))
    expect_error(dialog_line(1, abort = ""))
})

test_that("alert() | general test", {
    # is_namespace_loaded <- mctq:::is_namespace_loaded

    expect_equal(alert(1, abort = TRUE), NULL)
    expect_message(alert(1))
    expect_message(alert(c(1, 2)))
    expect_message(alert(1, 2))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_namespace_loaded = function(...) TRUE,
            alert("test"))
    }

    # x <- mock()
    expect_message(mock())
})

test_that("alert() | error test", {
    # Invalid values for `...`, `combined_styles`, and `abort`
    expect_error(alert())
    expect_error(alert(1, combined_styles = ""))
    expect_error(alert(1, abort = ""))
})
