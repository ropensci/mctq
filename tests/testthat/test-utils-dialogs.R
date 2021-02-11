# Don't forget to run devtools::load_all(".") and uncomment the variables
# before trying to run the tests interactively.

test_that("dialog_line() | general test", {
    # is_interactive <- mctq:::is_interactive
    # is_namespace_loaded <- mctq:::is_namespace_loaded
    # read_line <- mctq:::read_line
    # dialog_line <- mctq:::dialog_line

    expect_equal(dialog_line(abort = TRUE), 999)

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            dialog_line())
    }

    # x <- mock()
    expect_equal(mock(), 999)

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            read_line = function(...) TRUE,
            dialog_line(space_above = TRUE, space_below = TRUE))
    }

    # x <- mock()
    expect_equal(utils::capture.output(mock()), c("", "", "[1] TRUE"))
})

test_that("dialog_line() | error test", {
    # Invalid values for `line` and `abort`
    expect_error(dialog_line(line = ""))
    expect_error(dialog_line(space_above = ""))
    expect_error(dialog_line(space_below = ""))
    expect_error(dialog_line(abort = ""))
})
