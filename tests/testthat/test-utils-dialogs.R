test_that("dialog_line() | general test", {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            {dialog_line(
                1, space_above = TRUE, space_below = TRUE, abort = FALSE
            )}
        )
    }

    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {dialog_line(
                1, space_above = TRUE, space_below = TRUE, abort = TRUE
            )}
        )
    }

    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            require_namespace = function(...) TRUE,
            read_line = function(...) TRUE,
            {dialog_line(
                1, space_above = TRUE, space_below = TRUE, abort = FALSE
            )}
        )
    }

    expect_equal(utils::capture.output(mock()), c("", "", "[1] TRUE"))
})

test_that("dialog_line() | error test", {
    # "assert_has_length(list(...))"
    expect_error(dialog_line(
        space_above = TRUE, space_below = TRUE, abort = FALSE
    ),
    "Assertion on 'list\\(...\\)' failed"
    )

    # "checkmate::assert_flag(space_above)"
    expect_error(dialog_line(
        1, space_above = "", space_below = TRUE, abort = FALSE
    ),
    "Assertion on 'space_above' failed"
    )

    # "checkmate::assert_flag(space_below)"
    expect_error(dialog_line(
        1, space_above = TRUE, space_below = "", abort = FALSE
    ),
    "Assertion on 'space_below' failed"
    )

    # "checkmate::assert_flag(abort)"
    expect_error(dialog_line(
        1, space_above = TRUE, space_below = TRUE, abort = ""
    ),
    "Assertion on 'abort' failed"
    )
})
