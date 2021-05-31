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
    expect_error(dialog_line(1, combined_styles = 1),
                 "Assertion on 'combined_styles' failed")
    expect_error(dialog_line(1, space_above = ""),
                 "Assertion on 'space_above' failed")
    expect_error(dialog_line(1, space_below = ""),
                 "Assertion on 'space_below' failed")
    expect_error(dialog_line(1, abort = ""),
                 "Assertion on 'abort' failed")
})

test_that("alert() | general test", {
    expect_equal(alert(1, abort = TRUE), NULL)

    expect_message(alert(1), "1")
    expect_message(alert(c(1, 2)), "12")
    expect_message(alert(1, 2), "12")

    expect_warning(alert(1, type = "warning"), "1")

    expect_equal(getElement(evaluate_promise(
        alert(1, combined_styles = NULL, type = "cat")),
        "output"),
        "1")

    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # require_namespace <- mctq:::require_namespace

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) TRUE,
            alert("test"))
    }

    # mock()
    expect_message(mock())
})

test_that("alert() | error test", {
    expect_error(alert(), "Assertion on 'list\\(...\\)' failed")
    expect_error(alert(1, combined_styles = ""),
                 "Assertion on 'combined_styles' failed")
    expect_error(alert(1, type = ""),
                 "Assertion on 'type' failed")
    expect_error(alert(1, abort = ""),
                 "Assertion on 'abort' failed")
})

test_that("crayonize() | general test", {
    expect_equal(crayonize(1, abort = TRUE), NULL)
    expect_equal(crayonize("test", combined_styles = NULL), "test")
    expect_equal(crayonize("test", combined_styles = "red"),
                 crayon::red("test"))
})

test_that("crayonize() | error test", {
    expect_error(crayonize(), "Assertion on 'list\\(...\\)' failed")
    expect_error(crayonize(1, combined_styles = ""),
                 "Assertion on 'combined_styles' failed")
    expect_error(crayonize(1, abort = ""),
                 "Assertion on 'abort' failed")
})
