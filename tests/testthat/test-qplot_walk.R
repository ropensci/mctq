# Don't forget to run devtools::load_all(".") and uncomment the variables
# before trying to run the tests interactively.

test_that("qplot_walk() | general test", {
    data <- utils::head(datasets::iris, 5)

    # is_interactive <- mctq:::is_interactive
    # is_namespace_loaded <- mctq:::is_namespace_loaded
    # dialog_line <- mctq:::dialog_line
    # qplot_walk <- mctq::qplot_walk

    # "if (is.atomic(data))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            suppressWarnings(qplot_walk(data[[1]])))
    }

    # x <- mock()
    expect_s3_class(mock(), "ggplot")

    # "if ("xlab" %in% names(list(...)))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            suppressWarnings(qplot_walk(data[[1]], xlab = "test")))
    }

    # x <- mock()
    expect_s3_class(mock(), "ggplot")

    # "if (is.data.frame(data))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            dialog_line = function(...) TRUE,
            qplot_walk(data))
    }

    # x <- mock()
    expect_equal(mock(), NULL)

    # "if (!is.null(pattern))"
    # "x <- transform(data[[i]], midday_change)"
    # "if ("xlab" %in% names(list(...)))"
    data <- data.frame(a = hms::parse_hm("23:00"), b = lubridate::dhours(1))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            dialog_line = function(...) TRUE,
            qplot_walk(data, pattern = ".+", xlab = "test"))
    }

    # x <- mock()
    expect_equal(mock(), NULL)
})

test_that("qplot_walk() | error test", {
    data <- utils::head(datasets::iris, 5)

    # is_interactive <- mctq:::is_interactive
    # is_namespace_loaded <- mctq:::is_namespace_loaded
    # dialog_line <- mctq:::dialog_line
    # qplot_walk <- mctq::qplot_walk

    # "This function can only be used in interactive mode"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            qplot_walk(data))
    }

    # mock()
    expect_error(mock())

    # "This function requires the `grDevices` and `ggplot2` [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) FALSE,
            qplot_walk(data))
    }

    # mock()
    expect_error(mock())

    # "`x`, `y` and `data` are reserved arguments for `qplot_walk()`"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            qplot_walk(data, x = 1))
    }

    # mock()
    expect_error(mock())

    # "`cols` and `pattern` can't both have values [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            qplot_walk(data, cols = "Sepal.Length", pattern = "\\."))
    }

    # mock()
    expect_error(mock())

    # "`cols` and `pattern` can't both have values [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            qplot_walk(list(1)))
    }

    # mock()
    expect_error(mock())

    # "None match was found in `names(data)`"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            qplot_walk(data, pattern = "^999$"))
    }

    # mock()
    expect_error(mock())

    # "You can't ignore all variables in `cols` or in `data` [...]"
    ignore <- unique(vapply(data, function(x) class(x)[1], character(1)))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            qplot_walk(data, ignore = ignore))
    }

    # mock()
    expect_error(mock())
})

test_that("qplot_walk() | warning test", {
    data <- utils::head(datasets::iris, 5)

    # is_interactive <- mctq:::is_interactive
    # is_namespace_loaded <- mctq:::is_namespace_loaded
    # dialog_line <- mctq:::dialog_line
    # qplot_walk <- mctq::qplot_walk

    # "`data` is atomic. All other arguments, except `...` and [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            qplot_walk(data[[1]]))
    }

    # mock()
    expect_warning(mock())

    # "inline_collapse(match), " will be ignored due to the [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            is_namespace_loaded = function(...) TRUE,
            dialog_line = function(...) TRUE,
            qplot_walk(data, ignore = "factor"))
    }

    # mock()
    expect_warning(mock())
})
