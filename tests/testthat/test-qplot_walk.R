test_that("qplot_walk() | general test", {
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # is_interactive <- mctq:::is_interactive
    # dialog_line <- mctq:::dialog_line
    # qplot_walk <- mctq::qplot_walk

    # "if (is.atomic(data))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5)[[1]]))
            )
    }

    # mock()
    expect_s3_class(mock(), "ggplot")

    # "if ("xlab" %in% names(list(...)))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5)[[1]],
                                        xlab = "test"))
            )
    }

    # mock()
    expect_s3_class(mock(), "ggplot")

    # "if (is.data.frame(data))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5)))
            )
    }

    # mock()
    expect_equal(mock(), NULL)

    # "if (!is.null(pattern))"
    # "x <- transform(data[[i]], midday_change)"
    # "if ("xlab" %in% names(list(...)))"
    data <- data.frame(a = hms::parse_hm("23:00"), b = lubridate::dhours(1))
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line = function(...) TRUE,
            suppressMessages(qplot_walk(data, pattern = ".+", xlab = "test"))
            )
    }

    # mock()
    expect_equal(mock(), NULL)
})

test_that("qplot_walk() | error test", {
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # is_interactive <- mctq:::is_interactive
    # dialog_line <- mctq:::dialog_line
    # qplot_walk <- mctq::qplot_walk

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5)))
            )
    }

    # mock()
    expect_error(mock(), "This function can only be used in interactive mode.")

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5), x = 1))
            )
    }

    # mock()
    expect_error(mock(), "'x', 'y' and `data` are reserved arguments for .")

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5),
                                        cols = "Sepal.Length",
                                        pattern = "\\."))
            )
    }

    # mock()
    expect_error(mock(), "'cols' and 'pattern' can't both have values. ")

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(list(1))))
    }

    # mock()
    expect_error(mock(), "'data' must be an 'atomic' object or a data frame.")

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5),
                                        pattern = "^999$"))
            )
    }

    # mock()
    expect_error(mock(), "None match was found in 'names\\(data\\)'.")

    ignore <- unique(vapply(utils::head(datasets::iris, 5),
                            function(x) class(x)[1], character(1)))
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            suppressMessages(qplot_walk(utils::head(datasets::iris, 5),
                                        ignore = ignore))
            )
    }

    # mock()
    expect_error(mock(), "You can't ignore all variables in 'cols' or in ")
})

test_that("qplot_walk() | warning test", {
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # is_interactive <- mctq:::is_interactive
    # dialog_line <- mctq:::dialog_line
    # qplot_walk <- mctq::qplot_walk

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            qplot_walk(utils::head(datasets::iris, 5)[[1]]))
    }

    # mock()
    suppressMessages(
        expect_message(mock(), "'data' is 'atomic'. All other arguments, ")
        )

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line = function(...) TRUE,
            qplot_walk(utils::head(datasets::iris, 5), ignore = "factor"))
    }

    # mock()
    suppressMessages(
        expect_message(mock(), "'Species' will be ignored due to the settings ")
        )
})
