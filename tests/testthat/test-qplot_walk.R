test_that("qplot_walk() | general test", {
    # "if (is.atomic(data))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(data = utils::head(datasets::iris, 5)[[1]])}
            )
    }

    expect_s3_class(shush(mock()), "ggplot")

    # "if ("xlab" %in% names(list(...)))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(
                data = utils::head(datasets::iris, 5)[[1]],
                xlab = "test"
            )}
        )
    }

    expect_s3_class(shush(mock()), "ggplot")

    # "if (is.data.frame(data))"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line = function(...) TRUE,
            {qplot_walk(data = utils::head(datasets::iris, 5))}
            )
    }

    expect_equal(shush(mock()), NULL)

    # "if (!is.null(pattern))"
    # "x <- transform(data[[i]], midday_change)"
    # "if ("xlab" %in% names(list(...)))"
    data <- data.frame(a = hms::parse_hm("23:00"), b = lubridate::dhours(1))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line = function(...) TRUE,
            {qplot_walk(data = data, pattern = ".+", xlab = "test")}
            )
    }

    expect_equal(shush(mock()), NULL)
})

test_that("qplot_walk() | error test", {
    # if (!is_interactive()) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            {qplot_walk(data = utils::head(datasets::iris, 5))}
            )
    }

    expect_error(
        shush(mock()),
        "This function can only be used in interactive mode."
        )

    # if (any(c("x", "y", "data") %in% names(list(...)))) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(data = utils::head(datasets::iris, 5), x = 1)}
            )
    }

    expect_error(
        shush(mock()),
        "'x', 'y' and `data` are reserved arguments for ."
    )

    # if (!is.null(cols) && !is.null(pattern)) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(
                data = utils::head(datasets::iris, 5),
                cols = "Sepal.Length", pattern = "\\."
            )}
        )
    }

    expect_error(shush(mock()), "'cols' and 'pattern' can't both have values. ")

    # if (!is.atomic(data) && !is.data.frame(data)) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(data = list(1))}
        )
    }

    expect_error(
        shush(mock()),
        "'data' must be an 'atomic' object or a data frame."
    )

    # if (!is.null(pattern)) { ||| if (length(cols) == 0) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(
                data = utils::head(datasets::iris, 5),
                pattern = "^999$"
            )}
        )
    }

    expect_error(shush(mock()), "None match was found in 'names\\(data\\)'.")

    # if (all(unique(get_class(data[cols])) %in% ignore)) {
    ignore <- unique(vapply(utils::head(datasets::iris, 5),
                            function(x) class(x)[1], character(1)))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(data = utils::head(datasets::iris, 5), ignore = ignore)}
            )
    }

    expect_error(
        shush(mock()),
        "You can't ignore all variables in 'cols' or in "
    )
})

test_that("qplot_walk() | warning test", {
    # if (is.atomic(data)) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            {qplot_walk(data = utils::head(datasets::iris, 5)[[1]])}
        )
    }

    expect_message(mock(), "'data' is 'atomic'. All other arguments, ")

    # if (any(ignore %in% get_class(data[cols]))) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            dialog_line = function(...) TRUE,
            {qplot_walk(
                data = utils::head(datasets::iris, 5), ignore = "factor"
            )}
        )
    }

    expect_message(mock(), "'Species' will be ignored due to the settings ")
})
