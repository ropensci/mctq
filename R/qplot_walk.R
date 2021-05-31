#' Walk through distribution plots
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `qplot_walk()` helps you to visually assess the distribution of your data.
#' It uses [ggplot2::qplot()] to walk through each selected variable from a
#' data frame.
#'
#' This function requires the [`grDevices`][grDevices::grDevices-package] and
#' [`ggplot2`][ggplot2::ggplot2-package] packages and can only run in
#' interactive mode.
#'
#' @details
#'
#' ## Requirements
#'
#' This function requires the [`grDevices`][grDevices::grDevices-package] and
#' [`ggplot2`][ggplot2::ggplot2-package] package and can only run in
#' interactive mode. The `grDevices` comes with a standard R installation and
#' is typically loaded by default. Most people also run R interactively.
#'
#' If you don't have any of the two packages mentioned above, you can install
#' them with `install.packages("grDevices")` and `install.packages("ggplot2")`.
#'
#' ## Plot recover
#'
#' `qplot_walk()` erases all plots after it runs. For that reason, the function
#' first emits a dialog message warning the user of this behavior before it
#' runs. If you want to recover a single distribution plot, assign the variable
#' vector to the `data` argument.
#'
#' ## Additional arguments to [ggplot2::qplot()]
#'
#' `qplot_walk()` uses ggplot2 [ggplot2::qplot()] to generate plots. If you are
#' familiar with [ggplot2::qplot()], you can pass additional arguments to the
#' function using the ellipsis argument (`...`).
#'
#' Note that `x`, `y`, and `data` arguments are reserved for `qplot_walk()`.
#'
#' ## `Duration`, `Period`, and `difftime` objects
#'
#' To help with the visualization, `qplot_walk()` automatically
#' converts `Duration`, `Period`, and `difftime` objects to `hms`.
#'
#' ## Midday change
#'
#' Time variables with values greater than `22:00:00` will automatically be
#' converted to `POSIXct` and be attached to a two-day timeline using the midday
#' hour as a cutting point, i.e., all values with 12 hours or more will be
#' placed on day 1, and all the rest will be placed on day 2.
#'
#' This is made to better represent time vectors that cross the midnight hour.
#' You can disable this behavior by using `midday_change = FALSE`.
#'
#' Example: Say you have a vector of time values that cross the midnight hour
#' (e.g., an `hms` vector with `22:00`, `23:00`, `00:00`, `01:00` values). If
#' you use `midday_change = FALSE`, your data will be represented linearly.
#'
#' ```
#' 00:00 01:00                                22:00 23:00
#'   |-----|------------------------------------|-----|------->
#' ```
#'
#' By using `midday_change = TRUE` (default), `qplot_walk()` will fit your data
#' to a circular time frame of 24 hours.
#'
#' ```
#'              day 1                         day 2
#'                 22:00 23:00 00:00 01:00
#' ------------------|-----|-----|-----|---------------------->
#' ```
#'
#' ## `id` variables
#'
#' `qplot_walk()` will ignore any variable with the follow name pattern
#' `"^id$|[\\._-]id$"`, i.e. any variable named `id` or that ends with
#' `.id`, `_id`, or `-id`.
#'
#' You can disable this behavior using `remove_id = FALSE`.
#'
#' @param data An `atomic` or `data.frame` object.
#' @param ... (optional) additional arguments to be passed to
#'   [ggplot2::qplot()].
#' @param cols (optional) (only for data frames) a `character` object indicating
#'   column names in `data` for plotting. If `NULL`, `qplot_walk()` will use
#'   all columns in `data`. This setting only works if `pattern = NULL`
#'   (default: `NULL`).
#' @param pattern (optional) (only for data frames) a string with a regular
#'   expression to select column names in `data` for plotting. This setting
#'   only works if `cols = NULL` (default: `NULL`).
#' @param ignore (optional) (only for data frames) a `character` object
#'   indicating which object classes the function must ignore. This setting can
#'   be used with `cols` and `pattern`. Assign `NULL` to disable this behavior
#'   (default: `"character"`).
#' @param remove_id (optional) (only for data frames) a logical value indicating
#'   if the function must ignore column names in `data` that match with the
#'   regular expression `"^id$|[\\._-]id$"` (default: `TRUE`).
#' @param midday_change (optional) a logical value indicating if the function
#'   must apply a midday change for `hms` variables with values greater than
#'   `22:00:00` (see Details section to learn more) (default: `TRUE`).
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## Ploting a single column from `data`
#'
#' qplot_walk(mctq::std_mctq$bt_w)
#'
#' ## Ploting all columns from `data`
#'
#' qplot_walk(mctq::std_mctq, ignore = NULL, remove_id = FALSE)
#'
#' ## Ploting selected columns from `data`
#'
#' qplot_walk(mctq::std_mctq, cols = c("bt_w", "msf_sc"))
#'
#' ## Ploting selected columns from `data` using a name pattern
#'
#' qplot_walk(mctq::std_mctq, pattern = "_w$")
#'
#' ## Examples using other datasets
#'
#' if (requireNamespace("datasets", quietly = TRUE)) {
#'     qplot_walk(datasets::iris)
#'     qplot_walk(datasets::mtcars)
#' }}
qplot_walk <- function(data, ..., cols = NULL, pattern = NULL,
                       ignore = "character", remove_id = TRUE,
                       midday_change = TRUE) {
    if (!is_interactive()) {
        stop("This function can only be used in interactive mode.",
             call. = FALSE)
    }

    if (!require_namespace("grDevices", quietly = TRUE) ||
        !require_namespace("ggplot2", quietly = TRUE)) {
        stop("This function requires the 'grDevices' and 'ggplot2' packages ",
            'to run. You can install them by running: \n\n',
            'install.packages("grDevices") \n',
            'install.packages("ggplot2")' , call. = FALSE)
    }

    if (any(c("x", "y", "data") %in% names(list(...)))) {
        stop("'x', 'y' and `data` are reserved arguments for ",
            "'qplot_walk()'.", call. = FALSE)
    }

    if (is.data.frame(data)) {
        checkmate::assert_data_frame(data, all.missing = FALSE, min.rows = 1,
                                     min.cols = 1)
        checkmate::assert_subset(cols, names(data), empty.ok = TRUE)
        checkmate::assert_string(pattern, null.ok = TRUE)
        checkmate::assert_character(ignore, any.missing = FALSE, min.len = 1,
                                    unique = TRUE, null.ok = TRUE)
        checkmate::assert_flag(remove_id)

        if (!is.null(cols) && !is.null(pattern)) {
            stop("'cols' and 'pattern' can't both have values. ",
                "You need to choose only one selection method.", call. = FALSE)
        }
    }

    checkmate::assert_flag(midday_change)

    if (!is.atomic(data) && !is.data.frame(data)) {
        stop("'data' must be an 'atomic' object or a data frame.", call. = FALSE)
    }

    transform <- function(x, midday_change = TRUE) {
        classes <- c("Duration", "Period", "difftime")

        if (hms::is_hms(x) && isTRUE(midday_change) &&
            any(x > hms::parse_hm("22:00"), na.rm = TRUE)) {
            midday_change(x)
        } else if (checkmate::test_multi_class(x, classes)) {
            convert(x, "hms")
        } else {
            x
        }
    }

    if (is.atomic(data)) {
        assert_has_length(data)

        warning("'data' is 'atomic'. All other arguments, except '...' and ",
                "'midday_change', are ignored.", call. = FALSE)

        x <- transform(data, midday_change)
        xlab <- deparse(substitute(data))

        if ("xlab" %in% names(list(...))) {
            plot <- ggplot2::qplot(x, ...)
            return(shush(print(plot)))
        } else {
            plot <- ggplot2::qplot(x, xlab = xlab, ...)
            return(shush(print(plot)))
        }
    }

    if (is.null(cols) && is.null(pattern)) cols <- names(data)

    if (!is.null(pattern)) {
        cols <- grep(pattern, names(data), value = TRUE)

        if (length(cols) == 0) {
            stop("None match was found in 'names(data)'.", call. = FALSE)
        }
    }

    if (!is.null(ignore)) {
        if (all(unique(get_class(data[cols])) %in% ignore)) {
            stop("You can't ignore all variables in 'cols' or in 'data'. ",
                "Note that `qplot_walk()` is set by default to ignore ",
                "'character' objects. Please check your settings.",
                call. = FALSE)
        }

        if (any(ignore %in% get_class(data[cols]))) {
            match <- names(data[cols])[get_class(data[cols]) %in% ignore]
            warning(inline_collapse(match), " will be ignored due to the ",
                    "settings in the 'ignore' argument.",
                    call. = FALSE, immediate. = TRUE)
        }

        cols <- names(data[cols])[!(get_class(data[cols]) %in% ignore)]

    }

    if (isTRUE(remove_id)) {
        cols <- cols[!grepl("^id$|[\\._-]id$", cols, ignore.case = TRUE)]
    }

    alert(
        "\nWarning: `qplot_walk()` clears all plots from your system ",
        "after it runs. If you don't agree with this, press 'esc' to ",
        "exit.\n", combined_styles = c("bold", "red")
        )

    dialog <- dialog_line(
        "Press 'esc' to exit or 'enter' to continue >",
        space_above = FALSE, space_below = FALSE)

    for (i in cols) {
        x <- transform(data[[i]], midday_change)

        if ("xlab" %in% names(list(...))) {
            shush(print(ggplot2::qplot(x, ...)))
        } else {
            shush(print(ggplot2::qplot(x, xlab = i, ...)))
        }

        dialog <- dialog_line(
            "Press 'esc' to exit or 'enter' to continue > ",
            space_above = FALSE, space_below = FALSE)

        grDevices::dev.off()
    }

    invisible(NULL)
}
