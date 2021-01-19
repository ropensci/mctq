#' Quick plot for MCTQ variables
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `qplot_mctq()` uses [ggplot2::qplot()] to help you visualize the
#' distributions of your MCTQ data. The function also adapts time values
#' to better fit plots.
#'
#' @details
#'
#' ## `Duration`, `Period`, and `difftime` objects
#'
#' In order to help with the visualization, `qplot_mctq()` automatically
#' converts `Duration`, `Period`, and `difftime` objects to `hms`.
#'
#' ## Midday change
#'
#' `hms` variables with values greater than `22:00:00` will automatically be
#' converted to POSIXct with a midday change, _i.e._ all values with more
#' than 12 hours will represent day one, and all the rest will represent day 2.
#' This is made in order to better represent values that cross the midnight
#' hour.
#'
#' You can disable this behavior using `midday_change = FALSE`.
#'
#' ## `id` variables
#'
#' `qplot_mctq()` will ignore any variable with the follow name pattern
#' `"^id$|[\\._-]id$"`, _i.e_ any variable named `id` or that ends with
#' `.id`, `_id`, or `-id`.
#'
#' You can disable this behavior using `remove_id = FALSE`.
#'
#' @param data An `atomic` or `data frame` object.
#' @param cols (optional) (only for data frames) a character vector indicating
#'   columns names in `data` for plotting. If `NULL`, `qplot_mctq()` will use
#'   all columns in `data`. This setting only works if `pattern = NULL`
#'   (default: `NULL`).
#' @param pattern (optional) (only for data frames) a string with a regular
#'   expression to select columns names in `data` for plotting. This setting
#'   only works if `cols = NULL` (default: `NULL`).
#' @param ignore (optional) (only for data frames) a character vector indicating
#'   which object classes the function must ignore. This setting can be used
#'   with `cols` and `pattern`. Assign `NULL` to disable this behavior (default:
#'   `"character"`).
#' @param remove_id (optional) (only for data frames) a logical value indicating
#'   if the function must ignore column names in `data` that match with
#'   `"^id$|[\\._-]id$"` (default: `TRUE`).
#' @param midday_change (optional) a logical value indicating if the function
#'   must apply a midday change for `hms` variables with values greater than
#'   `22:00:00` (see Details section to learn more) (default: `TRUE`).
#'
#' @family utility functions
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' qplot_mctq(mctq::std_mctq$bt_w)
#' qplot_mctq(mctq::std_mctq)
#' qplot_mctq(mctq::std_mctq, cols = c("bt_w", "msf_sc"))
#' qplot_mctq(mctq::std_mctq, pattern = "_w$")
#' qplot_mctq(datasets::iris)
#' qplot_mctq(datasets::mtcars)
#' }
qplot_mctq <- function(data, cols = NULL, pattern = NULL, ignore = "character",
                       remove_id = TRUE, midday_change = TRUE) {

    # Check arguments -----

    if (isFALSE(rlang::is_interactive())) {
        rlang::abort("This function can only be used in interactive mode.")
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
            rlang::abort(paste0(
                "`cols` and `pattern` can't both have values. ",
                "You need to choose only one selection method."))
        }
    }

    checkmate::assert_flag(midday_change)

    if (!is.atomic(data) && !is.data.frame(data)) {
        rlang::abort("`data` must be an atomic object or a data frame.")
    }

    # Return output for `atomic` `data` -----

    transform <- function(x, midday_change = TRUE) {
        classes <- c("Duration", "Period", "difftime")

        if (hms::is_hms(x) && isTRUE(midday_change) &&
            any(x > hms::parse_hm("22:00"), na.rm = TRUE)) {
            midday_change(x)
        } else if (checkmate::test_multi_class(x, classes)) {
            convert_to(x, "hms")
        } else {
            x
        }
    }

    if (is.atomic(data)) {
        assert_has_length(data)

        rlang::warn(paste0(
            "`data` is atomic. All other arguments, except `midday_change`, ",
            "are ignored."
            ))

        x <- transform(data, midday_change)

        return(
            ggplot2::qplot(x, xlab = deparse(substitute(data))) %>%
                   print %>% shush
            )
    }

    # Set values -----

    if (is.null(cols) && is.null(pattern)) cols <- names(data)

    if (!is.null(pattern)) {
        cols <- grep(pattern, names(data), value = TRUE)

        if (length(cols) == 0) {
            rlang::abort("None match was found in `names(data)`.")
        }
    }

    if (!is.null(ignore)) {
        if (all(unique(get_class(data[cols])) %in% ignore)) {
            rlang::abort(paste0(
                "You can't ignore all variables in `cols` or in `data`. ",
                "Note that `qplot_mctq()` is set by default to ignore ",
                "'character' objects. Please check your settings."
            ))
        }

        if (!identical(names(data[cols]), names(data)) &&
            any(ignore %in% get_class(data[cols]))) {
            match <- names(data[cols])[get_class(data[cols]) %in% ignore]
            rlang::inform(paste0(
                inline_collapse(match), " ",
                "will be ignored due to the settings ",
                "in `ignore` argument."
            ))
        }

        cols <- names(data[cols])[!(get_class(data[cols]) %in% ignore)]

    }

    if (isTRUE(remove_id)) {
        cols <- cols[!grepl("^id$|[\\._-]id$", cols, ignore.case = TRUE)]
    }

    # Show introductory message -----

    if (requireNamespace("grDevices", quietly = TRUE)) {
        dialog_line(line = paste0(
            "WARNING: `qplot_mctq()` clears all plots from your system \n",
            "after it runs. If you don't agree with this, press `esc` to \n",
            "exit, or `enter` to continue."))
    }

    # Create qplots -----

    for (i in cols) {
        x <- transform(data[[i]], midday_change)
        shush(print(ggplot2::qplot(x, xlab = i)))

        dialog <- dialog_line(
            line = "Press `esc` to exit or `enter` to continue.",
            space_above = FALSE, space_below = FALSE)

        if (requireNamespace("grDevices", quietly = TRUE)) {
            grDevices::dev.off()
        }
    }

}
