#' Validate values from a standard MCTQ dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __UNDER DEVELOPMENT__
#'
#' @details
#'
#' __UNDER DEVELOPMENT__
#'
#' @section Custom Variables:
#'
#' __UNDER DEVELOPMENT__
#'
#' @param data A data frame.
#' @param check A character vector indicating the variables to check. See
#'   Details section for possible values. If `NULL` the function will validate
#'   all MCTQ variables it can find on data (default: `NULL`).
#' @param flag A logical value indicating if the function must return only a
#'   logical value indicating if critical errors was found (default: `FALSE`).
#' @param custom A named list with characters values indicating the equivalent
#'   names for the MCTQ variables. See Custom variables section to learn more
#'   (__optional__) (default: `NULL`)
#' @param breaks A logical value indicating if the function must stop
#' and ask to continue between each validation. This option only works in
#' interactive mode and when `flag = FALSE` (default: `TRUE`).
#'
#' @return If `flag` is `FALSE`, print results from validation tests. If `flag =
#'   TRUE`, returns a logical value indicating if critical errors was found.
#'
#'   When run in interactive mode and with `remove_breaks = FALSE`,
#'   `validate_mctq` offers a way to export the results.
#'
#' @family validation functions
#' @importFrom validate in_range is_complete all_complete
#' @importFrom lubridate is.duration
#' @export
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. doi:
#' [10.1002/9781118897126](http://dx.doi.org/10.1002/9781118897126).
#'
#' @examples
#' \dontrun{
#' validate_mctq(std_mctq, "bt_w")
#' }
validate_mctq <- function(data, check = NULL, flag = FALSE, custom = NULL,
                          breaks = TRUE) {

    # To do --------------------
    #
    # * Give the user a option to choose types of validation
    # * Use ellipsis to assign types for validations

    # Check arguments -----

    checkmate::assert_data_frame(data, all.missing = FALSE, min.rows = 1,
                                 min.cols = 1)
    checkmate::assert_character(check, any.missing = FALSE, min.len = 1,
                                unique = TRUE, null.ok = TRUE)
    checkmate::assert_flag(flag)
    checkmate::assert_list(custom, c("character", "NULL"), min.len = 1,
                           unique = TRUE, names = "unique", null.ok = TRUE)
    checkmate::assert_flag(breaks)

    choices <- c(names(mctq::std_mctq))
    checkmate::assert_subset(check, choices, empty.ok = TRUE)
    checkmate::assert_subset(names(custom), choices, empty.ok = TRUE,
                             .var.name = "custom argument list names")
    checkmate::assert_subset(unlist(custom, use.names = FALSE),
                             names(data), empty.ok = TRUE,
                             .var.name = "custom argument list elements")

    if (is.null(check) && is.null(custom) && !any(choices %in% names(data))) {
        rlang::abort("`data` do not have any MCTQ variable to validate")
        if(isTRUE(flag)) return(FALSE)
    } else if (!is.null(check) && is.null(custom) &&
               !any(check %in% names(data))) {
        rlang::abort("None of `check` variables are present on `data`")
        if(isTRUE(flag)) return(FALSE)
    }

    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    regular_work_schedule <- wd <- bt <- bt_w <- bt_f <- sprep <- NULL
    sprep_w <- sprep_f <- slat <- slat_w <- slat_f <- se <- NULL
    se_w <- se_f <- si <- si_w <- si_f <- alarm_w <- alarm_f <- NULL
    wake_before_alarm_w <- le <- le_w <- le_f <- reasons_f <- NULL
    fd <- so <- so_w <- so_f <- gu <- gu_w <- gu_f <- sd <- sd_w <- NULL
    sd_f <- tbt <- tbt_w <- tbt_f <- ms <- msw <- ms_w <- msf <- ms_f <- NULL
    sd_week <- msf_sc <- chronotype <- sloss_week <- sjl_rel <- NULL
    sjl <- le_week <- NULL

    # Rename `data` variables -----

    data_names <- names(data)

    if (!is.null(custom)) {
        for (i in names(custom)) {
            if (is.null(custom[i][1]) || is.na(custom[i][1])) {
                next
            } else {
                names(data)[which(names(data) == custom[i][1])] <- i
            }
        }
    }

    # Set values -----

    if (is.null(check)) {
        check <- names(data)[which(names(data) %in% choices)]
    }

    crayon_message(
        tag = "NOTE",
        title = "Initiating checks",
        text = paste0("Some check routines can take a while. We recommend ",
                      "using breaks (default) for large datasets.")
    )

    dialog_line(!breaks)
    out <- vector(mode = "list")

    # Do class checks (critical!) -----

    rules <- validate::validator(
        regular_work_schedule = is.logical(regular_work_schedule),
        wd = is.integrish(wd),
        bt_w = is.hms(bt_w),
        sprep_w = is.hms(sprep_w),
        slat_w = is.duration(slat_w),
        se_w = is.hms(se_w),
        si_w = is.duration(si_w),
        alarm_w = is.logical(alarm_w),
        wake_before_alarm_w = is.logical(wake_before_alarm_w),
        le_w = is.duration(le_w),
        bt_f = is.hms(bt_f),
        sprep_f = is.hms(sprep_f),
        slat_f = is.duration(slat_f),
        se_f = is.hms(se_f),
        si_f = is.duration(si_f),
        alarm_f = is.logical(alarm_f),
        reasons_f = is.character(reasons_f),
        le_f = is.duration(le_f)
    )

    export <- export_validation(data, rules, check,
                                name = "Class checks (critical!)")
    out <- append(out, list(export))

    if (isTRUE(flag) && !is_valid_test(export$summary)) return(FALSE)

    crayon_message(
        tag = "Results",
        title = "Class validation",
        text = paste0("`validate_mctq` requires that the MTCQ ",
                      "variables conforms to a class template ",
                      "(see `?validate_mctq`). If your ",
                      "data have any fails, errors or warnings in this test, ",
                      "the function will stop and return only this ",
                      "validation report.")
    )

    print(export$summary)

    if (!is_valid_test(export$summary)) return(invisible(out))
    if (dialog_line(!breaks) == 1) return(invisible(out))

    # Do univariate checks (critical!) -----

    rules <- validate::validator(
        wd = in_range(wd, min = 0, max = 7),
        bt_w = in_range(bt_w, min = hms::parse_hm("00:00"),
                        max = hms::parse_hm("24:00")),
        sprep_w = in_range(sprep_w, min = hms::parse_hm("00:00"),
                            max = hms::parse_hm("24:00")),
        slat_w = in_range(slat_w, min = lubridate::dhours(0),
                           max = lubridate::dhours(6)),
        se_w = in_range(se_w, min = hms::parse_hm("00:00"),
                        max = hms::parse_hm("24:00")),
        si_w = in_range(si_w, min = lubridate::dhours(0),
                        max = lubridate::dhours(6)),
        le_w = in_range(le_w, min = lubridate::dhours(0),
                        max = lubridate::dhours(24)),
        bt_f = in_range(bt_f, min = hms::parse_hm("00:00"),
                        max = hms::parse_hm("24:00")),
        sprep_f = in_range(sprep_f, min = hms::parse_hm("00:00"),
                            max = hms::parse_hm("24:00")),
        slat_f = in_range(slat_f, min = lubridate::dhours(0),
                           max = lubridate::dhours(6)),
        se_f = in_range(se_f, min = hms::parse_hm("00:00"),
                        max = hms::parse_hm("24:00")),
        si_f = in_range(si_f, min = lubridate::dhours(0),
                        max = lubridate::dhours(6)),
        le_f = in_range(le_f, min = lubridate::dhours(0),
                        max = lubridate::dhours(24))
    )

    export <- export_validation(data, rules, check,
                                name = "Univariate checks (critical!)")
    out <- append(out, list(export))

    if (isTRUE(flag) && !is_valid_test(export$summary)) return(FALSE)

    crayon_message(
        tag = "Results",
        title = "Univariate checks",
        text = paste0("Verifying if all values are within limits.")
    )

    print(export$summary)

    if (dialog_line(!breaks) == 1) return(invisible(out))

    ## Do multivariate checks (critical!) -----

    rules <- validate::validator(
        "wd | x_w" = if(!is.na(wd)) is_complete(bt_w, sprep_w, slat_w,
                                                se_w, si_w),
        "fd(wd) | x_f" = if(fd(wd) > 0) is_complete(bt_f, sprep_f, slat_f,
                                                    se_f, si_f),
        "bt_w | sprep_w inversion" =
            assign_date(bt_w, sprep_w) < lubridate::dhours(12),
        "bt_f | sprep_f inversion" =
            assign_date(bt_f, sprep_f) < lubridate::dhours(12)
    )

    export <- export_validation(data, rules, check,
                                name = "Multivariate checks (critical!)")
    out <- append(out, list(export))

    if (isTRUE(flag) && !is_valid_test(export$summary)) return(FALSE)

    crayon_message(
        tag = "Results",
        title = "Multivariate checks",
        text = paste0("Verifying if all values are within limits")
    )

    print(export$summary)

    if (dialog_line(!breaks) == 1) return(invisible(out))
    if (isTRUE(flag)) return(TRUE)

    # ## Check indicators (critical -----
    #
    # rules <- validate::indicator(
    #     so_w = so(sprep_w, slat_w)
    # )
    #
    # export <- export_validation(data, rules, check,
    #                             name = "Indicators checks (critical!)")
    # out <- append(out, list(export))
    #
    # if (isTRUE(flag) && !is_valid_test(export$summary)) return(FALSE)
    #
    # crayon_message(
    #     tag = "Results",
    #     title = "Univariate checks",
    #     text = paste0("Verifying if all values are within the limits")
    # )
    #
    # print(export$summary)
    #
    # if (dialog_line(!breaks) == 1) return(invisible(out))
    # if (isTRUE(flag)) return(TRUE)
    #
    # # Check for missing values -----
    #
    # rules <- validate::validator(
    #     wd = !is.na(wd),
    #     bt_w = !is.na(bt_w),
    #     s_prep_w = !is.na(s_prep_w)
    # )
    #
    # export <- export_validation(data, rules, check)
    #
    # rules <- validate::validator(
    #     wd = all_complete(wd),
    #     bt_w = all_complete(bt_w),
    #     s_prep_w = all_complete(s_prep_w)
    # )
    #
    # ## Check for suspicious values -----



}

#' @rdname validate_mctq
#' @export
is_valid <- function(data, check = NULL, custom = NULL) {

    validate_mctq(data, check, flag = TRUE, custom)

}

#' @family validation functions
#' @noRd
is_valid_test <- function(summary) {

    if (sum(summary$fails) > 0 || any(summary$error) ||
        any(summary$warning)) {
        FALSE
    } else {
        TRUE
    }

}

#' @family validation functions
#' @noRd
export_validation <- function(data, rules, check, name = NULL) {

    checkmate::assert_data_frame(data)
    checkmate::assert_class(rules, "validator")
    checkmate::assert_character(check, any.missing = FALSE, min.len = 1)
    checkmate::assert_string(name, null.ok = TRUE)

    index <- as.integer(NULL)
    for (i in seq_along(rules)) {
        if (all(validate::variables(rules[i]) %in% check)) {
            index <- append(index, i)
        }
    }

    rules <- rules[index]
    confront <- validate::confront(data, rules)
    summary <- validate::summary(confront)
    aggregate <- validate::aggregate(confront, by = "record")
    violating <- try(validate::violating(data, confront), silent = TRUE)

    if (inherits(violating, "try-error")) violating <- NULL

    out <- list(
        name = name,
        rules = rules[],
        confront = confront[],
        summary = summary,
        aggregate = aggregate,
        violating = violating)

    invisible(out)

}

# Wrappers

#' @family validation functions
#' @noRd
is.time <- function(x, rm = NULL) is_time(x, rm)

#' @family validation functions
#' @noRd
is.hms <- function(x) hms::is_hms(x)

#' @family validation functions
#' @noRd
is.integrish <- function(x) rlang::is_integerish(x)
