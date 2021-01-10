#' Build a fictional standard MCTQ raw dataset
#'
#' @description
#'
#' `build_std_mctq` builds a fictional raw dataset composed by MCTQ (standard
#' version) basic/measurable variables for testing and learning purposes.
#' See [mctq::std_mctq] to learn more.
#'
#' @param write A logical value indicating if `build_std_mctq` must write a
#'   `std_mctq.csv` file to `"./inst/extdata/"` (optional) (default: `FALSE`).
#'
#' @return An invisible tibble.
#' @family Data functions
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{utils::View(build_std_mctq())}
build_std_mctq <- function(write = FALSE) {

    # Create arguments --------------------

    checkmate::assert_logical(write, any.missing = FALSE)

    # Create raw dataset --------------------

    # Subject 1: Sleeps less than recommended for an adult on workdays and
    #            stretches during work-free days.

    std_mctq <- dplyr::tibble(
        `ID` = "1", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "00:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "01:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "15", # duration | M
        `W SLEEP END` = "06:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "No", ## character | Yes/No
        `W LIGHT EXPOSURE` = "02:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "01:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "02:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "15", # duration | M
        `F SLEEP END` = "12:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "30", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "04:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 2: Inverted values for bed time and sleep preparing on workdays.

    dplyr::add_row(
        `ID` = "2", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "4", # integer | [0-7]
        `W BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "23:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "10", # duration | M
        `W SLEEP END` = "05:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "No", # character | Yes/No
        `W LIGHT EXPOSURE` = "04:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "01:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "15", # duration | M
        `F SLEEP END` = "11:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "Child(ren)/pet(s)", # character
        `F LIGHT EXPOSURE` = "06:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 3: Inverted values for bed time and sleep preparing on work-free
    #            days.

    dplyr::add_row(
        `ID` = "3", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "22:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "23:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "30", # duration | M
        `W SLEEP END` = "08:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "10", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "Yes", # character | Yes/No
        `W LIGHT EXPOSURE` = "01:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "23:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "22:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "45", # duration | M
        `F SLEEP END` = "08:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "00:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 4: Presence of invalid values.

    dplyr::add_row(
        `ID` = "4", # integer | [auto-increment]
        `WORK REGULAR` = "No", # character | Yes/No
        `WORK DAYS` = "10", # integer | [0-7] # INVALID
        `W BED TIME` = "27:00", # time of day (0-24h) | HMS, HM, H # INVALID
        `W SLEEP PREP` = "02:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "30", # duration | M
        `W SLEEP END` = "12:15", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "20", # duration | M
        `W ALARM` = "No", # character | Yes/No
        `W WAKE BEFORE ALARM` = "Yes", # character | Yes/No # INVALID
        `W LIGHT EXPOSURE` = "02:15", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "34:00", # time of day (0-24h) | HMS, HM, H # INVALID
        `F SLEEP PREP` = "04:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "No", # duration | M
        `F SLEEP END` = "14:12", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "30", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "-01:30" # duration | [H]MS, [H]M, [H] # INVALID
    ) %>%

    # Subject 5: Sleeps more on workdays than work-free days.

    dplyr::add_row(
        `ID` = "5", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character (Yes/No)
        `WORK DAYS` = "2", # integer | [0-7]
        `W BED TIME` = "20:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "2030", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "15", # duration | M
        `W SLEEP END` = "08:15", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "10", # duration | M
        `W ALARM` = "No", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "0500", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "00:30", # time of day (0-24h) | (HMS, HM, H)
        `F SLEEP LAT` = "5", # duration | M
        `F SLEEP END` = "0600", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "07:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 6: Uses alarm clock on work-free days.

    dplyr::add_row(
        `ID` = "6", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "00:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "15", # duration | M
        `W SLEEP END` = "07:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "No", # character | Yes/No
        `W LIGHT EXPOSURE` = "01:55", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "01:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "01:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "30", # duration | M
        `F SLEEP END` = "08:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "0", # duration | M
        `F ALARM` = "Yes", # character | Yes/No
        `F REASONS` = "Child(ren)/pet(s)", # character
        `F LIGHT EXPOSURE` = "04:45" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 7: Null MCTQ (invalid case).

    dplyr::add_row(
        `ID` = "7", # integer | [auto-increment]
        `WORK REGULAR` = "", # character | Yes/No
        `WORK DAYS` = "", # integer | [0-7]
        `W BED TIME` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "", # duration | M
        `W SLEEP END` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "", # duration | M
        `W ALARM` = "", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "", # duration | M
        `F SLEEP END` = "", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "", # duration | M
        `F ALARM` = "", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 8: Did not answer workdays questions.

    dplyr::add_row(
        `ID` = "8", # integer | [auto-increment]
        `WORK REGULAR` = "No", # character | Yes/No
        `WORK DAYS` = "0", # integer | [0-7]
        `W BED TIME` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "", # duration | M
        `W SLEEP END` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "", # duration | M
        `W ALARM` = "", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "03:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "04:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "90", # duration | M
        `F SLEEP END` = "15:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "30", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "00:30" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 9: All basic variables have the same values (invalid case).

    dplyr::add_row(
        `ID` = "9", # integer | [auto-increment]
        `WORK REGULAR` = "0", # character | Yes/No
        `WORK DAYS` = "0", # integer | [0-7]
        `W BED TIME` = "0", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "0", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "0", # duration | M
        `W SLEEP END` = "0", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "0", # duration | M
        `W ALARM` = "0", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "0", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "0", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "0", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "0", # duration | M
        `F SLEEP END` = "0", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "0", # duration | M
        `F ALARM` = "0", # character | Yes/No
        `F REASONS` = "0", # character
        `F LIGHT EXPOSURE` = "0" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 10: Works 7 days a week and didn't answer work-free days section.

    dplyr::add_row(
        `ID` = "10", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "7", # integer | [0-7]
        `W BED TIME` = "23:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "23:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "10", # duration | M
        `W SLEEP END` = "06:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "No", # character | Yes/No
        `W LIGHT EXPOSURE` = "02:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "", # duration | M
        `F SLEEP END` = "", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "", # duration | M
        `F ALARM` = "", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 11: Suspicious values.

    dplyr::add_row(
        `ID` = "11", # integer | [auto-increment]
        `WORK REGULAR` = "No", # character | Yes/No
        `WORK DAYS` = "6", # integer | [0-7]
        `W BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "00:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "120", # duration | M
        `W SLEEP END` = "04:00", # time of day (0-24h) | HMS, HM, H # SUSPICIOUS
        `W SLEEP INERTIA` = "0", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "Yes", # character | Yes/No
        `W LIGHT EXPOSURE` = "18:00", # duration | [H]MS, [H]M, [H] # SUSPICIOUS
        `F BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "00:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "30", # duration | M
        `F SLEEP END` = "09:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "17:00" # duration | [H]MS, [H]M, [H] # SUSPICIOUS
    ) %>%

    # Subject 12: Ambiguous values.

    dplyr::add_row(
        `ID` = "12", # integer | [auto-increment]
        `WORK REGULAR` = "true", # character | Yes/No # AMBIGUOUS
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "11:00 PM", # time of day (0-24h) | HMS, HM, H # AMBIGUOUS
        `W SLEEP PREP` = "0000", # time of day (0-24h) | HMS, HM, H # AMBIGUOUS
        `W SLEEP LAT` = "00:15", # duration | M #AMBIGUOUS
        `W SLEEP END` = "07:15 AM", # time of day (0-24h) | HMS, HM, H # AMBIGUOUS
        `W SLEEP INERTIA` = "30", # duration | M
        `W ALARM` = "No", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "3", # duration | [H]MS, [H]M, [H] # AMBIGUOUS
        `F BED TIME` = "01:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "0130 AM", # time of day (0-24h) | HMS, HM, H # AMBIGUOUS
        `F SLEEP LAT` = "60", # duration | M
        `F SLEEP END` = "10:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "00:15", # duration | M # AMBIGUOUS
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "04:30" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 13: Possible filling error.

    dplyr::add_row(
        `ID` = "13", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "6", # integer | [0-7]
        `W BED TIME` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "", # duration | M
        `W SLEEP END` = "", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "", # duration | M
        `W ALARM` = "", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "20:30", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "21:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "15", # duration | M
        `F SLEEP END` = "06:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "3", # duration | M
        `F ALARM` = "Yes", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "01:30" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 14: Repeated workdays and work-free days values (possible
    #             carryover effect).

    dplyr::add_row(
        `ID` = "14", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "22:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "23:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "10", # duration | M
        `W SLEEP END` = "07:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # character | Yes/No
        `W WAKE BEFORE ALARM` = "No", # character | Yes/No
        `W LIGHT EXPOSURE` = "01:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "22:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "23:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "5", # duration | M
        `F SLEEP END` = "07:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "Yes", # character | Yes/No
        `F REASONS` = "No", # character
        `F LIGHT EXPOSURE` = "01:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    # Subject 15: Sleep onset is equal or greater than sleep end
    #             [(s_prep + s_lat) >= se].

    dplyr::add_row(
        `ID` = "15", # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # character | Yes/No
        `WORK DAYS` = "2", # integer | [0-7]
        `W BED TIME` = "22:30", # time of day (0-24h) | HMS, HM, H
        `W SLEEP PREP` = "00:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP LAT` = "60", # duration | M
        `W SLEEP END` = "08:00", # time of day (0-24h) | HMS, HM, H
        `W SLEEP INERTIA` = "10", # duration | M
        `W ALARM` = "No", # character | Yes/No
        `W WAKE BEFORE ALARM` = "", # character | Yes/No
        `W LIGHT EXPOSURE` = "01:20", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP PREP` = "02:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP LAT` = "120", # duration | M
        `F SLEEP END` = "04:00", # time of day (0-24h) | HMS, HM, H
        `F SLEEP INERTIA` = "15", # duration | M
        `F ALARM` = "No", # character | Yes/No
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "04:00" # duration | [H]MS, [H]M, [H]
    )

    # Write and output dataset --------------------

    if (write) {
        if(!(dir.exists("./inst/extdata/"))) {
            dir.create("./inst/extdata/")
        }

        std_mctq %>%
            readr::write_delim(paste0("./inst/extdata/", "std_mctq", ".csv"),
                        delim = ",",
                        col_names = TRUE)
    }

    invisible(std_mctq)

}

#' Tidy [mctq::build_std_mctq()] output
#'
#' @description
#'
#' `tidy_std_mctq` tidy the output of [mctq::build_std_mctq()].
#'
#' @details
#'
#' Here the process of `tiding` a dataset is understood as transforming it in
#' input data, like described in Loo and Jonge ([2018](https://bit.ly/3pVuUdt)).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' `std_mctq` data, run [mctq::validate_std_mctq()].
#'
#' To learn more about the concept of tidy data, _c.f._ Wickham
#' ([2014](https://bit.ly/3hBTE7g)) and Wickham and Grolemund
#' ([n.d.](https://r4ds.had.co.nz)).
#'
#' @param write A logical value indicating if the function must write a
#'   `std_mctq.rda` file to `"./data/"` (optional) (default: `FALSE`).
#'
#' @return An invisible tibble.
#' @family Data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data !!
#' @export
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. doi:
#' [10.1002/9781118897126](http://dx.doi.org/10.1002/9781118897126).
#'
#' Wickham, H. (2014). Tidy Data. _Journal of Statistical Software_, _59_(10),
#' 1-23. doi: [10.18637/jss.v059.i10](http://dx.doi.org/10.18637/jss.v059.i10).
#'
#' Wickham, H, & Grolemund. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. Retrieved from <https://r4ds.had.co.nz>.
#'
#' @examples
#' \dontrun{utils::View(tidy_std_mctq())}
tidy_std_mctq <- function(write = FALSE) {

    # Clean NULL cases --------------------

    ## dplyr method (uses Rccp -> more efficient)

    std_mctq <- build_std_mctq() %>%
        dplyr::mutate(dplyr::across(.fns = ~ dplyr::na_if(.x, ""))) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(length = dplyr::n_distinct(dplyr::c_across())) %>%
        dplyr::mutate(dplyr::across(.fns = ~ ifelse(length <= 2, NA, .x))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-length)

    # ## Loop method (not efficient for large datasets)
    #
    # for (i in seq_len(nrow(std_mctq))) {
    #     if (dplyr::n_distinct(as.character(std_mctq[i, ])) <= 2) {
    #         std_mctq[i, ] <- NA
    #     }
    # }

    # Convert variables --------------------

    ## Note 1: `base::ifelse`, `dplyr::if_else`, and `dplyr::case_when` do
    ## vectorised if/else operations, but don't do lazy evaluation, _i.e_ all
    ## parts of the statement are evaluated and then the condition is used to
    ## splice together the results to be returned. This can results in erroneous
    ## warnings. That's why you may see some `suppressWarnings()` or `quiet`
    ## arguments below. You must be very careful while using this kind of
    ## evaluations. __c.f.__ <http://bit.ly/2X1J4x0> and
    ## <http://bit.ly/2X5MUFC>.

    ## Note 2: "base::ifelse does evaluate both possible responses, except in
    ## cases where the test is either all `TRUE` or all `FALSE`". __c.f__.
    ## <http://bit.ly/2X1J4x0>.

    ## Note 3: Some if/else functions, like `data.table::fcase()`, do lazy
    ## evaluation, but that still produces a warning if some of the conditions
    ## are `TRUE` and the true value contain a vectorised function. That's
    ## because the function is evaluated once and, due to the vectorised nature,
    ## it evaluates all values of the object.

    ## Note 4: It appears that no one have a way to go around some of this
    ## situations (last Stack Overflow search: 2021-01-03).

    pattern_1 <- "^([0-1]\\d|2[0-3])(:)?[0-5]\\d((:)?[0-5]\\d)?"
    pattern_2 <- "^\\d{1,3}$"
    pattern_3 <- "[^A-Za-z]$"
    pattern_4 <- "^([-+])?(2[4-9]|[3-9]\\d|\\d{3,})(:)?[0-5]\\d((:)?[0-5]\\d)?"
    pattern_5 <- "^([-+])?\\d+(:)?[0-5]\\d((:)?[0-5]\\d)?"
    pattern_6 <- "^[0-1][0-2](:)?[0-5]\\d(AM|PM)"
    pattern_7 <- "(AM|PM)$"

    std_mctq <- std_mctq %>% dplyr::transmute(
        id = as.integer(.data$`ID`),
        regular_work_schedule = dplyr::case_when(
            .data$`WORK REGULAR` == "Yes" ~ TRUE,
            .data$`WORK REGULAR` == "true" ~ TRUE,
            .data$`WORK REGULAR` == "No" ~ FALSE),
        wd = as.integer(.data$`WORK DAYS`),
        bt_w = dplyr::case_when(
            stringr::str_detect(.data$`W BED TIME`, pattern_1) ~
                convert_to_date_time(.data$`W BED TIME`, "hms",
                                     c("HM", "IMp"), quiet = TRUE),
            stringr::str_detect(.data$`W BED TIME`, pattern_4) ~
                convert_to_date_time(.data$`W BED TIME`, "hms", "HM",
                                     quiet = TRUE)),
        s_prep_w = convert_to_date_time(.data$`W SLEEP PREP`, "hms"),
        s_lat_w = dplyr::case_when(
            stringr::str_detect(.data$`W SLEEP LAT`, pattern_2) ~
                convert_to_date_time(.data$`W SLEEP LAT`, "Duration", "M",
                                     quiet = TRUE),
            stringr::str_detect(.data$`W SLEEP LAT`, pattern_1) ~
                convert_to_date_time(.data$`W SLEEP LAT`, "Duration",
                                     quiet = TRUE)),
        se_w = convert_to_date_time(.data$`W SLEEP END`, "hms"),
        si_w = convert_to_date_time(.data$`W SLEEP INERTIA`, "Duration", "M"),
        alarm_w = dplyr::case_when(
            .data$`W ALARM` == "Yes" ~ TRUE,
            .data$`W ALARM` == "No" ~ FALSE),
        wake_before_alarm_w = dplyr::case_when(
            .data$`W WAKE BEFORE ALARM` == "Yes" ~ TRUE,
            .data$`W WAKE BEFORE ALARM` == "No" ~ FALSE),
        le_w = convert_to_date_time(.data$`W LIGHT EXPOSURE`, "Duration"),
        bt_f = dplyr::case_when(
            stringr::str_detect(.data$`F BED TIME`, pattern_1) ~
                convert_to_date_time(.data$`F BED TIME`, "hms", quiet = TRUE),
            stringr::str_detect(.data$`F BED TIME`, pattern_4) ~
                convert_to_date_time(.data$`F BED TIME`, "hms", "HM",
                                     quiet = TRUE)),
        s_prep_f = convert_to_date_time(.data$`F SLEEP PREP`, "hms"),
        s_lat_f = convert_to_date_time(.data$`F SLEEP LAT`, "Duration", "M",
                                       quiet = TRUE),
        se_f = convert_to_date_time(.data$`F SLEEP END`, "hms"),
        si_f = dplyr::case_when(
            stringr::str_detect(.data$`F SLEEP INERTIA`, pattern_2) ~
                convert_to_date_time(.data$`F SLEEP INERTIA`, "Duration", "M",
                                     quiet = TRUE),
            stringr::str_detect(.data$`F SLEEP INERTIA`, pattern_1) ~
                convert_to_date_time(.data$`F SLEEP INERTIA`, "Duration",
                                     quiet = TRUE)),
        alarm_f = dplyr::case_when(
            .data$`F ALARM` == "Yes" ~ TRUE,
            .data$`F ALARM` == "No" ~ FALSE),
        reasons_f = .data$`F REASONS` ,
        le_f = convert_to_date_time(.data$`F LIGHT EXPOSURE`, "Duration", "HM")
    )

    # Write and output dataset --------------------

    file <- paste0("./data/", "std_mctq", ".rda")

    if (write) {
        if(!(dir.exists("./data/"))) {
            dir.create("./data/")
        }

        save(std_mctq, file = file,
             envir = environment(), compress = "bzip2", version = 2)
    }

    invisible(std_mctq)

}

#' Validate [mctq::tidy_std_mctq()] output
#'
#' @description
#'
#' __UNDER DEVELOPMENT__
#'
#' @inheritParams tidy_std_mctq
#' @return An invisible tibble.
#' @family Data functions
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
#' \dontrun{utils::View(validate_std_mctq())}
validate_std_mctq <- function(write = FALSE) {

    # __UNDER DEVELOPMENT__

}

#' A standard MCTQ dataset
#'
#' @description
#'
#' A fictional dataset composed by MCTQ (standard version) basic/measurable
#' variables for testing and learning purposes.
#'
#' This data was created according to Roenneberg, Wirz-Justice & Merrow
#' ([2003](https://bit.ly/3rLu195)) and guidelines of The World Wide
#' Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)). See References
#' and Details sections to learn more.
#'
#' The naming of the variables took into account the standard names used by
#' theWeP and guidelines of the
#' [tidyverse style guide](https://style.tidyverse.org/).
#'
#' @details
#'
#' `std_mctq` is a tidy and valid version of [mctq::build_std_mctq()], which
#' output is the raw data for `std_mctq`. This dataset was created to
#' demonstrate common cases and data issues that researchers may find in their
#' MCTQ data.
#'
#' You can learn more about the `std_mctq` data cleaning process in
#' `vignette("data_issues")`.
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ), _cf._
#' Roenneberg, Wirz-Justice & Merrow ([2003](https://bit.ly/3rLu195)) and
#' ROENNEBERG, T. _et al._ ([2015](http://bit.ly/2X37mqE)).
#'
#' To know about different MCTQ versions, _cf._ Juda, Vetter & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)) and Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)).
#'
#' If you curious about the computations and want to have access to
#' the full questionnaire, _cf._ The Worldwide Experimental Platform
#' ([n.d.](http://bit.ly/3pv8EH1)).
#'
#' @format A data frame with 10 rows and 18 variables:
#'
#' \describe{
#'   \item{regular_work_schedule}{
#'   A logical value indicating if the subject have a regular work schedule.
#'
#'   Statement (EN): "I have a regular work schedule (this includes being, for
#'   example, a housewife or househusband)").
#'
#'   Class: `logical`.}
#'
#'   \item{wd}{
#'   Number of workdays per week.
#'
#'   Statement (EN): "I have a regular work schedule and work ___ days per
#'   week").
#'
#'   Class: `integer`.}
#'
#'   \item{bt_w}{
#'   Local time of going to bed on workdays.
#'
#'   Statement (EN): "I go to bed at ___ o'clock'".
#'
#'   Class: `hms`.}
#'
#'   \item{s_prep_w}{
#'   Local time of preparing to sleep on workdays.
#'
#'   Statement (EN): "I actually get ready to fall asleep at ___ o'clock".
#'
#'   Class: `hms`.}
#'
#'   \item{s_lat_w}{
#'   Sleep latency on workdays.
#'
#'   Statement (EN): "I need ___ minutes to fall asleep".
#'
#'   Class: `Duration`.}
#'
#'   \item{se_w}{
#'   Sleep end on workdays.
#'
#'   Statement (EN): "I wake up at ___ o'clock".
#'
#'   Class: `hms`.}
#'
#'   \item{si_w}{
#'   Sleep inertia on workdays.
#'
#'   Statement (EN): "After ___ minutes, I get up".
#'
#'   Class: `Duration`.}
#'
#'   \item{alarm_w}{
#'   A logical value indicating if the subject use an alarm clock on workdays.
#'
#'   Statement (EN): "I use an alarm clock on workdays".
#'
#'   Class: `logical`.}
#'
#'   \item{wake_before_alarm_w}{
#'   A logical value indicating if the subject regularly wake up BEFORE the
#'   alarm rings.
#'
#'   Statement (EN): "I regularly wake up BEFORE the alarm rings".
#'
#'   Class: `logical`.}
#'
#'   \item{le_w}{
#'   Light exposure on workdays.
#'
#'   Statement (EN): "On average, I spend the following amount of time outdoors
#'   in daylight (without a roof above my head)".
#'
#'   Class: `Duration`.}
#'
#'   \item{bt_f}{
#'   Local time of going to bed on work-free days.
#'
#'   Statement (EN): "I go to bed at ___ o'clock'".
#'
#'   Class: `hms`.}
#'
#'   \item{s_prep_f}{
#'   Local time of preparing to sleep on work-free days
#'
#'   Statement (EN): "I actually get ready to fall asleep at ___ o'clock".
#'
#'   Class: `hms`.}
#'
#'   \item{s_lat_f}{
#'   Sleep latency on work-free days.
#'
#'   Statement (EN): "I need ___ minutes to fall asleep".
#'
#'   Class: `Duration`.}
#'
#'   \item{se_f}{
#'   Sleep end on work-free days.
#'
#'   Statement (EN): "I wake up at ___ o'clock".
#'
#'   Class: `hms`.}
#'
#'   \item{si_f}{
#'   Sleep inertia on work-free days.
#'
#'   Statement __(EN)__: "After ___ minutes, I get up".
#'
#'   Class: `Duration`.}
#'
#'   \item{alarm_f}{
#'   A logical value indicating if the subject use an alarm clock on work-free
#'   days.
#'
#'   Statement (EN): "My wake-up time is due to the use of an alarm
#'   clock".
#'
#'   Class: `logical`.}
#'
#'   \item{reasons_f}{
#'   Particular reasons why the subject cannot freely choose his sleep times on
#'   work-free days.
#'
#'   Statement (EN): "There are particular reasons why I cannot freely choose my
#'   sleep times on free days".
#'
#'   Class: `character`.}
#'
#'   \item{le_f}{
#'   Light exposure on work-free days.
#'
#'   Statement (EN): "On average, I spend the following amount of time outdoors
#'   in daylight (without a roof above my head)".
#'
#'   Class: `Duration`.}
#' }
#'
#' @usage data(std_mctq)
#' @source Prepared by Daniel Vartanian (package's author).
#' @family Datasets
#'
#' @references
#'
#' Juda, M., Vetter, C., & Roenneberg, T. (2013). The Munich ChronoType
#' Questionnaire for shift-workers (MCTQShift). _Journal of Biological Rhythms_,
#' _28_(2), 130-140. doi:
#' [10.1177/0748730412475041](https://doi.org/10.1177/0748730412475041).
#'
#' Ghotbi, N., Pilz, L. K., Winnebeck, E. C., Vetter, C., Zerbini, G., Lenssen,
#' D., Frighetto, G., Salamanca, M., Costa, R., Montagnese, S., & Roenneberg, T.
#' (2020). The \eqn{\mu}MCTQ: an ultra-short version of the Munich ChronoType
#' Questionnaire. _Journal of Biological Rhythms_, _35_(1), 98-110. doi:
#' [10.1177/0748730419886986](https://doi.org/10.1177/0748730419886986).
#'
#' Roenneberg, T., Keller, L. K., Fischer, D., Matera, J. L., Vetter, C., &
#' Winnebeck, E. C. (2015). Human activity and rest in situ. In A. Sehgal (Ed.),
#' _Methods in enzymology_ (Vol. 552, pp. 257-283). London, UK: Academic Press.
#' doi:
#' [10.1016/bs.mie.2014.11.028](https://doi.org/10.1016/bs.mie.2014.11.028).
#'
#' Roenneberg, T., Wirz-Justice, A., & Merrow, M. (2003). Life between clocks:
#' daily temporal patterns of human chronotypes.
#' _Journal of Biological Rhythms_, _18_(1), 80-90. doi:
#' [10.1177/0748730402239679](https://doi.org/10.1177/0748730402239679).
#'
#' The Worldwide Experimental Platform (n.d.). MCTQ. Retrieved from
#' <https://www.thewep.org/documentations/mctq/>.
"std_mctq"
