# Source the file before running the functions
# Don't forget to uncomment the `library` functions below

# library(checkmate)
# library(dplyr)
# library(hms)
# library(lubridate)
# library(magrittr)
# library(mctq)
# library(rlang)
# library(usethis)
# library(utils)
# library(validate)

#' Build a fictional MCTQ\eqn{^{Shift}}{ Shift} raw dataset
#'
#' @description
#'
#' `build_shift_mctq()` builds a fictional raw dataset, __for testing and
#' learning purposes__, composed by basic/measurable variables of the Munich
#' Chronotype Questionnaire (MCTQ) shift version. See [mctq::shift_mctq] to
#' learn more.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `shift_mctq.csv` file to `"./inst/extdata/"` (default: `FALSE`).
#' @param random_cases (optional) a `logical` value indicating if the function
#'   must add random MCTQ cases besides the core ones.
#'
#' @return An invisible `tibble` with a raw MCTQ\eqn{^{Shift}}{ Shift} dataset.
#'
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::build_shift_mctq())
#' }}
build_shift_mctq <- function(write = FALSE, random_cases = TRUE) {
    # Check arguments -----

    checkmate::assert_flag(write)
    checkmate::assert_flag(random_cases)

    # Set IDs -----

    set.seed(1)
    reserved_id <- sample(50, 15)
    id <- seq(50)[!(seq(50) %in% reserved_id)]

    # Create cases -----

    ## Base subject: sleeps less than the recommended for an adult on workdays
    ##               and stretches during work-free days

    shift_mctq <- dplyr::tibble(
        `ID` = as.character(reserved_id[1]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No

        `W M N DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "00:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "01:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "15", # Duration | M
        `W SLEEP END` = "06:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # Duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "02:00", # Duration | [H]MS, [H]M, [H]

        `F BED TIME` = "01:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "02:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "15", # Duration | M
        `F SLEEP END` = "12:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "30", # Duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "No", # logical |  Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "04:00" # Duration | [H]MS, [H]M, [H]
    )

    ## Random cases

    format_logical <- function(x) {
        dplyr::case_when(
            x == TRUE ~ "Yes",
            x == FALSE ~ "No"
        )
    }

    format_hms <- function(x) {
        format <- c(1, 5)
        x <- convert(x, "hms")
        substr(as.character(x), format[1], format[2])
    }

    format_duration <- function(x) {
        as.character(convert_tu(x, "M"))
    }

    if (isTRUE(random_cases)) {
        for (i in id) {
            random_case <- dplyr::as_tibble(
                random_mctq(model = "standard", quiet = TRUE)) %>%
                dplyr::transmute(
                    `ID` = as.character(i),
                    `WORK REGULAR` =  format_logical(.data$work),
                    `WORK DAYS` = as.character(.data$wd),
                    `W BED TIME` = format_hms(.data$bt_w),
                    `W SLEEP PREP` = format_hms(.data$sprep_w),
                    `W SLEEP LAT` = format_duration(.data$slat_w),
                    `W SLEEP END` = format_hms(.data$se_w),
                    `W SLEEP INERTIA` = format_duration(.data$si_w),
                    `W ALARM` = format_logical(.data$alarm_w),
                    `W WAKE BEFORE ALARM` = format_logical(.data$wake_before_w),
                    `W LIGHT EXPOSURE` = format_hms(.data$le_w),
                    `F BED TIME` = format_hms(.data$bt_f),
                    `F SLEEP PREP` = format_hms(.data$sprep_f),
                    `F SLEEP LAT` = format_duration(.data$slat_f),
                    `F SLEEP END` = format_hms(.data$se_f),
                    `F SLEEP INERTIA` = format_duration(.data$si_f),
                    `F ALARM` = format_logical(.data$alarm_f),
                    `F REASONS` = format_logical(.data$reasons_f),
                    `F REASONS WHY` = .data$reasons_why_f,
                    `F LIGHT EXPOSURE` = format_hms(.data$le_f)
                )

            shift_mctq <- dplyr::bind_rows(shift_mctq, random_case)
        }
    }

    ## Inverted values for bed time and sleep preparing on workdays

    shift_mctq <- shift_mctq %>% dplyr::add_row(
        `ID` = as.character(reserved_id[2]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "4", # integer | [0-7]
        `W BED TIME` = "00:00", # hms | HMS, HM, H [0-24h] # INVERSION
        `W SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h] # INVERSION
        `W SLEEP LAT` = "10", # Duration | M
        `W SLEEP END` = "05:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # Duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "04:00", # Duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "01:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "15", # Duration | M
        `F SLEEP END` = "11:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "5", # Duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Child(ren)/pet(s)", # character
        `F LIGHT EXPOSURE` = "06:00" # Duration | [H]MS, [H]M, [H]
    ) %>%

    ## Inverted values for bed time and sleep preparing on work-free days

        dplyr::add_row(
            `ID` = as.character(reserved_id[3]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "5", # integer | [0-7]
            `W BED TIME` = "22:30", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "30", # Duration | M
            `W SLEEP END` = "08:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "10", # Duration | M
            `W ALARM` = "Yes", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "Yes", # logical | Yes/No
            `W LIGHT EXPOSURE` = "01:00", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "23:00", # hms | HMS, HM, H [0-24h] # INVERSION
            `F SLEEP PREP` = "22:30", # hms | HMS, HM, H [0-24h] # INVERSION
            `F SLEEP LAT` = "45", # Duration | M
            `F SLEEP END` = "08:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "5", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "No", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "00:00" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Presence of invalid values

        dplyr::add_row(
            `ID` = as.character(reserved_id[4]), # integer | [auto-increment]
            `WORK REGULAR` = "No", # logical | Yes/No
            `WORK DAYS` = "10", # integer | [0-7] # INVALID
            `W BED TIME` = "27:00", # hms | HMS, HM, H [0-24h] # INVALID
            `W SLEEP PREP` = "02:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "30", # Duration | M
            `W SLEEP END` = "12:15", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "20", # Duration | M
            `W ALARM` = "No", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "Yes", # logical | Yes/No # INVALID
            `W LIGHT EXPOSURE` = "02:15", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "34:00", # hms | HMS, HM, H [0-24h] # INVALID
            `F SLEEP PREP` = "04:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "No", # Duration | M
            `F SLEEP END` = "14:12", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "30", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "Yes", # logical | Yes/No
            `F REASONS WHY` = "Hobbies", # character
            `F LIGHT EXPOSURE` = "-01:30" # Duration | [H]MS, [H]M, [H] # INV.
        ) %>%

    ## Sleeps more on workdays than on work-free days

        dplyr::add_row(
            `ID` = as.character(reserved_id[5]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "2", # integer | [0-7]
            `W BED TIME` = "21:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "2130", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "15", # Duration | M
            `W SLEEP END` = "09:15", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "10", # Duration | M
            `W ALARM` = "No", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "0500", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "5", # Duration | M
            `F SLEEP END` = "06:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "5", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "Yes", # logical | Yes/No
            `F REASONS WHY` = "Hobbies", # character
            `F LIGHT EXPOSURE` = "07:00" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Uses an alarm clock on work-free days

        dplyr::add_row(
            `ID` = as.character(reserved_id[6]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "5", # integer | [0-7]
            `W BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "15", # Duration | M
            `W SLEEP END` = "07:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "5", # Duration | M
            `W ALARM` = "Yes", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
            `W LIGHT EXPOSURE` = "01:55", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "01:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "01:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "30", # Duration | M
            `F SLEEP END` = "08:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "0", # Duration | M
            `F ALARM` = "Yes", # logical | Yes/No
            `F REASONS` = "Yes", # logical | Yes/No
            `F REASONS WHY` = "Child(ren)/pet(s)", # character
            `F LIGHT EXPOSURE` = "04:45" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Null MCTQ (invalid case)

        dplyr::add_row(
            `ID` = as.character(reserved_id[7]), # integer | [auto-increment]
            `WORK REGULAR` = "", # logical | Yes/No
            `WORK DAYS` = "", # integer | [0-7]
            `W BED TIME` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "", # Duration | M
            `W SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "", # Duration | M
            `W ALARM` = "", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "", # Duration | M
            `F SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "", # Duration | M
            `F ALARM` = "", # logical | Yes/No
            `F REASONS` = "", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Did not answer workdays questions

        dplyr::add_row(
            `ID` = as.character(reserved_id[8]), # integer | [auto-increment]
            `WORK REGULAR` = "No", # logical | Yes/No
            `WORK DAYS` = "", # integer | [0-7]
            `W BED TIME` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "", # Duration | M
            `W SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "", # Duration | M
            `W ALARM` = "", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "03:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "04:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "90", # Duration | M
            `F SLEEP END` = "15:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "30", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "No", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "00:30" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## All, or almost all, basic variables have the same values
    ## (invalid case)

        dplyr::add_row(
            `ID` = as.character(reserved_id[9]), # integer | [auto-increment]
            `WORK REGULAR` = "0", # logical | Yes/No
            `WORK DAYS` = "0", # integer | [0-7]
            `W BED TIME` = "0", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "0", # Duration | M
            `W SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "0", # Duration | M
            `W ALARM` = "0", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "0", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "0", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "0", # Duration | M
            `F SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "0", # Duration | M
            `F ALARM` = "0", # logical | Yes/No
            `F REASONS` = "0", # logical | Yes/No
            `F REASONS WHY` = "0", # character
            `F LIGHT EXPOSURE` = "0" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Works 7 days a week and didn't answer the work-free days section

        dplyr::add_row(
            `ID` = as.character(reserved_id[10]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "7", # integer | [0-7]
            `W BED TIME` = "23:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "23:30", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "10", # Duration | M
            `W SLEEP END` = "06:30", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "5", # Duration | M
            `W ALARM` = "Yes", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
            `W LIGHT EXPOSURE` = "02:00", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "", # Duration | M
            `F SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "", # Duration | M
            `F ALARM` = "", # logical | Yes/No
            `F REASONS` = "", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Suspicious values (removed case)

        dplyr::add_row(
            `ID` = as.character(reserved_id[11]), # integer | [auto-increment]
            `WORK REGULAR` = "No", # logical | Yes/No
            `WORK DAYS` = "6", # integer | [0-7]
            `W BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "120", # Duration | M
            `W SLEEP END` = "04:00", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `W SLEEP INERTIA` = "0", # Duration | M
            `W ALARM` = "Yes", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "Yes", # logical | Yes/No
            `W LIGHT EXPOSURE` = "18:00", # Duration | [H]MS, [H]M, [H] # SUSP.
            `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "30", # Duration | M
            `F SLEEP END` = "09:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "5", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "No", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "17:00" # Duration | [H]MS, [H]M, [H] # SUSP.
        ) %>%

    ## Different formats

        dplyr::add_row(
            `ID` = as.character(reserved_id[12]), # integer | [auto-increment]
            `WORK REGULAR` = "true", # logical | Yes/No # AMBIGUOUS
            `WORK DAYS` = "5", # integer | [0-7]
            `W BED TIME` = "11:00 PM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W SLEEP PREP` = "0000", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W SLEEP LAT` = "00:15", # Duration | M #AMBIGUOUS
            `W SLEEP END` = "07:15 AM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W SLEEP INERTIA` = "30", # Duration | M
            `W ALARM` = "No", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "3", # Duration | [H]MS, [H]M, [H] # AMBIGUOUS
            `F BED TIME` = "01:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "0130 AM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `F SLEEP LAT` = "60", # Duration | M
            `F SLEEP END` = "10:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "00:15", # Duration | M # AMBIGUOUS
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "Yes", # logical | Yes/No
            `F REASONS WHY` = "Hobbies", # character
            `F LIGHT EXPOSURE` = "04:30" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Possible filling error

        dplyr::add_row(
            `ID` = as.character(reserved_id[13]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "6", # integer | [0-7]
            `W BED TIME` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "", # Duration | M
            `W SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "", # Duration | M
            `W ALARM` = "", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "22:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "22:30", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "15", # Duration | M
            `F SLEEP END` = "06:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "3", # Duration | M
            `F ALARM` = "Yes", # logical | Yes/No
            `F REASONS` = "Yes", # logical | Yes/No
            `F REASONS WHY` = "Hobbies", # character
            `F LIGHT EXPOSURE` = "01:30" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Repeated workdays and work-free days values (possible carryover
    ## effect)

        dplyr::add_row(
            `ID` = as.character(reserved_id[14]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "5", # integer | [0-7]
            `W BED TIME` = "22:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "10", # Duration | M
            `W SLEEP END` = "07:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "5", # Duration | M
            `W ALARM` = "Yes", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
            `W LIGHT EXPOSURE` = "01:00", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "22:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "10", # Duration | M
            `F SLEEP END` = "07:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "5", # Duration | M
            `F ALARM` = "Yes", # logical | Yes/No
            `F REASONS` = "No", # logical | Yes/No
            `F REASONS WHY` = "No", # character
            `F LIGHT EXPOSURE` = "01:00" # Duration | [H]MS, [H]M, [H]
        ) %>%

    ## Sleep onset is equal or greater than sleep end
    ## [(s_prep + s_lat) >= se] (invalid case)

        dplyr::add_row(
            `ID` = as.character(reserved_id[15]), # integer | [auto-increment]
            `WORK REGULAR` = "Yes", # logical | Yes/No
            `WORK DAYS` = "2", # integer | [0-7]
            `W BED TIME` = "22:30", # hms | HMS, HM, H [0-24h]
            `W SLEEP PREP` = "00:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP LAT` = "60", # Duration | M
            `W SLEEP END` = "08:00", # hms | HMS, HM, H [0-24h]
            `W SLEEP INERTIA` = "10", # Duration | M
            `W ALARM` = "No", # logical | Yes/No
            `W WAKE BEFORE ALARM` = "", # logical | Yes/No
            `W LIGHT EXPOSURE` = "01:20", # Duration | [H]MS, [H]M, [H]
            `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP PREP` = "02:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP LAT` = "120", # Duration | M
            `F SLEEP END` = "04:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "15", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "No", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "04:00" # Duration | [H]MS, [H]M, [H]
        )

    shift_mctq <- shift_mctq %>% dplyr::arrange(as.integer(.data$`ID`))

    # Write and return output -----

    if (isTRUE(write)) {
        if (!(dir.exists("./inst/extdata/"))) dir.create("./inst/extdata/")

        shift_mctq %>%
            utils::write.csv(paste0("./inst/extdata/", "shift_mctq", ".csv"),
                             row.names = FALSE,
                             quote = FALSE)
    }

    invisible(shift_mctq)
}

#' Tidy [mctq::build_shift_mctq()] output
#'
#' @description
#'
#' `tidy_shift_mctq` tidy the output of [mctq::build_shift_mctq()]. See
#' [mctq::shift_mctq] to learn more.
#'
#' @details
#'
#' Here the process of _tiding_ a dataset is understood as transforming it in
#' input data, like described in Loo and Jonge (2018). It's a very similar
#' process of tiding data described in the workflow proposed by Wickham and
#' Grolemund (n.d.).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' `shift_mctq` data, run [mctq::validate_shift_mctq()].
#'
#' To learn more about the concept of tidy data, _c.f._ Wickham (2014) and
#' Wickham and Grolemund (n.d.).
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `shift_mctq.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @return An invisible `tibble` with a tidied, but not validated,
#'   MCTQ\eqn{^{Shift}}{ Shift} dataset.
#'
#' @template references_e
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
#'
#' @examples
#' \dontrun{
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::tidy_shift_mctq())
#' }}
tidy_shift_mctq <- function(write = FALSE) {
    # Check arguments -----

    checkmate::assert_flag(write)

    # Clean NULL cases -----

    fix_character <- function(x) {
        x <- trimws(x)

        for (i in c("", "NA")) {
            x <- dplyr::na_if(x, i)
        }

        x
    }

    shift_mctq <- build_shift_mctq() %>%
        dplyr::mutate(dplyr::across(.fns = fix_character)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(length =
                          dplyr::n_distinct(dplyr::c_across(-.data$ID))) %>%
        dplyr::mutate(dplyr::across(-.data$ID, .fns = ~ ifelse(length <= 2,
                                                               NA, .x))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-length)

    # Convert columns -----

    pattern_1 <- "^([0-1]\\d|2[0-3])(:)?[0-5]\\d((:)?[0-5]\\d)?"
    pattern_2 <- "^\\d{1,3}$"
    pattern_3 <- "[^A-Za-z]$"
    pattern_4 <- "^([-+])?(2[4-9]|[3-9]\\d|\\d{3,})(:)?[0-5]\\d((:)?[0-5]\\d)?"
    pattern_5 <- "^([-+])?\\d+(:)?[0-5]\\d((:)?[0-5]\\d)?"
    pattern_6 <- "^[0-1][0-2](:)?[0-5]\\d(AM|PM)"
    pattern_7 <- "(AM|PM)$"

    shift_mctq <- shift_mctq %>% dplyr::transmute(
        id = as.integer(.data$`ID`),
        work = dplyr::case_when(
            tolower(.data$`WORK REGULAR`) == "yes" ~ TRUE,
            tolower(.data$`WORK REGULAR`) == "true" ~ TRUE,
            tolower(.data$`WORK REGULAR`) == "no" ~ FALSE),
        wd = as.integer(.data$`WORK DAYS`),
        bt_w = dplyr::case_when(
            grepl(pattern_1, .data$`W BED TIME`, perl = TRUE) ~
                convert_pt(.data$`W BED TIME`, "hms",
                           c("HM", "IMp"), quiet = TRUE),
            grepl(pattern_4, .data$`W BED TIME`, perl = TRUE) ~
                convert_pt(.data$`W BED TIME`, "hms", "HM",
                           quiet = TRUE)),
        sprep_w = convert_pt(.data$`W SLEEP PREP`, "hms",
                             c("HMS", "HM", "H")),
        slat_w = dplyr::case_when(
            grepl(pattern_2, .data$`W SLEEP LAT`, perl = TRUE) ~
                convert_pt(.data$`W SLEEP LAT`, "Duration", "M",
                           quiet = TRUE),
            grepl(pattern_1, .data$`W SLEEP LAT`) ~
                convert_pt(.data$`W SLEEP LAT`, "Duration",
                           c("HMS", "HM", "H"), quiet = TRUE)),
        se_w = convert_pt(.data$`W SLEEP END`, "hms", c("HMS", "HM", "H")),
        si_w = convert_pt(.data$`W SLEEP INERTIA`, "Duration", "M"),
        alarm_w = dplyr::case_when(
            tolower(.data$`W ALARM`) == "yes" ~ TRUE,
            tolower(.data$`W ALARM`) == "no" ~ FALSE),
        wake_before_w = dplyr::case_when(
            tolower(.data$`W WAKE BEFORE ALARM`) == "yes" ~ TRUE,
            tolower(.data$`W WAKE BEFORE ALARM`) == "no" ~ FALSE),
        le_w = convert_pt(.data$`W LIGHT EXPOSURE`, "Duration",
                          c("HMS", "HM", "H")),
        bt_f = dplyr::case_when(
            grepl(pattern_1, .data$`F BED TIME`, perl = TRUE) ~
                convert_pt(.data$`F BED TIME`, "hms", c("HMS", "HM", "H"),
                           quiet = TRUE),
            grepl(pattern_4, .data$`F BED TIME`, perl = TRUE) ~
                convert_pt(.data$`F BED TIME`, "hms", "HM",
                           quiet = TRUE)),
        sprep_f = convert_pt(.data$`F SLEEP PREP`, "hms",
                             c("HMS", "HM", "H")),
        slat_f = convert_pt(.data$`F SLEEP LAT`, "Duration", "M",
                            quiet = TRUE),
        se_f = convert_pt(.data$`F SLEEP END`, "hms", c("HMS", "HM", "H")),
        si_f = dplyr::case_when(
            grepl(pattern_2, .data$`F SLEEP INERTIA`) ~
                convert_pt(.data$`F SLEEP INERTIA`, "Duration", "M",
                           quiet = TRUE),
            grepl(pattern_1, .data$`F SLEEP INERTIA`) ~
                convert_pt(.data$`F SLEEP INERTIA`, "Duration",
                           c("HMS", "HM", "H"), quiet = TRUE)),
        alarm_f = dplyr::case_when(
            tolower(.data$`F ALARM`) == "yes" ~ TRUE,
            tolower(.data$`F ALARM`) == "no" ~ FALSE),
        reasons_f = dplyr::case_when(
            tolower(.data$`F REASONS`) == "yes" ~ TRUE,
            tolower(.data$`F REASONS`) == "no" ~ FALSE),
        reasons_why_f = .data$`F REASONS WHY`,
        le_f = convert_pt(.data$`F LIGHT EXPOSURE`, "Duration", "HM")
    )

    # Write and output dataset -----

    if (isTRUE(write)) usethis::use_data(shift_mctq, overwrite = TRUE)

    invisible(shift_mctq)
}

#' Validate [mctq::tidy_shift_mctq()] output
#'
#' @description
#'
#' `validate_shift_mctq()` validates the output of [mctq::tidy_shift_mctq()].
#' See [mctq::shift_mctq] to learn more.
#'
#' @details
#'
#' Here, the process of _validating_ a dataset is understood as detecting
#' invalid data, by checking whether data satisfies certain assumptions from
#' domain knowledge, to them removing or, if possible, fixing them. You can find
#' more about data validation and error location in Loo and Jonge (2018).
#'
#' This process can be considered as part of the process of transforming data,
#' described in the workflow proposed by Wickham and Grolemund (n.d.).
#'
#' @return An invisible `tibble` with a validated MCTQ\eqn{^{Shift}}{ Shift}
#'   dataset.
#'
#' @inheritParams tidy_shift_mctq
#' @template references_d
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::validate_shift_mctq())
#' }}
validate_shift_mctq <- function(write = FALSE) {
    # To do -----
    #
    # * Adapt this process by using `errorlocate` package with `validate`.

    # Check arguments -----

    checkmate::assert_flag(write)

    # R CMD Check variable bindings fix (see <http://bit.ly/3bliuam>) -----

    dummy <- bkp <- so_i <- sd_i <-  NULL

    # Do univariate validation -----

    hms_0 <- hms::parse_hm("00:00")
    hms_24 <- hms::parse_hm("24:00")
    duration_0 <- lubridate::dhours(0)
    duration_6 <- lubridate::dhours(6)
    duration_24 <- lubridate::dhours(24)

    foo <- function(x) {
        dplyr::case_when(
            x == hms_24 ~ hms_0,
            x >= hms_0 & x < hms_24 ~ x)
    }

    bar <- function(x) {
        dplyr::case_when(
            validate::in_range(x, min = duration_0, max = duration_6) ~ x
        )
    }

    baz <- function(x) {
        dplyr::case_when(
            validate::in_range(x, min = duration_0, max = duration_24) ~ x)
    }

    cols_1 <- c("bt_w", "sprep_w", "se_w", "bt_f", "sprep_f", "se_f")
    cols_2 <- c("slat_w", "si_w", "slat_f", "si_f")
    cols_3 <- c("le_w", "le_f")

    shift_mctq <- tidy_shift_mctq() %>% dplyr::mutate(
        wd = dplyr::case_when(
            validate::in_range(wd, min = 0, max = 7) ~ wd)) %>%
        dplyr::mutate(
            dplyr::across(dplyr::all_of(cols_1), foo),
            dplyr::across(dplyr::all_of(cols_2), bar),
            dplyr::across(dplyr::all_of(cols_3), baz))

    # Do multivariate validation -----

    for (i in c("w", "f")) {
        bt_i <- paste0("bt_", i)
        sprep_i <- paste0("sprep_", i)

        shift_mctq <- shift_mctq %>%
            dplyr::mutate(
                dummy = dplyr::case_when(
                    mctq::assign_date(!!as.symbol(bt_i), !!as.symbol(sprep_i)) >
                        lubridate::dhours(12) ~ TRUE,
                    TRUE ~ FALSE),
                bkp = !!as.symbol(bt_i),
                !!as.symbol(bt_i) :=
                    dplyr::if_else(dummy, !!as.symbol(sprep_i),
                                   !!as.symbol(bt_i)),
                !!as.symbol(sprep_i) :=
                    dplyr::if_else(dummy, bkp, !!as.symbol(sprep_i))) %>%
            dplyr::select(-dummy, -bkp)
    }

    for (i in c("w", "f")) {
        sprep_i <- paste0("sprep_", i)
        slat_i <- paste0("slat_", i)
        se_i <- paste0("se_", i)

        test <- shift_mctq %>%
            dplyr::mutate(
                so_i = so(!!as.symbol(sprep_i), !!as.symbol(slat_i)),
                sd_i = sd(so_i, !!as.symbol(se_i)),
                dummy = dplyr::case_when(
                    sd_i <= lubridate::dhours(2) |
                        sd_i >= lubridate::dhours(18) ~ TRUE,
                    TRUE ~ FALSE)) %>%
            dplyr::select(dummy)

        shift_mctq <- dplyr::bind_cols(shift_mctq, test) %>%
            dplyr::mutate(
                dplyr::across(dplyr::ends_with("_w"),
                              ~ dplyr::if_else(dummy, na_as(.x), .x))) %>%
            dplyr::select(-dummy)
    }

    # Clean invalid cases -----

    ## Cases: "Suspicious values (removed case)" and "Sleep onset is equal or
    ##        greater than sleep end [(s_prep + s_lat) >= se] (invalid case)"

    shift_mctq <- shift_mctq %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            dplyr::across(-.data$id, .fns = ~ dplyr::if_else(
                .data$id %in% c(15, 41), na_as(.x), .x))) %>%
        dplyr::ungroup()

    # Fix/impute linked data -----

    shift_mctq <- shift_mctq %>%
        dplyr::mutate(
            wd = dplyr::case_when(
                work == FALSE & is.na(wd) ~ as.integer(0),
                TRUE ~ wd),
            work = dplyr::case_when(
                work == FALSE & wd > 0 ~ TRUE,
                TRUE ~ work),
            wake_before_w = dplyr::case_when(
                alarm_w == FALSE ~ as.logical(NA),
                TRUE ~ wake_before_w),
            reasons_why_f = dplyr::case_when(
                tolower(reasons_why_f) == "no" ~ as.character(NA),
                TRUE ~ reasons_why_f),
            reasons_f = dplyr::case_when(
                !is.na(reasons_why_f) ~ TRUE,
                TRUE ~ reasons_f)
        )

    # Write and output dataset -----

    if (isTRUE(write)) usethis::use_data(shift_mctq, overwrite = TRUE)

    invisible(shift_mctq)
}

#' Analyze [mctq::validate_shift_mctq()] output
#'
#' @description
#'
#' `analyse_shift_mctq()` computes and creates the non-measured MCTQ variables
#' based on the output of [mctq::validate_shift_mctq()]. See [mctq::shift_mctq]
#' to learn more.
#'
#' @details
#'
#' Computing and creating new variables is part of the process of producing
#' statistics, like described in Loo and Jonge (2018). It's also a part of the
#' process of transforming data, described in the workflow proposed by Wickham
#' and Grolemund (n.d.).
#'
#' @return An invisible `tibble` with all the variables proposed for a
#'   MCTQ\eqn{^{Shift}}{ Shift} dataset.
#'
#' @inheritParams tidy_shift_mctq
#' @inheritParams pretty_mctq
#' @template references_d
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::analyze_shift_mctq())
#' }}
analyze_shift_mctq <- function(write = FALSE, round = TRUE, hms = FALSE) {
    # Check arguments -----

    checkmate::assert_flag(write)
    checkmate::assert_flag(round)
    checkmate::assert_flag(hms)

    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    id <- NULL

    n_w_m <- bt_w_m <- sprep_w_m <- slat_w_m <- so_w_m <- se_w_m <- NULL
    tgu_w_m <- gu_w_m <- alarm_w_m <- reasons_w_m <- reasons_why_w_m <- NULL
    sd_w_m <- tbt_w_m <- msw_m <- nap_w_m <- napo_w_m <- nape_w_m <- NULL
    napd_w_m <- sd24_w_m <- NULL

    n_f_m <- bt_f_m <- sprep_f_m <- slat_f_m <- so_f_m <- se_f_m <- NULL
    tgu_f_m <- gu_f_m <- alarm_f_m <- reasons_f_m <- reasons_why_f_m <- NULL
    sd_f_m <- tbt_f_m <- msf_m <- nap_f_m <- napo_f_m <- nape_f_m <- NULL
    napd_f_m <- sd24_f_m <- NULL

    sd_overall_m <- msf_sc_m <- sjl_rel_m <- sjl_m <- NULL

    n_w_e <- bt_w_e <- sprep_w_e <- slat_w_e <- so_w_e <- se_w_e <- NULL
    tgu_w_e <- gu_w_e <- alarm_w_e <- reasons_w_e <- reasons_why_w_e <- NULL
    sd_w_e <- tbt_w_e <- msw_e <- nap_w_e <- napo_w_e <- nape_w_e <- NULL
    napd_w_e <- sd24_w_e <- NULL

    n_f_e <- bt_f_e <- sprep_f_e <- slat_f_e <- so_f_e <- se_f_e <- NULL
    tgu_f_e <- gu_f_e <- alarm_f_e <- reasons_f_e <- reasons_why_f_e <- NULL
    sd_f_e <- tbt_f_e <- msf_e <- nap_f_e <- napo_f_e <- nape_f_e <- NULL
    napd_f_e <- sd24_f_e <- NULL

    sd_overall_e <- msf_sc_e <- sjl_rel_e <- sjl_e <- NULL

    n_w_n <- bt_w_n <- sprep_w_n <- slat_w_n <- so_w_n <- se_w_n <- NULL
    tgu_w_n <- gu_w_n <- alarm_w_n <- reasons_w_n <- reasons_why_w_n <- NULL
    sd_w_n <- tbt_w_n <- msw_n <- nap_w_n <- napo_w_n <- nape_w_n <- NULL
    napd_w_n <- sd24_w_n <- NULL

    n_f_n <- bt_f_n <- sprep_f_n <- slat_f_n <- so_f_n <- se_f_n <- NULL
    tgu_f_n <- gu_f_n <- alarm_f_n <- reasons_f_n <- reasons_why_f_n <- NULL
    sd_f_n <- tbt_f_n <- msf_n <- nap_f_n <- napo_f_n <- nape_f_n <- NULL
    napd_f_n <- sd24_f_n <- NULL

    sd_overall_n <- msf_sc_n <- sjl_rel_n <- sjl_n <- NULL

    sjl_weighted <- NULL

    # Create computed variables -----

    shift_mctq <- validate_shift_mctq() %>%
        dplyr::mutate(
            so_w_m = so(sprep_w_m, slat_w_m),
            gu_w_m = gu(se_w_m, si_w_m),
            sd_w_m = sd(so_w_m, se_w_m),
            tbt_w_m = tbt(bt_w_m, gu_w_m),
            msw_m = ms(so_w_m, sd_w_m),
            napd_w_m = napd(napo_w_m, nape_w_m),
            sd24_w_m = sd24(sd_w_m, napd_w_m, nap_w_m),

            so_f_m = so(sprep_f_m, slat_f_m),
            gu_f_m = gu(se_f_m, si_f_m),
            sd_f_m = sd(so_f_m, se_f_m),
            tbt_f_m = tbt(bt_f_m, gu_f_m),
            msf_m = ms(so_f_m, sd_f_m),
            napd_f_m = napd(napo_f_m, nape_f_m),
            sd24_f_m = sd24(sd_f_m, napd_f_m, nap_f_m),

            sd_overall_m = sd_overall(sd_w_m, sd_f_m, n_w_m, n_f_m) ,
            msf_sc_m = msf_sc(msf_m, sd_w_m, sd_f_m, sd_overall_m, alarm_f_m),
            sjl_rel_m = sjl_rel(msw_m, msf_m),
            sjl_m = abs(sjl_rel_m),

            so_w_e = so(sprep_w_e, slat_w_e),
            gu_w_e = gu(se_w_e, si_w_e),
            sd_w_e = sd(so_w_e, se_w_e),
            tbt_w_e = tbt(bt_w_e, gu_w_e),
            msw_e = ms(so_w_e, sd_w_e),
            napd_w_e = napd(napo_w_e, nape_w_e),
            sd24_w_e = sd24(sd_w_e, napd_w_e, nap_w_e),

            so_f_e = so(sprep_f_e, slat_f_e),
            gu_f_e = gu(se_f_e, si_f_e),
            sd_f_e = sd(so_f_e, se_f_e),
            tbt_f_e = tbt(bt_f_e, gu_f_e),
            msf_e = ms(so_f_e, sd_f_e),
            napd_f_e = napd(napo_f_e, nape_f_e),
            sd24_f_e = sd24(sd_f_e, napd_f_e, nap_f_e),

            sd_overall_e = sd_overall(sd_w_e, sd_f_e, n_w_e, n_f_e) ,
            msf_sc_e = msf_sc(msf_e, sd_w_e, sd_f_e, sd_overall_e, alarm_f_e),
            sjl_rel_e = sjl_rel(msw_e, msf_e),
            sjl_e = abs(sjl_rel_e),

            so_w_n = so(sprep_w_n, slat_w_n),
            gu_w_n = gu(se_w_n, si_w_n),
            sd_w_n = sd(so_w_n, se_w_n),
            tbt_w_n = tbt(bt_w_n, gu_w_n),
            msw_n = ms(so_w_n, sd_w_n),
            napd_w_n = napd(napo_w_n, nape_w_n),
            sd24_w_n = sd24(sd_w_n, napd_w_n, nap_w_n),

            so_f_n = so(sprep_f_n, slat_f_n),
            gu_f_n = gu(se_f_n, si_f_n),
            sd_f_n = sd(so_f_n, se_f_n),
            tbt_f_n = tbt(bt_f_n, gu_f_n),
            msf_n = ms(so_f_n, sd_f_n),
            napd_f_n = napd(napo_f_n, nape_f_n),
            sd24_f_n = sd24(sd_f_n, napd_f_n, nap_f_n),

            sd_overall_n = sd_overall(sd_w_n, sd_f_n, n_w_n, n_f_n) ,
            msf_sc_n = msf_sc(msf_n, sd_w_n, sd_f_n, sd_overall_n, alarm_f_n),
            sjl_rel_n = sjl_rel(msw_n, msf_n),
            sjl_n = abs(sjl_rel_n),

            sjl_weighted = sjl_weighted(
                sjl = list(sjl_m = sjl_m, sjl_e = sjl_e, sjl_n = sjl_n),
                n = list(n_w_m = n_w_m, n_w_e = n_w_e, n_w_n = n_w_n))) %>%
        dplyr::relocate(
            id,

            n_w_m, bt_w_m, sprep_w_m, slat_w_m, so_w_m, se_w_m, tgu_w_m,
            gu_w_m, alarm_w_m, reasons_w_m, reasons_why_w_m, sd_w_m, tbt_w_m,
            msw_m, nap_w_m, napo_w_m, nape_w_m, napd_w_m, sd24_w_m,

            n_f_m, bt_f_m, sprep_f_m, slat_f_m, so_f_m, se_f_m, tgu_f_m,
            gu_f_m, alarm_f_m, reasons_f_m, reasons_why_f_m, sd_f_m, tbt_f_m,
            msf_m, nap_f_m, napo_f_m, nape_f_m, napd_f_m, sd24_f_m,

            sd_overall_m, msf_sc_m, sjl_rel_m, sjl_m,

            n_w_e, bt_w_e, sprep_w_e, slat_w_e, so_w_e, se_w_e, tgu_w_e,
            gu_w_e, alarm_w_e, reasons_w_e, reasons_why_w_e, sd_w_e, tbt_w_e,
            msw_e, nap_w_e, napo_w_e, nape_w_e, napd_w_e, sd24_w_e,

            n_f_e, bt_f_e, sprep_f_e, slat_f_e, so_f_e, se_f_e, tgu_f_e,
            gu_f_e, alarm_f_e, reasons_f_e, reasons_why_f_e, sd_f_e, tbt_f_e,
            msf_e, nap_f_e, napo_f_e, nape_f_e, napd_f_e, sd24_f_e,

            sd_overall_e, msf_sc_e, sjl_rel_e, sjl_e,

            n_w_n, bt_w_n, sprep_w_n, slat_w_n, so_w_n, se_w_n, tgu_w_n,
            gu_w_n, alarm_w_n, reasons_w_n, reasons_why_w_n, sd_w_n, tbt_w_n,
            msw_n, nap_w_n, napo_w_n, nape_w_n, napd_w_n, sd24_w_n,

            n_f_n, bt_f_n, sprep_f_n, slat_f_n, so_f_n, se_f_n, tgu_f_n,
            gu_f_n, alarm_f_n, reasons_f_n, reasons_why_f_n, sd_f_n, tbt_f_n,
            msf_n, nap_f_n, napo_f_n, nape_f_n, napd_f_n, sd24_f_n,

            sd_overall_n, msf_sc_n, sjl_rel_n, sjl_n,

            sjl_weighted)

    # Make MCTQ pretty -----

    shift_mctq <- shift_mctq %>% pretty_mctq(round = round, hms = hms)

    # Write and output dataset -----

    if (isTRUE(write)) usethis::use_data(shift_mctq, overwrite = TRUE)

    invisible(shift_mctq)
}

# raw <- build_shift_mctq()
# tidy <- tidy_shift_mctq()
# valid <- validate_shift_mctq()
# analysis <- analyze_shift_mctq()
