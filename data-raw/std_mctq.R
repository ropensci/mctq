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

#' Build a fictional standard MCTQ raw dataset
#'
#' @description
#'
#' `build_std_mctq()` builds a fictional raw dataset, __for testing and learning
#' purposes__, composed by basic/measurable variables of the Munich Chronotype
#' Questionnaire (MCTQ) standard version. See [mctq::std_mctq] to learn more.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `std_mctq.csv` file to `"./inst/extdata/"` (default: `FALSE`).
#' @param random_cases (optional) a `logical` value indicating if the function
#'   must add random MCTQ cases besides the core ones.
#'
#' @return An invisible `tibble` with a raw standard MCTQ dataset.
#'
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::build_std_mctq())
#' }}
build_std_mctq <- function(write = FALSE, random_cases = TRUE) {
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

    std_mctq <- dplyr::tibble(
        `ID` = as.character(reserved_id[1]), # integer | [auto-increment]

        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]

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

            std_mctq <- dplyr::bind_rows(std_mctq, random_case)
        }
    }

    ## Inverted values for bed time and sleep preparing on workdays

    std_mctq <- std_mctq %>% dplyr::add_row(
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
            `W WAKE BEFORE ALARM` = "Sim", # logical | Yes/No # INVALID
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
            `F ALARM` = "Yes", # logical | Yes/No # Alarm == "Yes"
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
            `W SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `W SLEEP LAT` = "120", # Duration | M # SUSPICIOUS
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
            `F SLEEP LAT` = "00:60", # Duration | M  # AMBIGUOUS
            `F SLEEP END` = "10:00", # hms | HMS, HM, H [0-24h]
            `F SLEEP INERTIA` = "15", # Duration | M
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
            `F SLEEP PREP` = "02:00", # hms | HMS, HM, H [0-24h] # ERROR
            `F SLEEP LAT` = "120", # Duration | M # ERROR
            `F SLEEP END` = "04:00", # hms | HMS, HM, H [0-24h] # ERROR
            `F SLEEP INERTIA` = "15", # Duration | M
            `F ALARM` = "No", # logical | Yes/No
            `F REASONS` = "No", # logical | Yes/No
            `F REASONS WHY` = "", # character
            `F LIGHT EXPOSURE` = "04:00" # Duration | [H]MS, [H]M, [H]
        )

    std_mctq <- std_mctq %>% dplyr::arrange(as.integer(.data$`ID`))

    # Write and return output -----

    if (isTRUE(write)) {
        if (!(dir.exists("./inst/extdata/"))) dir.create("./inst/extdata/")

        std_mctq %>%
            utils::write.csv(paste0("./inst/extdata/", "std_mctq", ".csv"),
                             row.names = FALSE,
                             quote = FALSE)
    }

    invisible(std_mctq)
}

#' Tidy [mctq::build_std_mctq()] output
#'
#' @description
#'
#' `tidy_std_mctq` tidy the output of [mctq::build_std_mctq()]. See
#' [mctq::std_mctq] to learn more.
#'
#' @details
#'
#' Here the process of _tiding_ a dataset is understood as transforming it in
#' input data, like described in Loo and Jonge (2018). It's a very similar
#' process of tiding data described in the workflow proposed by Wickham and
#' Grolemund (n.d.).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' `std_mctq` data, run [mctq::validate_std_mctq()].
#'
#' To learn more about the concept of tidy data, _c.f._ Wickham (2014) and
#' Wickham and Grolemund (n.d.).
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `std_mctq.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @return An invisible `tibble` with a tidied, but not validated, standard MCTQ
#'   dataset.
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
#'     utils::View(mctq::tidy_std_mctq())
#' }}
tidy_std_mctq <- function(write = FALSE) {
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

    std_mctq <- build_std_mctq() %>%
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

    std_mctq <- std_mctq %>% dplyr::transmute(
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

    if (isTRUE(write)) usethis::use_data(std_mctq, overwrite = TRUE)

    invisible(std_mctq)
}

#' Validate [mctq::tidy_std_mctq()] output
#'
#' @description
#'
#' `validate_std_mctq()` validates the output of [mctq::tidy_std_mctq()].
#' See [mctq::std_mctq] to learn more.
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
#' @return An invisible `tibble` with a validated standard MCTQ dataset.
#'
#' @inheritParams tidy_std_mctq
#' @template references_d
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::validate_std_mctq())
#' }}
validate_std_mctq <- function(write = FALSE) {
    # To do -----
    #
    # * Adapt this process by using `errorlocate` package with `validate`.

    # Check arguments -----

    checkmate::assert_flag(write)

    # R CMD Check variable bindings fix (see <http://bit.ly/3bliuam>) -----

    dummy <- bkp <- so_i <- sd_i <-  NULL

    # Set values -----

    set.seed(1)
    reserved_id <- sample(50, 15)

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

    std_mctq <- tidy_std_mctq() %>% dplyr::mutate(
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

        std_mctq <- std_mctq %>%
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

        test <- std_mctq %>%
            dplyr::mutate(
                so_i = so(!!as.symbol(sprep_i), !!as.symbol(slat_i)),
                sd_i = sd(so_i, !!as.symbol(se_i)),
                dummy = dplyr::case_when(
                    sd_i <= lubridate::dhours(1) |
                        sd_i >= lubridate::dhours(18) ~ TRUE,
                    TRUE ~ FALSE)) %>%
            dplyr::select(dummy)

        std_mctq <- dplyr::bind_cols(std_mctq, test) %>%
            dplyr::mutate(
                dplyr::across(dplyr::ends_with("_w"),
                              ~ dplyr::if_else(dummy, na_as(.x), .x))) %>%
            dplyr::select(-dummy)
    }

    # Clean invalid cases -----

    ## Cases: "Suspicious values (removed case)" and "Sleep onset is equal or
    ##        greater than sleep end [(s_prep + s_lat) >= se] (invalid case)"

    invalid <- c(reserved_id[11], reserved_id[15])

    std_mctq <- std_mctq %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            dplyr::across(-.data$id, .fns = ~ dplyr::if_else(
                .data$id %in% invalid, na_as(.x), .x))) %>%
        dplyr::ungroup()

    # Fix/impute linked data -----

    std_mctq <- std_mctq %>%
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

    if (isTRUE(write)) usethis::use_data(std_mctq, overwrite = TRUE)

    invisible(std_mctq)
}

#' Analyze [mctq::validate_std_mctq()] output
#'
#' @description
#'
#' `analyse_std_mctq()` computes and creates the non-measured MCTQ variables
#' based on the output of [mctq::validate_std_mctq()]. See [mctq::std_mctq] to
#' learn more.
#'
#' @details
#'
#' Computing and creating new variables is part of the process of producing
#' statistics, like described in Loo and Jonge (2018). It's also a part of the
#' process of transforming data, described in the workflow proposed by Wickham
#' and Grolemund (n.d.).
#'
#' @return An invisible `tibble` with all the variables proposed for a standard
#'   MCTQ dataset.
#'
#' @inheritParams tidy_std_mctq
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
#'     utils::View(mctq::analyze_std_mctq())
#' }}
analyze_std_mctq <- function(write = FALSE, round = TRUE, hms = FALSE) {
    # Check arguments -----

    checkmate::assert_flag(write)
    checkmate::assert_flag(round)
    checkmate::assert_flag(hms)

    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    id <- NULL

    work <- wd <- fd <- NULL

    bt_w <- sprep_w <- slat_w <- so_w <- se_w <- si_w <- gu_w <- alarm_w <- NULL
    wake_before_w <- sd_w <- tbt_w <- msw <- le_w <- NULL

    bt_f <- sprep_f <- slat_f <- so_f <- se_f <- si_f <- gu_f <- alarm_f <- NULL
    reasons_f <- reasons_why_f <- sd_f <- tbt_f <- msf <- le_f <- NULL

    sd_week <- msf_sc <- sloss_week <- sjl_rel <- sjl <- le_week <- NULL

    dummy_0_a <- dummy_0_b <- dummy_0_c <- dummy_7_a <- dummy_7_b <- NULL
    dummy_0 <- dummy_7 <- NULL

    # Create computed variables -----

    std_mctq <- validate_std_mctq() %>%
        dplyr::mutate(
            fd = fd(wd),
            so_w = so(sprep_w, slat_w),
            gu_w = gu(se_w, si_w),
            sd_w = sd(so_w, se_w),
            tbt_w = tbt(bt_w, gu_w),
            msw = ms(so_w, sd_w),

            so_f = so(sprep_f, slat_f),
            gu_f = gu(se_f, si_f),
            sd_f = sd(so_f, se_f),
            tbt_f = tbt(bt_f, gu_f),
            msf = ms(so_f, sd_f),

            sd_week = sd_week(sd_w, sd_f, wd),
            msf_sc = msf_sc(msf, sd_w, sd_f, sd_week, alarm_f),
            sloss_week = sloss_week(sd_w, sd_f, wd),
            sjl_rel = sjl_rel(msw, msf),
            sjl = abs(sjl_rel),
            le_week = le_week(le_w, le_f, wd)) %>%
        dplyr::relocate(
            id, work, wd, fd,

            bt_w, sprep_w, slat_w, so_w, se_w, si_w, gu_w, alarm_w,
            wake_before_w, sd_w, tbt_w, msw, le_w,

            bt_f, sprep_f, slat_f, so_f, se_f, si_f, gu_f, alarm_f,
            reasons_f, reasons_why_f, sd_f, tbt_f, msf, le_f,

            sd_week, msf_sc, sloss_week, sjl_rel, sjl, le_week)

    # Fix missing sections -----

    ## See `vignette("missing_sections", "mctq")` to learn more.

    count_w <- length(names(std_mctq)[grepl("_w$", names(std_mctq))])
    count_f <- length(names(std_mctq)[grepl("_f$", names(std_mctq))])
    count_w <- count_w * 2/3
    count_f <- count_f * 2/3

    test <- std_mctq %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            dummy_0_a = as.integer(wd) == 0,
            dummy_0_b = count_na(
                dplyr::c_across(dplyr::ends_with("_w"))) >= count_w,
            dummy_0_c = alarm_f == FALSE,
            dummy_7_a = as.integer(wd) == 7,
            dummy_7_b = count_na(
                dplyr::c_across(dplyr::ends_with("_f"))) >= count_f,
            dummy_0 = dummy_0_a & dummy_0_b & dummy_0_c & dummy_7_b == FALSE,
            dummy_7 = dummy_7_a & dummy_7_b & dummy_0_b == FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::select(dummy_0, dummy_7)

    std_mctq <- dplyr::bind_cols(std_mctq, test) %>%
        dplyr::mutate(
            sd_week = dplyr::case_when(
                dummy_0 == TRUE ~ sd_f,
                dummy_7 == TRUE ~ sd_w,
                TRUE ~ sd_week),
            msf_sc = dplyr::if_else(dummy_0, msf, msf_sc),
            sloss_week = dplyr::if_else(dummy_0, lubridate::dhours(0),
                                        sloss_week),
            sjl_rel = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_rel),
            sjl = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl),
            le_week = dplyr::case_when(
                dummy_0 == TRUE ~ le_f,
                dummy_7 == TRUE ~ le_w,
                TRUE ~ le_week)) %>%
        dplyr::select(-dummy_0, -dummy_7)

    # Make MCTQ pretty -----

    std_mctq <- std_mctq %>% pretty_mctq(round = round, hms = hms)

    # Write and output dataset -----

    if (isTRUE(write)) usethis::use_data(std_mctq, overwrite = TRUE)

    invisible(std_mctq)
}

# raw <- build_std_mctq()
# tidy <- tidy_std_mctq()
# valid <- validate_std_mctq()
# analysis <- analyze_std_mctq()
