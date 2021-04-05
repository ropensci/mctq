# Source the file before running the functions
# Don't forget to uncomment the `library` functions below

# library(checkmate)
# library(dplyr)
# library(hms)
# library(lubridate)
# library(magrittr)
# library(mctq)
# library(rlang)
# library(stringr)
# library(usethis)
# library(utils)
# library(validate)

#' Build a fictional MCTQ\eqn{^{Shift}}{ Shift} raw dataset
#'
#' @description
#'
#' `build_shift_mctq()` builds a fictional raw dataset, __for testing and
#' learning purposes__, composed of basic/measurable variables of the Munich
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
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! :=
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
    reserved_id <- sample(50, 11)
    id <- seq(50)[!(seq(50) %in% reserved_id)]

    # Create cases -----

    ## Base subject: sleeps less than the recommended on workdays and
    ##               stretches during work-free days

    shift_mctq <- dplyr::tibble(
        `ID` = as.character(reserved_id[1]), # integer | [auto-increment]

        `W M N DAYS` = "6", # integer | [0-7]
        `W M BEDTIME` = "23:20", # hms | HMS, HM, H [0-24h]
        `W M SLEEP PREP` = "00:05", # hms | HMS, HM, H [0-24h]
        `W M SLEEP LAT` = "5", # Duration | M
        `W M SLEEP END` = "03:40", # hms | HMS, HM, H [0-24h]
        `W M TIME GU` = "10", # Duration | M
        `W M ALARM` = "Yes", # logical | Yes/No
        `W M REASONS` = "No", # logical | Yes/No
        `W M REASONS WHY` = "", # character
        `W M NAP` = "Yes", # logical | Yes/No
        `W M NAP ONSET` = "14:00", # hms | HMS, HM, H [0-24h]
        `W M NAP END` = "14:40", # hms | HMS, HM, H [0-24h]

        `F M N DAYS` = "2", # integer | [0-7]
        `F M BEDTIME` = "23:05", # hms | HMS, HM, H [0-24h]
        `F M SLEEP PREP` = "00:20", # hms | HMS, HM, H [0-24h]
        `F M SLEEP LAT` = "10", # Duration | M
        `F M SLEEP END` = "10:00", # hms | HMS, HM, H [0-24h]
        `F M TIME GU` = "25", # Duration | M
        `F M ALARM` = "No", # logical | Yes/No
        `F M REASONS` = "Yes", # logical | Yes/No
        `F M REASONS WHY` = "Child(ren)/pet(s)", # character
        `F M NAP` = "No", # logical | Yes/No
        `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
        `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

        `W E N DAYS` = "4", # integer | [0-7]
        `W E BEDTIME` = "01:45", # hms | HMS, HM, H [0-24h]
        `W E SLEEP PREP` = "02:50", # hms | HMS, HM, H [0-24h]
        `W E SLEEP LAT` = "30", # Duration | M
        `W E SLEEP END` = "08:15", # hms | HMS, HM, H [0-24h]
        `W E TIME GU` = "10", # Duration | M
        `W E ALARM` = "Yes", # logical | Yes/No
        `W E REASONS` = "Yes", # logical | Yes/No
        `W E REASONS WHY` = "Child(ren)/pet(s)", # character
        `W E NAP` = "No", # logical | Yes/No
        `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
        `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

        `F E N DAYS` = "2", # integer | [0-7]
        `F E BEDTIME` = "00:55", # hms | HMS, HM, H [0-24h]
        `F E SLEEP PREP` = "02:00", # hms | HMS, HM, H [0-24h]
        `F E SLEEP LAT` = "35", # Duration | M
        `F E SLEEP END` = "11:20", # hms | HMS, HM, H [0-24h]
        `F E TIME GU` = "15", # Duration | M
        `F E ALARM` = "No", # logical | Yes/No
        `F E REASONS` = "Yes", # logical | Yes/No
        `F E REASONS WHY` = "Hobbies", # character
        `F E NAP` = "Yes", # logical | Yes/No
        `F E NAP ONSET` = "18:40", # hms | HMS, HM, H [0-24h]
        `F E NAP END` = "19:20", # hms | HMS, HM, H [0-24h]

        `W N N DAYS` = "6", # integer | [0-7]
        `W N BEDTIME` = "07:30", # hms | HMS, HM, H [0-24h]
        `W N SLEEP PREP` = "07:55", # hms | HMS, HM, H [0-24h]
        `W N SLEEP LAT` = "55", # Duration | M
        `W N SLEEP END` = "12:35", # hms | HMS, HM, H [0-24h]
        `W N TIME GU` = "25", # Duration | M
        `W N ALARM` = "Yes", # logical | Yes/No
        `W N REASONS` = "No", # logical | Yes/No
        `W N REASONS WHY` = "", # character
        `W N NAP` = "No", # logical | Yes/No
        `W N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
        `W N NAP END` = "", # hms | HMS, HM, H [0-24h]

        `F N N DAYS` = "8", # integer | [0-7]
        `F N BEDTIME` = "21:30", # hms | HMS, HM, H [0-24h]
        `F N SLEEP PREP` = "22:55", # hms | HMS, HM, H [0-24h]
        `F N SLEEP LAT` = "75", # Duration | M
        `F N SLEEP END` = "06:20", # hms | HMS, HM, H [0-24h]
        `F N TIME GU` = "30", # Duration | M
        `F N ALARM` = "No", # logical | Yes/No
        `F N REASONS` = "No", # logical | Yes/No
        `F N REASONS WHY` = "", # character
        `F N NAP` = "Yes", # logical | Yes/No
        `F N NAP ONSET` = "12:45", # hms | HMS, HM, H [0-24h]
        `F N NAP END` = "13:10" # hms | HMS, HM, H [0-24h]
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
                random_mctq(model = "shift", quiet = TRUE))

            values <- list(
                w_m = c("W M", "_w_m"),
                f_m = c("F M", "_f_m"),
                w_e = c("W E", "_w_e"),
                f_r = c("F E", "_f_e"),
                w_n = c("W N", "_w_n"),
                f_n = c("F N", "_f_n")
            )

            random_case <- random_case %>%
                dplyr::mutate(`ID` = as.character(i)) %>%
                dplyr::relocate(.data$`ID`, .before = .data$n_w_m)

            for (i in values) {
                random_case <- random_case %>%
                    dplyr::mutate(
                        !!as.symbol(paste(i[1], "N DAYS")) :=
                            as.character(.data[[paste0("n", i[2])]]),
                        !!as.symbol(paste(i[1], "BEDTIME")) :=
                            format_hms(.data[[paste0("bt", i[2])]]),
                        !!as.symbol(paste(i[1], "SLEEP PREP")) :=
                            format_hms(.data[[paste0("sprep", i[2])]]),
                        !!as.symbol(paste(i[1], "SLEEP LAT")) :=
                            format_duration(.data[[paste0("slat", i[2])]]),
                        !!as.symbol(paste(i[1], "SLEEP END")) :=
                            format_hms(.data[[paste0("se", i[2])]]),
                        !!as.symbol(paste(i[1], "TIME GU")) :=
                            format_duration(.data[[paste0("tgu", i[2])]]),
                        !!as.symbol(paste(i[1], "ALARM")) :=
                            format_logical(.data[[paste0("alarm", i[2])]]),
                        !!as.symbol(paste(i[1], "REASONS")) :=
                            format_logical(.data[[paste0("reasons", i[2])]]),
                        !!as.symbol(paste(i[1], "REASONS WHY")) :=
                            as.character(.data[[paste0("reasons_why", i[2])]]),
                        !!as.symbol(paste(i[1], "NAP")) :=
                            format_logical(.data[[paste0("nap", i[2])]]),
                        !!as.symbol(paste(i[1], "NAP ONSET")) :=
                            format_hms(.data[[paste0("napo", i[2])]]),
                        !!as.symbol(paste(i[1], "NAP END")) :=
                            format_hms(.data[[paste0("nape", i[2])]])

                    ) %>%
                    dplyr::select(-dplyr::ends_with(i[2]))
            }

            shift_mctq <- dplyr::bind_rows(shift_mctq, random_case)
        }
    }

    ## Inverted values for bedtime and sleep preparation

    shift_mctq <- shift_mctq %>% dplyr::add_row(
        `ID` = as.character(reserved_id[2]), # integer | [auto-increment]

        `W M N DAYS` = "6", # integer | [0-7]
        `W M BEDTIME` = "20:20", # hms | HMS, HM, H [0-24h]  # INVERSION
        `W M SLEEP PREP` = "19:40", # hms | HMS, HM, H [0-24h]  # INVERSION
        `W M SLEEP LAT` = "5", # Duration | M
        `W M SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
        `W M TIME GU` = "5", # Duration | M
        `W M ALARM` = "Yes", # logical | Yes/No
        `W M REASONS` = "Yes", # logical | Yes/No
        `W M REASONS WHY` = "Hobbies", # character
        `W M NAP` = "Yes", # logical | Yes/No
        `W M NAP ONSET` = "13:30", # hms | HMS, HM, H [0-24h]
        `W M NAP END` = "14:10", # hms | HMS, HM, H [0-24h]

        `F M N DAYS` = "2", # integer | [0-7]
        `F M BEDTIME` = "23:25", # hms | HMS, HM, H [0-24h]
        `F M SLEEP PREP` = "23:50", # hms | HMS, HM, H [0-24h]
        `F M SLEEP LAT` = "20", # Duration | M
        `F M SLEEP END` = "09:05", # hms | HMS, HM, H [0-24h]
        `F M TIME GU` = "70", # Duration | M
        `F M ALARM` = "No", # logical | Yes/No
        `F M REASONS` = "No", # logical | Yes/No
        `F M REASONS WHY` = "", # character
        `F M NAP` = "No", # logical | Yes/No
        `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
        `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

        `W E N DAYS` = "4", # integer | [0-7]
        `W E BEDTIME` = "00:25", # hms | HMS, HM, H [0-24h]
        `W E SLEEP PREP` = "01:05", # hms | HMS, HM, H [0-24h]
        `W E SLEEP LAT` = "15", # Duration | M
        `W E SLEEP END` = "08:15", # hms | HMS, HM, H [0-24h]
        `W E TIME GU` = "25", # Duration | M
        `W E ALARM` = "No", # logical | Yes/No
        `W E REASONS` = "No", # logical | Yes/No
        `W E REASONS WHY` = "", # character
        `W E NAP` = "Yes", # logical | Yes/No
        `W E NAP ONSET` = "19:25", # hms | HMS, HM, H [0-24h]
        `W E NAP END` = "19:55", # hms | HMS, HM, H [0-24h]

        `F E N DAYS` = "2", # integer | [0-7]
        `F E BEDTIME` = "02:25", # hms | HMS, HM, H [0-24h] # INVERSION
        `F E SLEEP PREP` = "01:55", # hms | HMS, HM, H [0-24h] # INVERSION
        `F E SLEEP LAT` = "25", # Duration | M
        `F E SLEEP END` = "08:55", # hms | HMS, HM, H [0-24h]
        `F E TIME GU` = "25", # Duration | M
        `F E ALARM` = "No", # logical | Yes/No
        `F E REASONS` = "No", # logical | Yes/No
        `F E REASONS WHY` = "", # character
        `F E NAP` = "Yes", # logical | Yes/No
        `F E NAP ONSET` = "15:35", # hms | HMS, HM, H [0-24h]
        `F E NAP END` = "16:10", # hms | HMS, HM, H [0-24h]

        `W N N DAYS` = "6", # integer | [0-7]
        `W N BEDTIME` = "08:00", # hms | HMS, HM, H [0-24h]
        `W N SLEEP PREP` = "08:15", # hms | HMS, HM, H [0-24h]
        `W N SLEEP LAT` = "35", # Duration | M
        `W N SLEEP END` = "15:10", # hms | HMS, HM, H [0-24h]
        `W N TIME GU` = "35", # Duration | M
        `W N ALARM` = "No", # logical | Yes/No
        `W N REASONS` = "No", # logical | Yes/No
        `W N REASONS WHY` = "", # character
        `W N NAP` = "Yes", # logical | Yes/No
        `W N NAP ONSET` = "01:55", # hms | HMS, HM, H [0-24h]
        `W N NAP END` = "02:40", # hms | HMS, HM, H [0-24h]

        `F N N DAYS` = "8", # integer | [0-7]
        `F N BEDTIME` = "18:05", # hms | HMS, HM, H [0-24h]
        `F N SLEEP PREP` = "18:45", # hms | HMS, HM, H [0-24h]
        `F N SLEEP LAT` = "75", # Duration | M
        `F N SLEEP END` = "02:00", # hms | HMS, HM, H [0-24h]
        `F N TIME GU` = "45", # Duration | M
        `F N ALARM` = "No", # logical | Yes/No
        `F N REASONS` = "No", # logical | Yes/No
        `F N REASONS WHY` = "", # character
        `F N NAP` = "No", # logical | Yes/No
        `F N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
        `F N NAP END` = "" # hms | HMS, HM, H [0-24h]
    ) %>%

    ## Presence of invalid values

        dplyr::add_row(
            `ID` = as.character(reserved_id[3]), # integer | [auto-increment]

            `W M N DAYS` = "6", # integer | [0-7]
            `W M BEDTIME` = "21:30", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "22:25", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "20", # Duration | M
            `W M SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "5", # Duration | M
            `W M ALARM` = "Yes", # logical | Yes/No
            `W M REASONS` = "Yes", # logical | Yes/No
            `W M REASONS WHY` = "Hobbies", # character
            `W M NAP` = "Yes", # logical | Yes/No
            `W M NAP ONSET` = "11:20", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "11:45", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "2", # integer | [0-7]
            `F M BEDTIME` = "00:45", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "35:00", # hms | HMS, HM, H [0-24h] # INVALID
            `F M SLEEP LAT` = "30", # Duration | M
            `F M SLEEP END` = "08:40", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "10", # Duration | M
            `F M ALARM` = "No", # logical | Yes/No
            `F M REASONS` = "No", # logical | Yes/No
            `F M REASONS WHY` = "", # character
            `F M NAP` = "Yes", # logical | Yes/No
            `F M NAP ONSET` = "17:35", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "18:05", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "4", # integer | [0-7]
            `W E BEDTIME` = "00:30", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "00:55", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "10", # Duration | M
            `W E SLEEP END` = "08:20", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "30", # Duration | M
            `W E ALARM` = "Yes", # logical | Yes/No
            `W E REASONS` = "No", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "No", # logical | Yes/No
            `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "2", # integer | [0-7]
            `F E BEDTIME` = "01:00", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "01:05", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "30", # Duration | M
            `F E SLEEP END` = "09:20", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "10", # Duration | M
            `F E ALARM` = "No", # logical | Yes/No
            `F E REASONS` = "Yes", # logical | Yes/No
            `F E REASONS WHY` = "Hobbies", # character
            `F E NAP` = "Yes", # logical | Yes/No
            `F E NAP ONSET` = "14:35", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "14:55", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "06:05", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "06:50", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "30", # Duration | M
            `W N SLEEP END` = "42:05", # hms | HMS, HM, H [0-24h] # INVALID
            `W N TIME GU` = "30", # Duration | M
            `W N ALARM` = "Yes", # logical | Yes/No
            `W N REASONS` = "No", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "Yes", # logical | Yes/No
            `W N NAP ONSET` = "03:30", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "04:20", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "8", # integer | [0-7]
            `F N BEDTIME` = "21:30", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "21:55", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "50", # Duration | M
            `F N SLEEP END` = "01:55", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "40", # Duration | M
            `F N ALARM` = "No", # logical | Yes/No
            `F N REASONS` = "Yes", # logical | Yes/No
            `F N REASONS WHY` = "Hobbies", # character
            `F N NAP` = "No", # logical | Yes/No
            `F N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Uses an alarm clock on work-free days

        dplyr::add_row(
            `ID` = as.character(reserved_id[4]), # integer | [auto-increment]

            `W M N DAYS` = "6", # integer | [0-7]
            `W M BEDTIME` = "23:25", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "23:35", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "30", # Duration | M
            `W M SLEEP END` = "04:15", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "5", # Duration | M
            `W M ALARM` = "Yes", # logical | Yes/No
            `W M REASONS` = "No", # logical | Yes/No
            `W M REASONS WHY` = "", # character
            `W M NAP` = "Yes", # logical | Yes/No
            `W M NAP ONSET` = "15:55", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "16:30", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "2", # integer | [0-7]
            `F M BEDTIME` = "01:25", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "01:50", # hms | HMS, HM, H [0-24h]
            `F M SLEEP LAT` = "30", # Duration | M
            `F M SLEEP END` = "08:05", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "65", # Duration | M
            `F M ALARM` = "Yes", # logical | Yes/No # ALARM == "Yes"
            `F M REASONS` = "Yes", # logical | Yes/No
            `F M REASONS WHY` = "Hobbies", # character
            `F M NAP` = "No", # logical | Yes/No
            `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "4", # integer | [0-7]
            `W E BEDTIME` = "23:10", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "23:40", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "30", # Duration | M
            `W E SLEEP END` = "05:45", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "30", # Duration | M
            `W E ALARM` = "No", # logical | Yes/No
            `W E REASONS` = "No", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "No", # logical | Yes/No
            `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "2", # integer | [0-7]
            `F E BEDTIME` = "23:10", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "00:15", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "35", # Duration | M
            `F E SLEEP END` = "08:45", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "5", # Duration | M
            `F E ALARM` = "Yes", # logical | Yes/No # ALARM == "Yes"
            `F E REASONS` = "No", # logical | Yes/No
            `F E REASONS WHY` = "", # character
            `F E NAP` = "Yes", # logical | Yes/No
            `F E NAP ONSET` = "17:20", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "18:00", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "07:10", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "07:20", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "15", # Duration | M
            `W N SLEEP END` = "12:40", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "20", # Duration | M
            `W N ALARM` = "No", # logical | Yes/No
            `W N REASONS` = "No", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "Yes", # logical | Yes/No
            `W N NAP ONSET` = "23:45", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "00:00", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "8", # integer | [0-7]
            `F N BEDTIME` = "22:25", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "22:50", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "80", # Duration | M
            `F N SLEEP END` = "10:55", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "35", # Duration | M
            `F N ALARM` = "Yes", # logical | Yes/No # ALARM == "Yes"
            `F N REASONS` = "No", # logical | Yes/No
            `F N REASONS WHY` = "", # character
            `F N NAP` = "Yes", # logical | Yes/No
            `F N NAP ONSET` = "17:50", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "18:25" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Null MCTQ

        dplyr::add_row(
            `ID` = as.character(reserved_id[5]), # integer | [auto-increment]

            `W M N DAYS` = "", # integer | [0-7]
            `W M BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "", # Duration | M
            `W M SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "", # Duration | M
            `W M ALARM` = "", # logical | Yes/No
            `W M REASONS` = "", # logical | Yes/No
            `W M REASONS WHY` = "", # character
            `W M NAP` = "", # logical | Yes/No
            `W M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "", # integer | [0-7]
            `F M BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F M SLEEP LAT` = "", # Duration | M
            `F M SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "", # Duration | M
            `F M ALARM` = "", # logical | Yes/No
            `F M REASONS` = "", # logical | Yes/No
            `F M REASONS WHY` = "", # character
            `F M NAP` = "", # logical | Yes/No
            `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "", # integer | [0-7]
            `W E BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "", # Duration | M
            `W E SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "", # Duration | M
            `W E ALARM` = "", # logical | Yes/No
            `W E REASONS` = "", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "", # logical | Yes/No
            `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "", # integer | [0-7]
            `F E BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "", # Duration | M
            `F E SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "", # Duration | M
            `F E ALARM` = "", # logical | Yes/No
            `F E REASONS` = "", # logical | Yes/No
            `F E REASONS WHY` = "", # character
            `F E NAP` = "", # logical | Yes/No
            `F E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "", # integer | [0-7]
            `W N BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "", # Duration | M
            `W N SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "", # Duration | M
            `W N ALARM` = "", # logical | Yes/No
            `W N REASONS` = "", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "", # logical | Yes/No
            `W N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "", # integer | [0-7]
            `F N BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "", # Duration | M
            `F N SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "", # Duration | M
            `F N ALARM` = "", # logical | Yes/No
            `F N REASONS` = "", # logical | Yes/No
            `F N REASONS WHY` = "", # character
            `F N NAP` = "", # logical | Yes/No
            `F N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## All, or almost all, basic variables have the same values

        dplyr::add_row(
            `ID` = as.character(reserved_id[6]), # integer | [auto-increment]

            `W M N DAYS` = "0", # integer | [0-7]
            `W M BEDTIME` = "0", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "0", # Duration | M
            `W M SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "0", # Duration | M
            `W M ALARM` = "0", # logical | Yes/No
            `W M REASONS` = "0", # logical | Yes/No
            `W M REASONS WHY` = "0", # character
            `W M NAP` = "0", # logical | Yes/No
            `W M NAP ONSET` = "0", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "0", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "0", # integer | [0-7]
            `F M BEDTIME` = "0", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `F M SLEEP LAT` = "0", # Duration | M
            `F M SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "0", # Duration | M
            `F M ALARM` = "0", # logical | Yes/No
            `F M REASONS` = "0", # logical | Yes/No
            `F M REASONS WHY` = "0", # character
            `F M NAP` = "0", # logical | Yes/No
            `F M NAP ONSET` = "0", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "0", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "0", # integer | [0-7]
            `W E BEDTIME` = "0", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "0", # Duration | M
            `W E SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "0", # Duration | M
            `W E ALARM` = "0", # logical | Yes/No
            `W E REASONS` = "0", # logical | Yes/No
            `W E REASONS WHY` = "0", # character
            `W E NAP` = "0", # logical | Yes/No
            `W E NAP ONSET` = "0", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "0", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "0", # integer | [0-7]
            `F E BEDTIME` = "0", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "0", # Duration | M
            `F E SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "0", # Duration | M
            `F E ALARM` = "0", # logical | Yes/No
            `F E REASONS` = "0", # logical | Yes/No
            `F E REASONS WHY` = "0", # character
            `F E NAP` = "0", # logical | Yes/No
            `F E NAP ONSET` = "0", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "0", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "0", # integer | [0-7]
            `W N BEDTIME` = "0", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "0", # Duration | M
            `W N SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "0", # Duration | M
            `W N ALARM` = "0", # logical | Yes/No
            `W N REASONS` = "0", # logical | Yes/No
            `W N REASONS WHY` = "0", # character
            `W N NAP` = "0", # logical | Yes/No
            `W N NAP ONSET` = "0", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "0", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "0", # integer | [0-7]
            `F N BEDTIME` = "0", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "0", # Duration | M
            `F N SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "0", # Duration | M
            `F N ALARM` = "0", # logical | Yes/No
            `F N REASONS` = "0", # logical | Yes/No
            `F N REASONS WHY` = "0", # character
            `F N NAP` = "0", # logical | Yes/No
            `F N NAP ONSET` = "0", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "0" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Suspicious values

        dplyr::add_row(
            `ID` = as.character(reserved_id[7]), # integer | [auto-increment]

            `W M N DAYS` = "6", # integer | [0-7]
            `W M BEDTIME` = "02:30", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `W M SLEEP PREP` = "04:00", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `W M SLEEP LAT` = "20", # Duration | M
            `W M SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `W M TIME GU` = "5", # Duration | M
            `W M ALARM` = "Yes", # logical | Yes/No
            `W M REASONS` = "Yes", # logical | Yes/No
            `W M REASONS WHY` = "Hobbies", # character
            `W M NAP` = "Yes", # logical | Yes/No
            `W M NAP ONSET` = "11:20", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "11:45", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "2", # integer | [0-7]
            `F M BEDTIME` = "11:00", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `F M SLEEP PREP` = "12:30", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `F M SLEEP LAT` = "30", # Duration | M
            `F M SLEEP END` = "16:30", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `F M TIME GU` = "10", # Duration | M
            `F M ALARM` = "No", # logical | Yes/No
            `F M REASONS` = "No", # logical | Yes/No
            `F M REASONS WHY` = "", # character
            `F M NAP` = "Yes", # logical | Yes/No
            `F M NAP ONSET` = "17:35", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "18:05", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "4", # integer | [0-7]
            `W E BEDTIME` = "00:30", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "00:55", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "10", # Duration | M
            `W E SLEEP END` = "08:20", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "30", # Duration | M
            `W E ALARM` = "Yes", # logical | Yes/No
            `W E REASONS` = "No", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "No", # logical | Yes/No
            `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "2", # integer | [0-7]
            `F E BEDTIME` = "01:00", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "01:05", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "30", # Duration | M
            `F E SLEEP END` = "09:20", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "10", # Duration | M
            `F E ALARM` = "No", # logical | Yes/No
            `F E REASONS` = "Yes", # logical | Yes/No
            `F E REASONS WHY` = "Hobbies", # character
            `F E NAP` = "Yes", # logical | Yes/No
            `F E NAP ONSET` = "10:00", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
            `F E NAP END` = "19:00", # hms | HMS, HM, H [0-24h] # SUSPICIOUS

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "06:05", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "06:50", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "30", # Duration | M
            `W N SLEEP END` = "16:05", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "30", # Duration | M
            `W N ALARM` = "Yes", # logical | Yes/No
            `W N REASONS` = "No", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "Yes", # logical | Yes/No
            `W N NAP ONSET` = "03:30", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "04:20", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "8", # integer | [0-7]
            `F N BEDTIME` = "21:30", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "21:55", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "50", # Duration | M
            `F N SLEEP END` = "01:55", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "40", # Duration | M
            `F N ALARM` = "No", # logical | Yes/No
            `F N REASONS` = "Yes", # logical | Yes/No
            `F N REASONS WHY` = "Hobbies", # character
            `F N NAP` = "No", # logical | Yes/No
            `F N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Different formats

        dplyr::add_row(
            `ID` = as.character(reserved_id[8]), # integer | [auto-increment]

            `W M N DAYS` = "6", # integer | [0-7]
            `W M BEDTIME` = "11:25 PM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W M SLEEP PREP` = "2335", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W M SLEEP LAT` = "30", # Duration | M
            `W M SLEEP END` = "0415", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W M TIME GU` = "5", # Duration | M
            `W M ALARM` = "Yes", # logical | Yes/No
            `W M REASONS` = "No", # logical | Yes/No
            `W M REASONS WHY` = "", # character
            `W M NAP` = "Yes", # logical | Yes/No
            `W M NAP ONSET` = "15:55", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "16:30", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "2", # integer | [0-7]
            `F M BEDTIME` = "01:25", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "01:50", # hms | HMS, HM, H [0-24h]
            `F M SLEEP LAT` = "30", # Duration | M
            `F M SLEEP END` = "08:05", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "65", # Duration | M
            `F M ALARM` = "No", # logical | Yes/No
            `F M REASONS` = "Yes", # logical | Yes/No
            `F M REASONS WHY` = "Hobbies", # character
            `F M NAP` = "No", # logical | Yes/No
            `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "4", # integer | [0-7]
            `W E BEDTIME` = "11:10 PM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W E SLEEP PREP` = "23:40 PM", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "30", # Duration | M
            `W E SLEEP END` = "0545", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W E TIME GU` = "30", # Duration | M
            `W E ALARM` = "No", # logical | Yes/No
            `W E REASONS` = "No", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "No", # logical | Yes/No
            `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "2", # integer | [0-7]
            `F E BEDTIME` = "23:10", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "00:15", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "35", # Duration | M
            `F E SLEEP END` = "08:45", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "5", # Duration | M
            `F E ALARM` = "No", # logical | Yes/No
            `F E REASONS` = "No", # logical | Yes/No
            `F E REASONS WHY` = "", # character
            `F E NAP` = "Yes", # logical | Yes/No
            `F E NAP ONSET` = "17:20 PM", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "18:00", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "07:10", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "07:20", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "15", # Duration | M
            `W N SLEEP END` = "1240", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
            `W N TIME GU` = "20", # Duration | M
            `W N ALARM` = "No", # logical | Yes/No
            `W N REASONS` = "No", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "Yes", # logical | Yes/No
            `W N NAP ONSET` = "23:45", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "00:00", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "8", # integer | [0-7]
            `F N BEDTIME` = "22:25", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "22:50", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "80", # Duration | M
            `F N SLEEP END` = "10:55", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "35", # Duration | M
            `F N ALARM` = "No", # logical | Yes/No
            `F N REASONS` = "No", # logical | Yes/No
            `F N REASONS WHY` = "", # character
            `F N NAP` = "Yes", # logical | Yes/No
            `F N NAP ONSET` = "17:50", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "18:25" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Possible filling error

        dplyr::add_row(
            `ID` = as.character(reserved_id[9]), # integer | [auto-increment]

            `W M N DAYS` = "", # integer | [0-7]
            `W M BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "", # Duration | M
            `W M SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "", # Duration | M
            `W M ALARM` = "", # logical | Yes/No
            `W M REASONS` = "", # logical | Yes/No
            `W M REASONS WHY` = "", # character
            `W M NAP` = "", # logical | Yes/No
            `W M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "2", # integer | [0-7]
            `F M BEDTIME` = "23:40", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "00:45", # hms | HMS, HM, H [0-24h]
            `F M SLEEP LAT` = "45", # Duration | M
            `F M SLEEP END` = "10:35", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "40", # Duration | M
            `F M ALARM` = "No", # logical | Yes/No
            `F M REASONS` = "No", # logical | Yes/No
            `F M REASONS WHY` = "", # character
            `F M NAP` = "No", # logical | Yes/No
            `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "", # integer | [0-7]
            `W E BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "", # Duration | M
            `W E SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "", # Duration | M
            `W E ALARM` = "", # logical | Yes/No
            `W E REASONS` = "", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "", # logical | Yes/No
            `W E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "", # integer | [0-7]
            `F E BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "", # Duration | M
            `F E SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "", # Duration | M
            `F E ALARM` = "", # logical | Yes/No
            `F E REASONS` = "", # logical | Yes/No
            `F E REASONS WHY` = "", # character
            `F E NAP` = "", # logical | Yes/No
            `F E NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "07:40", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "08:15", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "35", # Duration | M
            `W N SLEEP END` = "13:20", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "40", # Duration | M
            `W N ALARM` = "No", # logical | Yes/No
            `W N REASONS` = "No", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "No", # logical | Yes/No
            `W N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "", # integer | [0-7]
            `F N BEDTIME` = "", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "", # Duration | M
            `F N SLEEP END` = "", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "", # Duration | M
            `F N ALARM` = "", # logical | Yes/No
            `F N REASONS` = "", # logical | Yes/No
            `F N REASONS WHY` = "", # character
            `F N NAP` = "", # logical | Yes/No
            `F N NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "" # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Repeated workdays and work-free days values (possible carryover
    ## effect)

        dplyr::add_row(
            `ID` = as.character(reserved_id[10]), # integer | [auto-increment]

            `W M N DAYS` = "6", # integer | [0-7]
            `W M BEDTIME` = "21:10", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "21:40", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "25", # Duration | M
            `W M SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "10", # Duration | M
            `W M ALARM` = "Yes", # logical | Yes/No
            `W M REASONS` = "No", # logical | Yes/No
            `W M REASONS WHY` = "", # character
            `W M NAP` = "Yes", # logical | Yes/No
            `W M NAP ONSET` = "11:25", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "11:50", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "6", # integer | [0-7]
            `F M BEDTIME` = "21:10", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "21:40", # hms | HMS, HM, H [0-24h]
            `F M SLEEP LAT` = "25", # Duration | M
            `F M SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `F M TIME GU` = "10", # Duration | M
            `F M ALARM` = "Yes", # logical | Yes/No
            `F M REASONS` = "No", # logical | Yes/No
            `F M REASONS WHY` = "", # character
            `F M NAP` = "Yes", # logical | Yes/No
            `F M NAP ONSET` = "11:25", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "11:50", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "6", # integer | [0-7]
            `W E BEDTIME` = "21:10", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "21:40", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "25", # Duration | M
            `W E SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "10", # Duration | M
            `W E ALARM` = "Yes", # logical | Yes/No
            `W E REASONS` = "No", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "Yes", # logical | Yes/No
            `W E NAP ONSET` = "11:25", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "11:50", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "6", # integer | [0-7]
            `F E BEDTIME` = "21:10", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "21:40", # hms | HMS, HM, H [0-24h]
            `F E SLEEP LAT` = "25", # Duration | M
            `F E SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `F E TIME GU` = "10", # Duration | M
            `F E ALARM` = "Yes", # logical | Yes/No
            `F E REASONS` = "No", # logical | Yes/No
            `F E REASONS WHY` = "", # character
            `F E NAP` = "Yes", # logical | Yes/No
            `F E NAP ONSET` = "11:25", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "11:50", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "21:10", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "21:40", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "25", # Duration | M
            `W N SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "10", # Duration | M
            `W N ALARM` = "Yes", # logical | Yes/No
            `W N REASONS` = "No", # logical | Yes/No
            `W N REASONS WHY` = "", # character
            `W N NAP` = "Yes", # logical | Yes/No
            `W N NAP ONSET` = "11:25", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "11:50", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "6", # integer | [0-7]
            `F N BEDTIME` = "21:10", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "21:40", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "25", # Duration | M
            `F N SLEEP END` = "04:45", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "10", # Duration | M
            `F N ALARM` = "Yes", # logical | Yes/No
            `F N REASONS` = "No", # logical | Yes/No
            `F N REASONS WHY` = "", # character
            `F N NAP` = "Yes", # logical | Yes/No
            `F N NAP ONSET` = "11:25", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "11:50", # hms | HMS, HM, H [0-24h]
        ) %>%

    ## Sleep onset is equal or greater than sleep end
    ## [(s_prep + s_lat) >= se]

        dplyr::add_row(
            `ID` = as.character(reserved_id[11]), # integer | [auto-increment]

            `W M N DAYS` = "6", # integer | [0-7]
            `W M BEDTIME` = "22:00", # hms | HMS, HM, H [0-24h]
            `W M SLEEP PREP` = "22:50", # hms | HMS, HM, H [0-24h]
            `W M SLEEP LAT` = "15", # Duration | M
            `W M SLEEP END` = "04:15", # hms | HMS, HM, H [0-24h]
            `W M TIME GU` = "15", # Duration | M
            `W M ALARM` = "No", # logical | Yes/No
            `W M REASONS` = "No", # logical | Yes/No
            `W M REASONS WHY` = "", # character
            `W M NAP` = "Yes", # logical | Yes/No
            `W M NAP ONSET` = "13:35", # hms | HMS, HM, H [0-24h]
            `W M NAP END` = "14:00", # hms | HMS, HM, H [0-24h]

            `F M N DAYS` = "2", # integer | [0-7]
            `F M BEDTIME` = "05:30", # hms | HMS, HM, H [0-24h]
            `F M SLEEP PREP` = "07:00", # hms | HMS, HM, H [0-24h] # ERROR
            `F M SLEEP LAT` = "45", # Duration | M # ERROR
            `F M SLEEP END` = "07:45", # hms | HMS, HM, H [0-24h] # ERROR
            `F M TIME GU` = "20", # Duration | M
            `F M ALARM` = "Yes", # logical | Yes/No
            `F M REASONS` = "No", # logical | Yes/No
            `F M REASONS WHY` = "", # character
            `F M NAP` = "No", # logical | Yes/No
            `F M NAP ONSET` = "", # hms | HMS, HM, H [0-24h]
            `F M NAP END` = "", # hms | HMS, HM, H [0-24h]

            `W E N DAYS` = "4", # integer | [0-7]
            `W E BEDTIME` = "23:35", # hms | HMS, HM, H [0-24h]
            `W E SLEEP PREP` = "23:35", # hms | HMS, HM, H [0-24h]
            `W E SLEEP LAT` = "40", # Duration | M
            `W E SLEEP END` = "06:55", # hms | HMS, HM, H [0-24h]
            `W E TIME GU` = "25", # Duration | M
            `W E ALARM` = "Yes", # logical | Yes/No
            `W E REASONS` = "No", # logical | Yes/No
            `W E REASONS WHY` = "", # character
            `W E NAP` = "Yes", # logical | Yes/No
            `W E NAP ONSET` = "17:00", # hms | HMS, HM, H [0-24h]
            `W E NAP END` = "17:40", # hms | HMS, HM, H [0-24h]

            `F E N DAYS` = "2", # integer | [0-7]
            `F E BEDTIME` = "10:05", # hms | HMS, HM, H [0-24h]
            `F E SLEEP PREP` = "12:10", # hms | HMS, HM, H [0-24h] # ERROR
            `F E SLEEP LAT` = "5", # Duration | M # ERROR
            `F E SLEEP END` = "11:00", # hms | HMS, HM, H [0-24h] # ERROR
            `F E TIME GU` = "25", # Duration | M
            `F E ALARM` = "No", # logical | Yes/No
            `F E REASONS` = "No", # logical | Yes/No
            `F E REASONS WHY` = "", # character
            `F E NAP` = "Yes", # logical | Yes/No
            `F E NAP ONSET` = "17:35", # hms | HMS, HM, H [0-24h]
            `F E NAP END` = "17:55", # hms | HMS, HM, H [0-24h]

            `W N N DAYS` = "6", # integer | [0-7]
            `W N BEDTIME` = "07:20", # hms | HMS, HM, H [0-24h]
            `W N SLEEP PREP` = "07:45", # hms | HMS, HM, H [0-24h]
            `W N SLEEP LAT` = "20", # Duration | M
            `W N SLEEP END` = "12:45", # hms | HMS, HM, H [0-24h]
            `W N TIME GU` = "15", # Duration | M
            `W N ALARM` = "No", # logical | Yes/No
            `W N REASONS` = "Yes", # logical | Yes/No
            `W N REASONS WHY` = "Hobbies", # character
            `W N NAP` = "Yes", # logical | Yes/No
            `W N NAP ONSET` = "04:05", # hms | HMS, HM, H [0-24h]
            `W N NAP END` = "04:30", # hms | HMS, HM, H [0-24h]

            `F N N DAYS` = "8", # integer | [0-7]
            `F N BEDTIME` = "19:40", # hms | HMS, HM, H [0-24h]
            `F N SLEEP PREP` = "20:25", # hms | HMS, HM, H [0-24h]
            `F N SLEEP LAT` = "55", # Duration | M
            `F N SLEEP END` = "04:40", # hms | HMS, HM, H [0-24h]
            `F N TIME GU` = "20", # Duration | M
            `F N ALARM` = "No", # logical | Yes/No
            `F N REASONS` = "No", # logical | Yes/No
            `F N REASONS WHY` = "", # character
            `F N NAP` = "Yes", # logical | Yes/No
            `F N NAP ONSET` = "14:05", # hms | HMS, HM, H [0-24h]
            `F N NAP END` = "14:45" # hms | HMS, HM, H [0-24h]
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
#' Here, the process of _tiding_ a dataset is understood as transforming it in
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
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! :=
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

    values <- list(
        w_m = c("W M", "_w_m"),
        f_m = c("F M", "_f_m"),
        w_e = c("W E", "_w_e"),
        f_r = c("F E", "_f_e"),
        w_n = c("W N", "_w_n"),
        f_n = c("F N", "_f_n")
    )

    shift_mctq <- shift_mctq %>%
        dplyr::mutate(id = as.integer(.data$`ID`)) %>%
        dplyr::select(-.data$`ID`) %>%
        dplyr::relocate(.data$id, .before = .data$`W M N DAYS`)

    for (i in values) {
        shift_mctq <- shift_mctq %>% dplyr::mutate(
            !!as.symbol(paste0("n", i[2])) :=
                as.integer(.data[[paste(i[1], "N DAYS")]]),
            !!as.symbol(paste0("bt", i[2])) :=
                convert_pt(.data[[paste(i[1], "BEDTIME")]], "hms",
                                   c("HM", "IMp")),
            !!as.symbol(paste0("sprep", i[2])) :=
                dplyr::case_when(
                    grepl(pattern_4, .data[[paste(i[1], "SLEEP PREP")]],
                          perl = TRUE) ~
                        convert_pt(.data[[paste(i[1], "SLEEP PREP")]], "hms",
                                   "HM", quiet = TRUE),
                    TRUE ~
                        convert_pt(.data[[paste(i[1], "SLEEP PREP")]], "hms",
                                   c("HM", "IMp"), quiet = TRUE)),
            !!as.symbol(paste0("slat", i[2])) :=
                convert_pt(.data[[paste(i[1], "SLEEP LAT")]], "Duration", "M"),
            !!as.symbol(paste0("se", i[2])) :=
                dplyr::case_when(
                    grepl(pattern_4, .data[[paste(i[1], "SLEEP END")]],
                          perl = TRUE) ~
                        convert_pt(.data[[paste(i[1], "SLEEP END")]], "hms",
                                   "HM", quiet = TRUE),
                    TRUE ~
                        convert_pt(.data[[paste(i[1], "SLEEP END")]], "hms",
                                   c("HM", "IMp"), quiet = TRUE)),
            !!as.symbol(paste0("tgu", i[2])) :=
                convert_pt(.data[[paste(i[1], "TIME GU")]], "Duration", "M"),
            !!as.symbol(paste0("alarm", i[2])) := dplyr::case_when(
                tolower(.data[[paste(i[1], "ALARM")]]) == "yes" ~ TRUE,
                tolower(.data[[paste(i[1], "ALARM")]]) == "no" ~ FALSE),
            !!as.symbol(paste0("reasons", i[2])) := dplyr::case_when(
                tolower(.data[[paste(i[1], "REASONS")]]) == "yes" ~ TRUE,
                tolower(.data[[paste(i[1], "REASONS")]]) == "no" ~ FALSE),
            !!as.symbol(paste0("reasons_why", i[2])) :=
                .data[[paste(i[1], "REASONS WHY")]],
            !!as.symbol(paste0("nap", i[2])) := dplyr::case_when(
                tolower(.data[[paste(i[1], "NAP")]]) == "yes" ~ TRUE,
                tolower(.data[[paste(i[1], "NAP")]]) == "no" ~ FALSE),
            !!as.symbol(paste0("napo", i[2])) :=
                convert_pt(.data[[paste(i[1], "NAP ONSET")]], "hms",
                           c("HM", "IMp")),
            !!as.symbol(paste0("nape", i[2])) :=
                convert_pt(.data[[paste(i[1], "NAP END")]], "hms",
                           c("HM", "IMp")),
        ) %>%
            dplyr::select(-dplyr::starts_with(i[1]))
    }

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
#' domain knowledge, to them, removing or, if possible, fixing them. You can
#' find more about data validation and error location in Loo and Jonge (2018).
#'
#' This process can be considered as part of the process of transforming data,
#' described in the workflow proposed by Wickham and Grolemund (n.d.).
#'
#' @return An invisible `tibble` with a validated MCTQ\eqn{^{Shift}}{ Shift}
#'   dataset.
#'
#' @inheritParams tidy_shift_mctq
#' @template references_d
#' @family data functions
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

    # Set values -----

    set.seed(1)
    reserved_id <- sample(50, 11)
    shift_mctq <- tidy_shift_mctq()

    # Do univariate validation -----

    hms_0 <- hms::parse_hm("00:00")
    hms_24 <- hms::parse_hm("24:00")
    duration_0 <- lubridate::dhours(0)
    duration_6 <- lubridate::dhours(6)

    validate_hms <- function(x) {
        dplyr::case_when(
            x == hms_24 ~ hms_0,
            x >= hms_0 & x < hms_24 ~ x)
    }

    validate_duration <- function(x) {
        dplyr::case_when(
            validate::in_range(x, min = duration_0, max = duration_6) ~ x
        )
    }

    cols_1 <- stringr::str_subset(names(shift_mctq),
                                  "^bt|^sprep|^se|^napo|^nape")
    cols_2 <- stringr::str_subset(names(shift_mctq), "^slat|^tgu")

    shift_mctq <- shift_mctq %>%
        dplyr::mutate(
            dplyr::across(dplyr::all_of(cols_1), validate_hms),
            dplyr::across(dplyr::all_of(cols_2), validate_duration))

    # Do multivariate validation -----

    for (i in c("_w_m", "_f_m", "_w_e", "_f_e", "_w_n", "_f_n")) {
        bt_i <- paste0("bt", i)
        sprep_i <- paste0("sprep", i)

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

    for (i in c("_w_m", "_f_m", "_w_e", "_f_e", "_w_n", "_f_n")) {
        sprep_i <- paste0("sprep", i)
        slat_i <- paste0("slat", i)
        se_i <- paste0("se", i)

        test <- shift_mctq %>%
            dplyr::mutate(
                so_i = mctq::so(!!as.symbol(sprep_i), !!as.symbol(slat_i)),
                sd_i = mctq::sd(so_i, !!as.symbol(se_i)),
                dummy = dplyr::case_when(
                    sd_i < lubridate::dhours(2) |
                        sd_i > lubridate::dhours(18) ~ TRUE,
                    TRUE ~ FALSE)) %>%
            dplyr::select(dummy)

        shift_mctq <- dplyr::bind_cols(shift_mctq, test) %>%
            dplyr::mutate(
                dplyr::across(dplyr::ends_with(i),
                              ~ dplyr::if_else(dummy, na_as(.x), .x))) %>%
            dplyr::select(-dummy)
    }

    # Clean invalid cases -----

    ## Cases: "Suspicious values" and "Repeated workdays and work-free days
    ## values (possible carryover effect)"

    invalid <- c(reserved_id[7], reserved_id[10])

    shift_mctq <- shift_mctq %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            dplyr::across(-.data$id, .fns = ~ dplyr::if_else(
                .data$id %in% invalid, na_as(.x), .x))) %>%
        dplyr::ungroup()

    # Fix/impute linked data -----

    for (i in c("_w_m", "_f_m", "_w_e", "_f_e", "_w_n", "_f_n")) {
        shift_mctq <- shift_mctq %>%
            dplyr::mutate(
                !!as.symbol(paste0("reasons_why", i)) := dplyr::case_when(
                    !!as.symbol(paste0("reasons_why", i)) == "no" ~
                        as.character(NA),
                    TRUE ~ !!as.symbol(paste0("reasons_why", i))),
                !!as.symbol(paste0("reasons", i)) := dplyr::case_when(
                    !is.na(!!as.symbol(paste0("reasons_why", i))) ~ TRUE,
                    TRUE ~ !!as.symbol(paste0("reasons", i)))
            )
    }

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
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! :=
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

    # Set values -----

    shift_mctq <- validate_shift_mctq()

    # Create computed variables -----

    for (i in c("_w_m", "_f_m", "_w_e", "_f_e", "_w_n", "_f_n")) {
        ms <- paste0("ms", stringr::str_extract(i, "._.$"))

        shift_mctq <- shift_mctq %>%
            dplyr::mutate(
                !!as.symbol(paste0("so", i)) :=
                    so(!!as.symbol(paste0("sprep", i)),
                       !!as.symbol(paste0("slat", i))),
                !!as.symbol(paste0("gu", i)) :=
                    gu(!!as.symbol(paste0("se", i)),
                       !!as.symbol(paste0("tgu", i))),
                !!as.symbol(paste0("sd", i)) :=
                    sd(!!as.symbol(paste0("so", i)),
                       !!as.symbol(paste0("se", i))),
                !!as.symbol(paste0("tbt", i)) :=
                    tbt(!!as.symbol(paste0("bt", i)),
                        !!as.symbol(paste0("gu", i))),
                !!as.symbol(ms) :=
                    ms(!!as.symbol(paste0("so", i)),
                       !!as.symbol(paste0("sd", i))),
                !!as.symbol(paste0("napd", i)) :=
                    napd(!!as.symbol(paste0("napo", i)),
                         !!as.symbol(paste0("nape", i))),
                !!as.symbol(paste0("sd24", i)) :=
                    sd24(!!as.symbol(paste0("sd", i)),
                         !!as.symbol(paste0("napd", i)),
                         !!as.symbol(paste0("nap", i)))
                )
    }

    for (i in c("_m", "_e", "_n")) {
        shift_mctq <- shift_mctq %>%
            dplyr::mutate(
                !!as.symbol(paste0("sd_overall", i)) :=
                    sd_overall(!!as.symbol(paste0("sd_w", i)),
                               !!as.symbol(paste0("sd_f", i)),
                               !!as.symbol(paste0("n_w", i)),
                               !!as.symbol(paste0("n_f", i))),
                !!as.symbol(paste0("msf_sc", i)) :=
                    msf_sc(!!as.symbol(paste0("msf", i)),
                           !!as.symbol(paste0("sd_w", i)),
                           !!as.symbol(paste0("sd_f", i)),
                           !!as.symbol(paste0("sd_overall", i)),
                           !!as.symbol(paste0("alarm_f", i))),
                !!as.symbol(paste0("sjl_rel", i)) :=
                    sjl_rel(!!as.symbol(paste0("msw", i)),
                            !!as.symbol(paste0("msf", i))),
                !!as.symbol(paste0("sjl", i)) :=
                    abs(!!as.symbol(paste0("sjl_rel", i)))
            )
    }

    shift_mctq <- shift_mctq %>%
        dplyr::mutate(
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

    # Apply corrections to `sjl_rel` and `sjl` -----

    ## Helper:
    ## shift_mctq_hms <- pretty_mctq(shift_mctq)
    ## test <- dplyr::select(shift_mctq_hms, msf_m, msw_m, sjl_rel_m)

    cases <- c(39)

    shift_mctq <- shift_mctq %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            sjl_rel_n = dplyr::if_else(.data$id %in% cases,
                                       sjl_rel(.data$msw_n, .data$msf_n,
                                               method = "longer"),
                                       .data$sjl_rel_n),
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            sjl_n = abs(.data$sjl_rel_n)
        )

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
