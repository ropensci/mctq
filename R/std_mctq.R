#' A standard MCTQ dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' A fictional dataset composed by Munich Chronotype Questionnaire (MCTQ)
#' (standard version) basic/measurable and computed variables for testing and
#' learning purposes.
#'
#' This data was created according to Roenneberg, Wirz-Justice & Merrow
#' ([2003](https://bit.ly/3rLu195)) and guidelines of The World Wide
#' Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)). See References
#' and Details sections to learn more.
#'
#' The naming of the variables took into account the standard names used by
#' the theWeP and guidelines of the
#' [tidyverse style guide](https://style.tidyverse.org/).
#'
#' @details
#'
#' `std_mctq` is a tidied, validated, and transformed version of
#' `mctq:::build_std_mctq()`. This dataset was created to demonstrate common
#' cases and data issues that researchers may find in their MCTQ data.
#'
#' You can learn more about the `std_mctq` data cleaning process in
#' `vignette("data-wrangling", package = "mctq")`.
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
#' @format A tibble with `r ncol(std_mctq)` columns and `r nrow(std_mctq)` rows:
#'
#' \describe{
#'   \item{id}{
#'   A unique numeric value for the purpose of identifying each subject in the
#'   dataset.
#'   \cr \cr
#'   Type: Control
#'   \cr \cr
#'   R class: `integer`.}
#'
#'
#'   \item{work}{
#'   A logical value indicating if the subject have a regular work schedule.
#'   \cr \cr
#'   Statement (EN): "I have a regular work schedule (this includes being, for
#'   example, a housewife or househusband)").
#'   \cr \cr
#'   Type: Basic
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{wd}{
#'   Number of workdays per week.
#'   \cr \cr
#'   Statement (EN): "I have a regular work schedule and work ___ days per
#'   week").
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'   \item{fd}{
#'   Number of work-free days per week.
#'   \cr \cr
#'   Type: Computed (7 - wd).
#'   \cr \cr
#'   R class: `integer`.}
#'
#'
#'   \item{bt_w}{
#'   Local time of going to bed on workdays.
#'   \cr \cr
#'   Statement (EN): "I go to bed at ___ o'clock'".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sprep_w}{
#'   Local time of preparing to sleep on workdays.
#'   \cr \cr
#'   Statement (EN): "I actually get ready to fall asleep at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{slat_w}{
#'   Sleep latency on workdays.
#'   \cr \cr
#'   Statement (EN): "I need ___ minutes to fall asleep".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{so_w}{
#'   Sleep onset on workdays.
#'   \cr \cr
#'   Type: Computed (sprep_w + slat_w).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_w}{
#'   Sleep end on workdays.
#'   \cr \cr
#'   Statement (EN): "I wake up at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{si_w}{
#'   Sleep inertia on workdays.
#'   \cr \cr
#'   Statement (EN): "After ___ minutes, I get up".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{gu_w}{
#'   Local time of getting out of bed on workdays.
#'   \cr \cr
#'   Type: Computed (se_w + si_w).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{alarm_w}{
#'   A logical value indicating if the subject use an alarm clock on workdays.
#'   \cr \cr
#'   Statement (EN): "I use an alarm clock on workdays".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{wake_before_w}{
#'   A logical value indicating if the subject regularly wake up BEFORE the
#'   alarm rings.
#'   \cr \cr
#'   Statement (EN): "I regularly wake up BEFORE the alarm rings".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{le_w}{
#'   Light exposure on workdays.
#'   \cr \cr
#'   Statement (EN): "On average, I spend the following amount of time outdoors
#'   in daylight (without a roof above my head)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sd_w}{
#'   Sleep duration on workdays.
#'   \cr \cr
#'   Type: Computed (se_w - so_w).
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{tbt_w}{
#'   Total time in bed on workdays.
#'   \cr \cr
#'   Type: Computed (gu_w - bt_w).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{msw}{
#'   Mid-sleep on workdays.
#'   \cr \cr
#'   Type: Computed (so_w + (sd_w / 2)).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'
#'   \item{bt_f}{
#'   Local time of going to bed on work-free days.
#'   \cr \cr
#'   Statement (EN): "I go to bed at ___ o'clock'".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sprep_f}{
#'   Local time of preparing to sleep on work-free days
#'   \cr \cr
#'   Statement (EN): "I actually get ready to fall asleep at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{slat_f}{
#'   Sleep latency on work-free days.
#'   \cr \cr
#'   Statement (EN): "I need ___ minutes to fall asleep".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{so_f}{
#'   Sleep onset on work-free days.
#'   \cr \cr
#'   Type: Computed (sprep_f + slat_f).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_f}{
#'   Sleep end on work-free days.
#'   \cr \cr
#'   Statement (EN): "I wake up at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{si_f}{
#'   Sleep inertia on work-free days.
#'   \cr \cr
#'   Statement __(EN)__: "After ___ minutes, I get up".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{gu_f}{
#'   Local time of getting out of bed on work-free days.
#'   \cr \cr
#'   Type: Computed (se_f + si_f).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{alarm_f}{
#'   A logical value indicating if the subject use an alarm clock on work-free
#'   days.
#'   \cr \cr
#'   Statement (EN): "My wake-up time is due to the use of an alarm
#'   clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_f}{
#'   A logical value indicating if the subject have any particular reasons for
#'   why he cannot freely choose his sleep times on work-free days.
#'   \cr \cr
#'   Statement (EN): "There are particular reasons why I cannot freely choose my
#'   sleep times on free days".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_why_f}{
#'   Particular reasons why the subject cannot freely choose his sleep times on
#'   work-free days.
#'   \cr \cr
#'   Statement (EN): "There are particular reasons why I cannot freely choose my
#'   sleep times on free days".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `character`.}
#'
#'   \item{le_f}{
#'   Light exposure on work-free days.
#'   \cr \cr
#'   Statement (EN): "On average, I spend the following amount of time outdoors
#'   in daylight (without a roof above my head)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sd_f}{
#'   Sleep duration on work-free days.
#'   \cr \cr
#'   Type: Computed (se_f - so_f).
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{tbt_f}{
#'   Total time in bed on work-free days.
#'   \cr \cr
#'   Type: Computed (gu_f - bt_f).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{msf}{
#'   Mid-sleep on work-free days.
#'   \cr \cr
#'   Type: Computed (so_f + (sd_f / 2)).
#'   \cr \cr
#'   R class: `hms`.}
#'
#'
#'   \item{sd_week}{
#'   Average weekly sleep duration.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf_sc}{
#'   chronotype/corrected midsleep on work-free days.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sloss_week}{
#'   Weekly sleep loss.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sjl_rel}{
#'   Relative social jetlag.
#'   \cr \cr
#'   Type: Computed (msf - msw).
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sjl}{
#'   Absolute social jetlag.
#'   \cr \cr
#'   Type: Computed (abs(msf - msw)).
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{le_week}{
#'   Average weekly light exposure.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#' }
#'
#' @usage data(std_mctq)
#' @source Prepared by Daniel Vartanian (package's author).
#' @family datasets
#' @template references_b
"std_mctq"


# HELPERS =====

#' Build a fictional standard MCTQ raw dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `build_std_mctq()` builds a fictional raw dataset composed by Munich
#' Chronotype Questionnaire (MCTQ) (standard version) basic/measurable variables
#' for testing and learning purposes. See [mctq::std_mctq] to learn more.
#'
#' @param write (optional) a logical value indicating if `build_std_mctq()` must
#'   write a `std_mctq.csv` file to `"./inst/extdata/"` (default: `FALSE`).
#' @param random_cases (optional) a logical value indicating if
#'   `build_std_mctq()` must add random MCTQ cases besides the core ones.
#'
#' @return An invisible tibble with a raw standard MCTQ dataset.
#'
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' utils::View(mctq::build_std_mctq())
#' }
build_std_mctq <- function(write = FALSE, random_cases = TRUE) {

    # Check arguments -----

    checkmate::assert_flag(write)
    checkmate::assert_flag(random_cases)

    # Set IDs -----

    # reserved_id <- sample(1:50, 14)
    set.seed(1)
    reserved_id <- sample(50, 14)
    id <- seq(50)[!(seq(50) %in% reserved_id)]

    # Create cases -----

    ## Base subject: Sleeps less than recommended for an adult on workdays and
    ##               stretches during work-free days

    std_mctq <- dplyr::tibble(
        `ID` = as.character(id[1]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "00:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "01:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "15", # duration | M
        `W SLEEP END` = "06:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "02:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "01:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "02:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "15", # duration | M
        `F SLEEP END` = "12:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "30", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "No", # logical |  Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "04:00" # duration | [H]MS, [H]M, [H]
    )

    ## Random cases

    format_logical <- function(x) {
        checkmate::assert_logical(x)

        dplyr::case_when(
            x == TRUE ~ "Yes",
            x == FALSE ~ "No"
        )
    }

    format_hms <- function(x) {
        assert_time(x)

        # format <- sample(list(c(1, 5), c(1, 8)), 1, prob = c(10, 1))
        # format <- unlist(format)
        format <- c(1, 5)

        out <- convert_to(x, "hms")
        # out <- hms::trunc_hms(x, 60)
        out <- substr(as.character(out), format[1], format[2])

        # if (sample(c(TRUE, FALSE), 1, prob = c(1, 5)) && nchar(out) != 8){
        #     stringr::str_remove_all(out, ":")
        # }

        out
    }

    format_duration <- function(x) {
        assert_time(x)
        out <- convert_to(x, "duration")
        as.character(convert_to_tu(out, "M"))
    }

    if (isTRUE(random_cases)) {
        for (i in id[-1]) {
            random_case <- dplyr::as_tibble(random_mctq()) %>% dplyr::transmute(
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

    ## Inverted values for bed time and sleep preparing on workdays.

    std_mctq <- std_mctq %>% dplyr::add_row(
        `ID` = as.character(reserved_id[1]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "4", # integer | [0-7]
        `W BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "10", # duration | M
        `W SLEEP END` = "05:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "04:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "01:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "15", # duration | M
        `F SLEEP END` = "11:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Child(ren)/pet(s)", # character
        `F LIGHT EXPOSURE` = "06:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Inverted values for bed time and sleep preparing on work-free days

    dplyr::add_row(
        `ID` = as.character(reserved_id[2]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "22:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "30", # duration | M
        `W SLEEP END` = "08:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "10", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "Yes", # logical | Yes/No
        `W LIGHT EXPOSURE` = "01:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "23:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "22:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "45", # duration | M
        `F SLEEP END` = "08:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "No", # logical | Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "00:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Presence of invalid values

    dplyr::add_row(
        `ID` = as.character(reserved_id[3]), # integer | [auto-increment]
        `WORK REGULAR` = "No", # logical | Yes/No
        `WORK DAYS` = "10", # integer | [0-7] # INVALID
        `W BED TIME` = "27:00", # hms | HMS, HM, H [0-24h] # INVALID
        `W SLEEP PREP` = "02:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "30", # duration | M
        `W SLEEP END` = "12:15", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "20", # duration | M
        `W ALARM` = "No", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "Yes", # logical | Yes/No # INVALID
        `W LIGHT EXPOSURE` = "02:15", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "34:00", # hms | HMS, HM, H [0-24h] # INVALID
        `F SLEEP PREP` = "04:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "No", # duration | M
        `F SLEEP END` = "14:12", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "30", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "-01:30" # duration | [H]MS, [H]M, [H] # INVALID
    ) %>%

    ## Sleeps more on workdays than work-free days

    dplyr::add_row(
        `ID` = as.character(reserved_id[4]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "2", # integer | [0-7]
        `W BED TIME` = "21:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "2130", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "15", # duration | M
        `W SLEEP END` = "09:15", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "10", # duration | M
        `W ALARM` = "No", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "0500", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "5", # duration | M
        `F SLEEP END` = "0600", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "07:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Uses alarm clock on work-free days

    dplyr::add_row(
        `ID` = as.character(reserved_id[5]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "15", # duration | M
        `W SLEEP END` = "07:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "01:55", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "01:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "01:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "30", # duration | M
        `F SLEEP END` = "08:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "0", # duration | M
        `F ALARM` = "Yes", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Child(ren)/pet(s)", # character
        `F LIGHT EXPOSURE` = "04:45" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Null MCTQ (invalid case)

    dplyr::add_row(
        `ID` = as.character(reserved_id[6]), # integer | [auto-increment]
        `WORK REGULAR` = "", # logical | Yes/No
        `WORK DAYS` = "", # integer | [0-7]
        `W BED TIME` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "", # duration | M
        `W SLEEP END` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "", # duration | M
        `W ALARM` = "", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "", # duration | M
        `F SLEEP END` = "", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "", # duration | M
        `F ALARM` = "", # logical | Yes/No
        `F REASONS` = "", # logical | Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Did not answer workdays questions

    dplyr::add_row(
        `ID` = as.character(reserved_id[7]), # integer | [auto-increment]
        `WORK REGULAR` = "No", # logical | Yes/No
        `WORK DAYS` = "", # integer | [0-7]
        `W BED TIME` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "", # duration | M
        `W SLEEP END` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "", # duration | M
        `W ALARM` = "", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "03:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "04:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "90", # duration | M
        `F SLEEP END` = "15:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "30", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "No", # logical | Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "00:30" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## All basic variables have the same values (invalid case)

    dplyr::add_row(
        `ID` = as.character(reserved_id[8]), # integer | [auto-increment]
        `WORK REGULAR` = "0", # logical | Yes/No
        `WORK DAYS` = "0", # integer | [0-7]
        `W BED TIME` = "0", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "0", # duration | M
        `W SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "0", # duration | M
        `W ALARM` = "0", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "0", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "0", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "0", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "0", # duration | M
        `F SLEEP END` = "0", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "0", # duration | M
        `F ALARM` = "0", # logical | Yes/No
        `F REASONS` = "0", # logical | Yes/No
        `F REASONS WHY` = "0", # character
        `F LIGHT EXPOSURE` = "0" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Works 7 days a week and didn't answer work-free days section

    dplyr::add_row(
        `ID` = as.character(reserved_id[9]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "7", # integer | [0-7]
        `W BED TIME` = "23:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "23:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "10", # duration | M
        `W SLEEP END` = "06:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "02:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "", # duration | M
        `F SLEEP END` = "", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "", # duration | M
        `F ALARM` = "", # logical | Yes/No
        `F REASONS` = "", # logical | Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Suspicious values (removed case)

    dplyr::add_row(
        `ID` = as.character(reserved_id[10]), # integer | [auto-increment]
        `WORK REGULAR` = "No", # logical | Yes/No
        `WORK DAYS` = "6", # integer | [0-7]
        `W BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "120", # duration | M
        `W SLEEP END` = "04:00", # hms | HMS, HM, H [0-24h] # SUSPICIOUS
        `W SLEEP INERTIA` = "0", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "Yes", # logical | Yes/No
        `W LIGHT EXPOSURE` = "18:00", # duration | [H]MS, [H]M, [H] # SUSPICIOUS
        `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "00:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "30", # duration | M
        `F SLEEP END` = "09:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "No", # logical | Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "17:00" # duration | [H]MS, [H]M, [H] # SUSPICIOUS
    ) %>%

    ## Different formats

    dplyr::add_row(
        `ID` = as.character(reserved_id[11]), # integer | [auto-increment]
        `WORK REGULAR` = "true", # logical | Yes/No # AMBIGUOUS
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "11:00 PM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
        `W SLEEP PREP` = "0000", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
        `W SLEEP LAT` = "00:15", # duration | M #AMBIGUOUS
        `W SLEEP END` = "07:15 AM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
        `W SLEEP INERTIA` = "30", # duration | M
        `W ALARM` = "No", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "3", # duration | [H]MS, [H]M, [H] # AMBIGUOUS
        `F BED TIME` = "01:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "0130 AM", # hms | HMS, HM, H [0-24h] # AMBIGUOUS
        `F SLEEP LAT` = "60", # duration | M
        `F SLEEP END` = "10:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "00:15", # duration | M # AMBIGUOUS
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "04:30" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Possible filling error

    dplyr::add_row(
        `ID` = as.character(reserved_id[12]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "6", # integer | [0-7]
        `W BED TIME` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "", # duration | M
        `W SLEEP END` = "", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "", # duration | M
        `W ALARM` = "", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "22:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "22:30", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "15", # duration | M
        `F SLEEP END` = "06:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "3", # duration | M
        `F ALARM` = "Yes", # logical | Yes/No
        `F REASONS` = "Yes", # logical | Yes/No
        `F REASONS WHY` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "01:30" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Repeated workdays and work-free days values (possible carryover effect).

    dplyr::add_row(
        `ID` = as.character(reserved_id[13]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "5", # integer | [0-7]
        `W BED TIME` = "22:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "10", # duration | M
        `W SLEEP END` = "07:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "5", # duration | M
        `W ALARM` = "Yes", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "No", # logical | Yes/No
        `W LIGHT EXPOSURE` = "01:00", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "22:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "23:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "5", # duration | M
        `F SLEEP END` = "07:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "5", # duration | M
        `F ALARM` = "Yes", # logical | Yes/No
        `F REASONS` = "No", # logical | Yes/No
        `F REASONS WHY` = "No", # character
        `F LIGHT EXPOSURE` = "01:00" # duration | [H]MS, [H]M, [H]
    ) %>%

    ## Sleep onset is equal or greater than sleep end [(s_prep + s_lat) >= se]
    ## (invalid case)

    dplyr::add_row(
        `ID` = as.character(reserved_id[14]), # integer | [auto-increment]
        `WORK REGULAR` = "Yes", # logical | Yes/No
        `WORK DAYS` = "2", # integer | [0-7]
        `W BED TIME` = "22:30", # hms | HMS, HM, H [0-24h]
        `W SLEEP PREP` = "00:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP LAT` = "60", # duration | M
        `W SLEEP END` = "08:00", # hms | HMS, HM, H [0-24h]
        `W SLEEP INERTIA` = "10", # duration | M
        `W ALARM` = "No", # logical | Yes/No
        `W WAKE BEFORE ALARM` = "", # logical | Yes/No
        `W LIGHT EXPOSURE` = "01:20", # duration | [H]MS, [H]M, [H]
        `F BED TIME` = "00:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP PREP` = "02:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP LAT` = "120", # duration | M
        `F SLEEP END` = "04:00", # hms | HMS, HM, H [0-24h]
        `F SLEEP INERTIA` = "15", # duration | M
        `F ALARM` = "No", # logical | Yes/No
        `F REASONS` = "No", # logical | Yes/No
        `F REASONS WHY` = "", # character
        `F LIGHT EXPOSURE` = "04:00" # duration | [H]MS, [H]M, [H]
    )

    std_mctq <- std_mctq %>% dplyr::arrange(as.integer(.data$`ID`))

    # Write and return output -----

    if (isTRUE(write)) {
        if(!(dir.exists("./inst/extdata/"))) {
            dir.create("./inst/extdata/")
        }

        std_mctq %>%
            utils::write.csv(paste0("./inst/extdata/", "std_mctq", ".csv"),
                             row.names = FALSE,
                             quote = FALSE)

        # std_mctq %>%
        #     readr::write_delim(paste0("./inst/extdata/", "std_mctq", ".csv"),
        #                 delim = ",",
        #                 col_names = TRUE)
    }

    invisible(std_mctq)

}

#' Tidy [mctq::build_std_mctq()] output
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tidy_std_mctq` tidy the output of [mctq::build_std_mctq()]. See
#' [mctq::std_mctq] to learn more.
#'
#' @details
#'
#' Here the process of _tiding_ a dataset is understood as transforming it in
#' input data, like described in Loo and Jonge ([2018](https://bit.ly/3pVuUdt)).
#' Is a very similar process of tiding data described in the workflow proposed
#' by Wickham and Grolemund ([n.d.](https://r4ds.had.co.nz)).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' `std_mctq` data, run [mctq::validate_std_mctq()].
#'
#' To learn more about the concept of tidy data, _c.f._ Wickham
#' ([2014](https://bit.ly/3hBTE7g)) and Wickham and Grolemund
#' ([n.d.](https://r4ds.had.co.nz)).
#'
#' @param write (optional) a logical value indicating if the function must write
#'   a `std_mctq.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @return An invisible tibble with a tidied, but not validated, standard MCTQ
#'   dataset.
#'
#' @family data wrangling functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
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
#' \dontrun{
#' utils::View(tidy_std_mctq())
#' }
tidy_std_mctq <- function(write = FALSE) {

    # Check arguments -----

    checkmate::assert_flag(write)

    # Clean NULL cases -----

    std_mctq <- build_std_mctq() %>%
        dplyr::mutate(dplyr::across(.fns = ~ dplyr::na_if(.x, ""))) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(length =
                          dplyr::n_distinct(dplyr::c_across(-.data$ID))) %>%
        dplyr::mutate(dplyr::across(-.data$ID, .fns = ~ ifelse(length <= 2,
                                                         NA, .x))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-length)

    # Convert variables -----

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
        work = dplyr::case_when(
            tolower(.data$`WORK REGULAR`) == "yes" ~ TRUE,
            tolower(.data$`WORK REGULAR`) == "true" ~ TRUE,
            tolower(.data$`WORK REGULAR`) == "no" ~ FALSE),
        wd = as.integer(.data$`WORK DAYS`),
        bt_w = dplyr::case_when(
            stringr::str_detect(.data$`W BED TIME`, pattern_1) ~
                convert_to_pt(.data$`W BED TIME`, "hms",
                              c("HM", "IMp"), quiet = TRUE),
            stringr::str_detect(.data$`W BED TIME`, pattern_4) ~
                convert_to_pt(.data$`W BED TIME`, "hms", "HM",
                              quiet = TRUE)),
        sprep_w = convert_to_pt(.data$`W SLEEP PREP`, "hms",
                                c("HMS", "HM", "H")),
        slat_w = dplyr::case_when(
            stringr::str_detect(.data$`W SLEEP LAT`, pattern_2) ~
                convert_to_pt(.data$`W SLEEP LAT`, "Duration", "M",
                              quiet = TRUE),
            stringr::str_detect(.data$`W SLEEP LAT`, pattern_1) ~
                convert_to_pt(.data$`W SLEEP LAT`, "Duration",
                              c("HMS", "HM", "H"), quiet = TRUE)),
        se_w = convert_to_pt(.data$`W SLEEP END`, "hms", c("HMS", "HM", "H")),
        si_w = convert_to_pt(.data$`W SLEEP INERTIA`, "Duration", "M"),
        alarm_w = dplyr::case_when(
            tolower(.data$`W ALARM`) == "yes" ~ TRUE,
            tolower(.data$`W ALARM`) == "no" ~ FALSE),
        wake_before_w = dplyr::case_when(
            tolower(.data$`W WAKE BEFORE ALARM`) == "yes" ~ TRUE,
            tolower(.data$`W WAKE BEFORE ALARM`) == "no" ~ FALSE),
        le_w = convert_to_pt(.data$`W LIGHT EXPOSURE`, "Duration",
                             c("HMS", "HM", "H")),
        bt_f = dplyr::case_when(
            stringr::str_detect(.data$`F BED TIME`, pattern_1) ~
                convert_to_pt(.data$`F BED TIME`, "hms", c("HMS", "HM", "H"),
                              quiet = TRUE),
            stringr::str_detect(.data$`F BED TIME`, pattern_4) ~
                convert_to_pt(.data$`F BED TIME`, "hms", "HM",
                              quiet = TRUE)),
        sprep_f = convert_to_pt(.data$`F SLEEP PREP`, "hms",
                                c("HMS", "HM", "H")),
        slat_f = convert_to_pt(.data$`F SLEEP LAT`, "Duration", "M",
                               quiet = TRUE),
        se_f = convert_to_pt(.data$`F SLEEP END`, "hms", c("HMS", "HM", "H")),
        si_f = dplyr::case_when(
            stringr::str_detect(.data$`F SLEEP INERTIA`, pattern_2) ~
                convert_to_pt(.data$`F SLEEP INERTIA`, "Duration", "M",
                              quiet = TRUE),
            stringr::str_detect(.data$`F SLEEP INERTIA`, pattern_1) ~
                convert_to_pt(.data$`F SLEEP INERTIA`, "Duration",
                              c("HMS", "HM", "H"), quiet = TRUE)),
        alarm_f = dplyr::case_when(
            tolower(.data$`F ALARM`) == "yes" ~ TRUE,
            tolower(.data$`F ALARM`) == "no" ~ FALSE),
        reasons_f = dplyr::case_when(
            tolower(.data$`F REASONS`) == "yes" ~ TRUE,
            tolower(.data$`F REASONS`) == "no" ~ FALSE),
        reasons_why_f = .data$`F REASONS WHY`,
        le_f = convert_to_pt(.data$`F LIGHT EXPOSURE`, "Duration", "HM")
    )

    # Write and output dataset -----

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
#' `r lifecycle::badge("experimental")`
#'
#' `validate_std_mctq()` validates the output of [mctq::tidy_std_mctq()].
#' See [mctq::std_mctq] to learn more.
#'
#' @details
#'
#' Here the process of _validating_ a dataset is understood as detecting invalid
#' data, by checking whether data satisfies certain assumptions from domain
#' knowledge, to them removing or, if possible, fixing them. You can find
#' more about data validation and error location in Loo and Jonge
#' ([2018](https://bit.ly/3pVuUdt)).
#'
#' This process can be considered as a part of the process of transforming data,
#' described in the workflow proposed by Wickham and Grolemund
#' ([n.d.](https://r4ds.had.co.nz)).
#'
#' `mctq` provides a extensive list of validation parameters to ensure that
#' common MCTQ data issues do not go unnoticed. See [mctq::validate_mctq()] to
#' learn more.
#'
#' @return An invisible tibble with a validated standard MCTQ dataset.
#'
#' @family data wrangling functions
#' @inheritParams tidy_std_mctq
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. doi:
#' [10.1002/9781118897126](http://dx.doi.org/10.1002/9781118897126).
#'
#' Wickham, H, & Grolemund. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. Retrieved from <https://r4ds.had.co.nz>.
#'
#' @examples
#' \dontrun{
#' utils::View(validate_std_mctq())
#' }
validate_std_mctq <- function(write = FALSE) {

    # To do -----
    #
    # * Adapt this process by using `errorlocate` package with `validate`.

    # Check arguments -----

    checkmate::assert_flag(write)

    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    dummy <- bkp <- NULL

    # Set values -----

    std_mctq <- tidy_std_mctq()

    # Do univariate validation -----

    std_mctq <- std_mctq %>% dplyr::mutate(
        wd = dplyr::case_when(validate::in_range(wd, min = 0, max = 7) ~ wd)
    )

    hms_0 <- hms::parse_hm("00:00")
    hms_24 <- hms::parse_hm("24:00")

    foo <- function(x) {
        dplyr::case_when(
            x == hms_24 ~ hms_0,
            x >= hms_0 & x < hms_24 ~ x
            )
    }

    cols <- c("bt_w", "sprep_w", "se_w", "bt_f", "sprep_f", "se_f")
    std_mctq <- std_mctq %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(cols), foo))

    duration_0 <- lubridate::dhours(0)
    duration_6 <- lubridate::dhours(6)

    foo <- function(x) {
        dplyr::case_when(
            validate::in_range(x, min = duration_0, max = duration_6) ~ x
        )
    }

    cols <- c("slat_w", "si_w", "slat_f", "si_f")
    std_mctq <- std_mctq %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(cols), foo))

    duration_24 <- lubridate::dhours(24)

    foo <- function(x) {
        dplyr::case_when(
            validate::in_range(x, min = duration_0, max = duration_24) ~ x
        )
    }

    cols <- c("le_w", "le_f")
    std_mctq <- std_mctq %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(cols), foo))

    # Do multivariate validation -----

    for (i in c("w", "f")) {
        bt_i <- paste0("bt_", i)
        sprep_i <- paste0("sprep_", i)

        std_mctq <- std_mctq %>%
            dplyr::mutate(
                dummy = dplyr::case_when(
                    assign_date(!!as.symbol(bt_i), !!as.symbol(sprep_i)) >
                        lubridate::dhours(12) ~ TRUE,
                    TRUE ~ FALSE
                    ),
                bkp = !!as.symbol(bt_i),
                !!as.symbol(bt_i) :=
                    dplyr::if_else(dummy, !!as.symbol(sprep_i),
                                   !!as.symbol(bt_i)),
                !!as.symbol(sprep_i) :=
                    dplyr::if_else(dummy, bkp, !!as.symbol(sprep_i))) %>%
            dplyr::select(-dummy, -bkp)
    }

    # Clean invalid cases -----

    std_mctq <- std_mctq %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dplyr::across(-.data$id, .fns = ~ if_else(
                .data$id %in% c(9, 21), na_as(.x), .x))) %>%
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

#' Analyze [mctq::validate_std_mctq()] output
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `analyse_std_mctq()` computes and creates the non-measured MCTQ variables
#'  for the output of [mctq::validate_std_mctq()]. See [mctq::std_mctq] to
#'  learn more.
#'
#' @details
#'
#' Computing and creating new variables is part of the process of producing
#' statistics, like described in Loo and Jonge ([2018](https://bit.ly/3pVuUdt)).
#' It is also a part of the process of transforming data, described in the
#' workflow proposed by Wickham and Grolemund ([n.d.](https://r4ds.had.co.nz)).
#'
#' @return An invisible tibble with all the variables proposed for a standard
#'   MCTQ dataset.
#'
#' @family data wrangling functions
#' @inheritParams tidy_std_mctq
#' @inheritParams pretty_mctq
#' @inherit validate_std_mctq references
#' @noRd
#'
#' @examples
#' \dontrun{
#' utils::View(analyze_std_mctq())
#' }
analyze_std_mctq <- function(write = FALSE, round = TRUE, hms = TRUE) {

    # Check arguments -----

    checkmate::assert_flag(write)

    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    id <- NULL
    work <- wd <- bt_w <- bt_f <- sprep_w <- sprep_f <- slat_w <- slat_f <- NULL
    so_w <- so_f <- se_w <- se_f <- si_w <- si_f <- gu_w <- gu_f <- NULL
    alarm_w <- alarm_f <- wake_before_w <- reasons_f <- reasons_why_f<- NULL
    le_w <- le_f <- sd_w <- sd_f <- tbt_w <- tbt_f <- msw <- msf <- NULL
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
            sd_week = sd_week(wd, sd_w, sd_f),
            msf_sc = msf_sc(msf, sd_w, sd_f, sd_week, alarm_f),
            sloss_week = sloss_week(wd, sd_w, sd_f, sd_week),
            sjl_rel = sjl_rel(msw, msf),
            sjl = abs(sjl_rel),
            le_week = le_week(wd, le_w, le_f)) %>%
        dplyr::relocate(
            id, work, wd, fd, bt_w, sprep_w, slat_w, so_w, se_w, si_w, gu_w,
            alarm_w, wake_before_w, le_w, sd_w, tbt_w, msw, bt_f, sprep_f,
            slat_f, so_f, se_f, si_f, gu_f, alarm_f, reasons_f, reasons_why_f,
            le_f, sd_f, tbt_f, msf, sd_week, msf_sc, sloss_week, sjl_rel, sjl,
            le_week)

    # Fix missing sections -----

    ## See `vignette("missing_sections", "mctq")` to learn more.

    count_w <- length(subset(names(std_mctq),  grepl("_w$", names(std_mctq))))
    count_f <- length(subset(names(std_mctq),  grepl("_f$", names(std_mctq))))
    count_w <- count_w * 2/3
    count_f <- count_f * 2/3

    test <- std_mctq %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            dummy_0_a = as.integer(wd) == 0 & !is.na(wd),
            dummy_0_b = count_na(
                dplyr::c_across(dplyr::ends_with("_w"))) >= count_w,
            dummy_0_c = alarm_f == as.logical(FALSE) | is.na(alarm_f),
            dummy_7_a = as.integer(wd) == 7 & !is.na(wd),
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
