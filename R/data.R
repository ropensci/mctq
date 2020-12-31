#' @noRd
build_mctq_std <- function() {

    # Create raw dataset --------------------

    # # Helper
    # x <- mctq::mctq_std
    # names(x)
    # as.data.frame(x[1, ])
    # as.data.frame(x[, 1])

    mctq_std <- dplyr::tibble(
        `ID` = "1",
        `WORK REGULAR` = "Yes", # character (Yes/No)
        `WORK DAYS` = "1", # numeric
        `W BED TIME` = "0030", # HMS, HM, H
        `W SLEEP PREP` = "01:30", # HMS, HM, H
        `W SLEEP LAT` = "15",
        `W SLEEP END` = "05:30:00", # HMS, HM, H
        `W SLEEP INERTIA` = "0", # M
        `W ALARM` = "Yes", # character (Yes/No)
        `W WAKE BEFORE ALARM` = "No", ## character (Yes/No)
        `W LIGHT EXPOSURE` = "",
        `F BED TIME` = "0030", # HMS, HM, H
        `F SLEEP PREP` = "01:30", # HMS, HM, H
        `F SLEEP LAT` = "15", # M
        `F SLEEP END` = "0900", # HMS, HM, H
        `F SLEEP INERTIA` = "30", # M
        `F ALARM` = "Yes", # character (Yes/No)
        `F REASONS` = "Hobbies", # character
        `F LIGHT EXPOSURE` = "04:00" # HMS, HM, H
        )

    mctq_std <- mctq_std %>% dplyr::add_row(
        `ID` = "",
        `WORK REGULAR` = "", # character (Yes/No)
        `WORK DAYS` = "", # numeric
        `W BED TIME` = "", # HMS, HM, H
        `W SLEEP PREP` = "", # HMS, HM, H
        `W SLEEP LAT` = "",
        `W SLEEP END` = "", # HMS, HM, H
        `W SLEEP INERTIA` = "", # M
        `W ALARM` = "", # character (Yes/No)
        `W WAKE BEFORE ALARM` = "", # character (Yes/No)
        `W LIGHT EXPOSURE` = "",
        `F BED TIME` = "", # HMS, HM, H
        `F SLEEP PREP` = "", # HMS, HM, H
        `F SLEEP LAT` = "", # M
        `F SLEEP END` = "", # HMS, HM, H
        `F SLEEP INERTIA` = "", # M
        `F ALARM` = "", # character (Yes/No)
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "" # HMS, HM, H
    )

    mctq_std <- mctq_std %>% dplyr::add_row(
        `ID` = "",
        `WORK REGULAR` = "", # character (Yes/No)
        `WORK DAYS` = "", # numeric
        `W BED TIME` = "", # HMS, HM, H
        `W SLEEP PREP` = "", # HMS, HM, H
        `W SLEEP LAT` = "",
        `W SLEEP END` = "", # HMS, HM, H
        `W SLEEP INERTIA` = "", # M
        `W ALARM` = "", # character (Yes/No)
        `W WAKE BEFORE ALARM` = "", # character (Yes/No)
        `W LIGHT EXPOSURE` = "",
        `F BED TIME` = "", # HMS, HM, H
        `F SLEEP PREP` = "", # HMS, HM, H
        `F SLEEP LAT` = "", # M
        `F SLEEP END` = "", # HMS, HM, H
        `F SLEEP INERTIA` = "", # M
        `F ALARM` = "", # character (Yes/No)
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "" # HMS, HM, H
    )

    mctq_std <- mctq_std %>% dplyr::add_row(
        `ID` = "",
        `WORK REGULAR` = "", # character (Yes/No)
        `WORK DAYS` = "", # numeric
        `W BED TIME` = "", # HMS, HM, H
        `W SLEEP PREP` = "", # HMS, HM, H
        `W SLEEP LAT` = "",
        `W SLEEP END` = "", # HMS, HM, H
        `W SLEEP INERTIA` = "", # M
        `W ALARM` = "", # character (Yes/No)
        `W WAKE BEFORE ALARM` = "", # character (Yes/No)
        `W LIGHT EXPOSURE` = "",
        `F BED TIME` = "", # HMS, HM, H
        `F SLEEP PREP` = "", # HMS, HM, H
        `F SLEEP LAT` = "", # M
        `F SLEEP END` = "", # HMS, HM, H
        `F SLEEP INERTIA` = "", # M
        `F ALARM` = "", # character (Yes/No)
        `F REASONS` = "", # character
        `F LIGHT EXPOSURE` = "" # HMS, HM, H
    )

    mctq_std

    # Export dataset (leave as comment) --------------------

    #

}

#' @noRd
tidy_mctq_std <- function(raw_data = build_mctq_std()) {

    # Tidy raw dataset --------------------



    # Export dataset (leave as comment) --------------------

    # usethis::use_data(mctq_std)

}

#' A standard MCTQ dataset
#'
#' @description
#'
#' A fictional MCTQ (standard version) dataset for testing and learning.
#'
#' This data was created according to Roenneberg, Wirz-Justice & Merrow
#' ([2003](https://bit.ly/3rLu195)) and the guidelines of The World Wide
#' Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)). See References
#' and Details sections to learn more.
#'
#' The naming of the variables took into account the standard used by theWeP and
#' the guidelines of the [tidyverse style guide](https://style.tidyverse.org/).
#'
#' @details
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ), _cf._
#' Roenneberg, Wirz-Justice & Merrow ([2003](https://bit.ly/3rLu195)).
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
#' @usage data(mctq_std)
#' @source Prepared by Daniel Vartanian (package's author).
#'
#' @references
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
#' Roenneberg, T., Wirz-Justice, A., & Merrow, M. (2003). Life between clocks:
#' daily temporal patterns of human chronotypes. _Journal of Biological
#' Rhythms_, _18_(1), 80-90. doi:
#' [10.1177/0748730402239679](https://doi.org/10.1177/0748730402239679).
#'
#' The Worldwide Experimental Platform (n.d.). MCTQ. Retrieved from
#' <https://www.thewep.org/documentations/mctq/>.
"mctq_std"
