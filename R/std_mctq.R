#' A fictional standard MCTQ dataset
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' A fictional dataset, __for testing and learning purposes__, composed of
#' basic/measurable and computed variables of the Munich Chronotype
#' Questionnaire (MCTQ) standard version.
#'
#' This data was created following the guidelines in Roenneberg, Wirz-Justice, &
#' Merrow (2003), Roenneberg, Allebrandt, Merrow, & Vetter (2012), and The
#' Worldwide Experimental Platform (n.d.). See the References and Details
#' sections to learn more.
#'
#' @details
#'
#' `std_mctq` is a tidied, validated, and transformed version of
#' `raw_data("std_mctq.csv")`.
#'
#' ## Guidelines
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ),
#' see Roenneberg, Wirz-Justice, & Merrow (2003), Roenneberg, Allebrandt,
#' Merrow, & Vetter (2012), Roenneberg et al. (2015), and Roenneberg, Pilz,
#' Zerbini, & Winnebeck (2019).
#'
#' To know about different MCTQ versions, see Juda, Vetter, & Roenneberg
#' (2013) and Ghotbi et al. (2020).
#'
#' If you're curious about the variable computations and want to have access to
#' the full questionnaire, see The Worldwide Experimental Platform (n.d.).
#'
#' ## Data building and data wrangling
#'
#' This dataset was created by randomized sampling (see [mctq::random_mctq()])
#' and by manual insertions of special cases. Its purpose is to demonstrate
#' common cases and data issues that researchers may find in their MCTQ data, in
#' addition to be a suggested data structure for MCTQ data.
#'
#' You can see the `std_mctq` build and data wrangling processes
#' [here](https://github.com/ropensci/mctq/blob/master/data-raw/std_mctq.R).
#'
#' ## Variable naming
#'
#' The naming of the variables took into account the naming scheme used in MCTQ
#' publications, in addition to the guidelines of the [tidyverse style
#' guide](https://style.tidyverse.org/).
#'
#' ## Variable classes
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. These classes can be found in the [hms][hms::hms-package]
#' and [lubridate][lubridate::lubridate-package] package.
#'
#' ## `Duration` objects
#'
#' If you prefer to view `Duration` objects as `hms` objects, run
#' `pretty_mctq(std_mctq)`.
#'
#' @format A tibble with `r ncol(mctq::std_mctq)` columns and
#'   `r nrow(mctq::std_mctq)` rows:
#'
#' \describe{
#'   \item{id}{
#'   A unique `integer` value to identify each respondent in the dataset.
#'   \cr \cr
#'   Type: Control.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'
#'   \item{work}{
#'   A `logical` value indicating if the respondent has a regular work schedule.
#'   \cr \cr
#'   Statement (`EN`): "I have a regular work schedule (this includes being, for
#'   example, a housewife or househusband): Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{wd}{
#'   Number of __workdays__ per week.
#'   \cr \cr
#'   Statement (`EN`): "I have a regular work schedule and work ___ days per
#'   week".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'   \item{fd}{
#'   Number of __work-free days__ per week.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'
#'   \item{bt_w}{
#'   Local time of going to bed on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "I go to bed at ___ o'clock'".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sprep_w}{
#'   Local time of preparing to sleep on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "I actually get ready to fall asleep at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{slat_w}{
#'   Sleep latency or time to fall asleep after preparing to sleep on
#'   __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "I need ___ minutes to fall asleep".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{so_w}{
#'   Local time of sleep onset on __workdays__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_w}{
#'   Local time of sleep end on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "I wake up at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{si_w}{
#'   "Sleep inertia" on __workdays__.
#'   \cr \cr
#'   Despite the name, this variable represents the time the respondent takes to
#'   get up after sleep end.
#'   \cr \cr
#'   Statement (`EN`): "After ___ minutes, I get up".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{gu_w}{
#'   Local time of getting out of bed on __workdays__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{alarm_w}{
#'   A `logical` value indicating if the respondent uses an alarm clock to wake
#'   up on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "I use an alarm clock on workdays: Yes ( ___ ) No ( ___
#'   )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{wake_before_w}{
#'   A `logical` value indicating if the respondent regularly wakes up
#'   __before__ the alarm rings on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": I regularly wake up BEFORE the alarm rings:
#'   Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{sd_w}{
#'   Sleep duration on __workdays__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{tbt_w}{
#'   Total time in bed on __workdays__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{le_w}{
#'   Light exposure on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "On average, I spend the following amount of time
#'   outdoors in daylight (without a roof above my head)".
#'   \cr \cr
#'   Type: Extra.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msw}{
#'   Local time of mid-sleep on __workdays__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'
#'   \item{bt_f}{
#'   Local time of going to bed on __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "I go to bed at ___ o'clock'".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sprep_f}{
#'   Local time of preparing to sleep on __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "I actually get ready to fall asleep at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{slat_f}{
#'   Sleep latency or time to fall asleep after preparing to sleep on
#'   __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "I need ___ minutes to fall asleep".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{so_f}{
#'   Local time of sleep onset on __work-free days__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_f}{
#'   Local time of sleep end on __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "I wake up at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{si_f}{
#'   "Sleep inertia" on __work-free days__.
#'   \cr \cr
#'   Despite the name, this variable represents the time the respondent takes to
#'   get up after sleep end.
#'   \cr \cr
#'   Statement (`EN`): "After ___ minutes, I get up".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{gu_f}{
#'   Local time of getting out of bed on __work-free days__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{alarm_f}{
#'   A `logical` value indicating if the respondent uses an alarm clock to wake
#'   up on __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "My wake-up time is due to the use of an alarm
#'   clock: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_f}{
#'   A `logical` value indicating if the respondent has any particular reasons
#'   for why they __cannot__ freely choose their sleep times on __work-free
#'   days__.
#'   \cr \cr
#'   Statement (`EN`): "There are particular reasons why I __cannot__ freely
#'   choose my sleep times on free days: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_why_f}{
#'   Particular reasons for why the respondent cannot freely choose their sleep
#'   times on __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": Child(ren)/pet(s) ( ___ ) Hobbies ( ___ )
#'   Others ( ___ ), for example: ___".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `character`.}
#'
#'   \item{sd_f}{
#'   Sleep duration on __work-free days__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{tbt_f}{
#'   Total time in bed on __work-free days__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{le_f}{
#'   Light exposure on __work-free days__.
#'   \cr \cr
#'   Statement (`EN`): "On average, I spend the following amount of time
#'   outdoors in daylight (without a roof above my head)".
#'   \cr \cr
#'   Type: Extra.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf}{
#'   Local time of mid-sleep on __work-free days__.
#'   \cr \cr
#'   Type: Computed.
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
#'   \item{sloss_week}{
#'   Weekly sleep loss.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{le_week}{
#'   Average weekly light exposure.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf_sc}{
#'   Chronotype or corrected local time of mid-sleep on __work-free days__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sjl_rel}{
#'   Relative social jetlag.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sjl}{
#'   Absolute social jetlag.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#' }
#'
#' @source Created by Daniel Vartanian (package author).
#' @family datasets
#' @template references_b
"std_mctq"
