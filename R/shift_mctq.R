#' A fictional MCTQ\eqn{^{Shift}}{ Shift} dataset
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' A fictional dataset, for __testing and learning purposes__, composed of
#' basic/measurable and computed variables of the Munich Chronotype
#' Questionnaire (MCTQ) shift version.
#'
#' This data was created following the guidelines in Juda, Vetter, & Roenneberg
#' (2013), in addition to the guidelines found in Roenneberg, Wirz-Justice, &
#' Merrow (2003), Roenneberg, Allebrandt, Merrow, & Vetter (2012), and The
#' Worldwide Experimental Platform (n.d.). See the References and Details
#' sections to learn more.
#'
#' @details
#'
#' `shift_mctq` is a tidied, validated, and transformed version of
#' `raw_data("shift_mctq.csv")`.
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
#' You can see the `shift_mctq` build and data wrangling processes
#' [here](https://github.com/ropensci/mctq/blob/master/data-raw/shift_mctq.R).
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
#' `pretty_mctq(shift_mctq)`.
#'
#' @format A tibble with `r ncol(shift_mctq)` columns and `r nrow(shift_mctq)`
#'   rows:
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
#'   \item{n_w_m}{
#'   Number of days __worked in morning shifts__ within a shift cycle.
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'   \item{bt_w_m}{
#'   Local time of going to bed on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I go to bed at ___ o'clock'".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sprep_w_m}{
#'   Local time of preparing to sleep on workdays __between two morning
#'   shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I actually get ready to fall asleep at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{slat_w_m}{
#'   Sleep latency or time to fall asleep after preparing to sleep on workdays
#'   __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I need ___ minutes to fall asleep".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{so_w_m}{
#'   Local time of sleep onset on workdays __between two morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_w_m}{
#'   Local time of sleep end on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I wake up at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{tgu_w_m}{
#'   Time to get up on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I get up after ___ minutes".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{gu_w_m}{
#'   Local time of getting out of bed on workdays __between two morning
#'   shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{alarm_w_m}{
#'   A `logical` value indicating if the respondent uses an alarm clock to wake
#'   up on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I wake up at ___ o'clock: ( ___ ) with alarm ( ___ )
#'   without alarm".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_w_m}{
#'   A `logical` value indicating if the respondent has any particular reasons
#'   for why they __cannot__ freely choose their sleep times on workdays
#'   __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "There are particular reasons why I __cannot__ freely
#'   choose my sleep times on morning shifts: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_why_w_m}{
#'   Particular reasons for why the respondent cannot freely choose their sleep
#'   times on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": Child(ren)/pet(s) ( ___ ) Hobbies ( ___ )
#'   Others, for example: ___".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `character`.}
#'
#'   \item{sd_w_m}{
#'   Sleep duration on workdays __between two morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{tbt_w_m}{
#'   Total time in bed on workdays __between two morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msw_m}{
#'   Local time of mid-sleep on workdays __between two morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{nap_w_m}{
#'   A `logical` value indicating if the respondent usually takes a nap on
#'   workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I usually take a nap: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{napo_w_m}{
#'   Local time of nap onset on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": I take a nap from ___ o'clock to ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{nape_w_m}{
#'   Local time of nap end on workdays __between two morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": I take a nap from ___ o'clock to ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{napd_w_m}{
#'   Nap duration on workdays __between two morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sd24_w_m}{
#'   24 hours sleep duration (sleep duration + nap duration) on workdays
#'   __between two morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'
#'   \item{n_f_m}{
#'   Number of free days __after working in morning shifts__ within a shift
#'   cycle.
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'   \item{bt_f_m}{
#'   Local time of going to bed on work-free days __between two free days after
#'   morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I go to bed at ___ o'clock'".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sprep_f_m}{
#'   Local time of preparing to sleep on work-free days __between two free days
#'   after morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I actually get ready to fall asleep at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{slat_f_m}{
#'   Sleep latency or time to fall asleep after preparing to sleep on work-free
#'   days __between two free days after morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I need ___ minutes to fall asleep".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{so_f_m}{
#'   Local time of sleep onset on work-free days __between two free days after
#'   morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_f_m}{
#'   Local time of sleep end on work-free days __between two free days after
#'   morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I wake up at ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{tgu_f_m}{
#'   Time to get up on work-free days __between two free days after morning
#'   shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I get up after ___ minutes".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{gu_f_m}{
#'   Local time of getting out of bed on work-free days __between two free days
#'   after morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{alarm_f_m}{
#'   A `logical` value indicating if the respondent uses an alarm clock to wake
#'   up on work-free days __between two free days after morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I wake up at ___ o'clock: ( ___ ) with alarm ( ___ )
#'   without alarm".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_f_m}{
#'   A `logical` value indicating if the respondent has any particular reasons
#'   for why they __cannot__ freely choose their sleep times on work-free days
#'   __between two free days after morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "There are particular reasons why I __cannot__ freely
#'   choose my sleep times on morning shifts: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{reasons_why_f_m}{
#'   Particular reasons for why the respondent cannot freely choose their sleep
#'   times on work-free days __between two free days after morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": Child(ren)/pet(s) ( ___ ) Hobbies ( ___ )
#'   Others, for example: ___".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `character`.}
#'
#'   \item{sd_f_m}{
#'   Sleep duration on work-free days __between two free days after morning
#'   shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{tbt_f_m}{
#'   Total time in bed on work-free days __between two free days after morning
#'   shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf_m}{
#'   Local time of mid-sleep on work-free days __between two free days after
#'   morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{nap_f_m}{
#'   A `logical` value indicating if the respondent usually takes a nap on
#'   work-free days __between two free days after morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "I usually take a nap: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{napo_f_m}{
#'   Local time of nap onset on work-free days __between two free days after
#'   morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": I take a nap from ___ o'clock to ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{nape_f_m}{
#'   Local time of nap end on work-free days __between two free days after
#'   morning shifts__.
#'   \cr \cr
#'   Statement (`EN`): "If "Yes": I take a nap from ___ o'clock to ___ o'clock".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{napd_f_m}{
#'   Nap duration on work-free days __between two free days after morning
#'   shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sd24_f_m}{
#'   24 hours sleep duration (sleep duration + nap duration) on work-free days
#'   __between two free days after morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'
#'   \item{sd_overall_m}{
#'   Overall sleep duration considering workdays __between two morning shifts__
#'   and work-free days __between two free days after morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf_sc_m}{
#'   Corrected local time of mid-sleep on work-free days __between two free days
#'   after morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sjl_rel_m}{
#'   Relative social jetlag considering workdays __between two morning shifts__
#'   and work-free days __between two free days after morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{sjl_m}{
#'   Absolute social jetlag considering workdays __between two morning shifts__
#'   and work-free days __between two free days after morning shifts__.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'
#'   \item{...}{
#'   For brevity, the subsequent variables, except for __sjl_weighted__
#'   (described below), are not shown here. That's because they have
#'   the same configurations of the variables shown above, differing only
#'   by shift (__evening shift__ (`_e`) and __night shift__ (`_n`)).}
#'
#'
#'   \item{sjl_weighted}{
#'   Absolute social jetlag across all shifts.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#' }
#'
#' @source Created by Daniel Vartanian (package author).
#' @family datasets
#' @template references_b
"shift_mctq"
