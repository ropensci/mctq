#' A fictional \eqn{\mu}MCTQ dataset
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' A fictional dataset, __for testing and learning purposes__, composed of
#' basic/measurable and computed variables of the Munich Chronotype
#' Questionnaire (MCTQ) micro version.
#'
#' This data was created following the guidelines in Ghotbi _et.al_ (2020), in
#' addition to the guidelines in Roenneberg, Wirz-Justice, & Merrow (2003),
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), and The Worldwide
#' Experimental Platform (n.d.). See the References and Details sections
#' to learn more.
#'
#' @details
#'
#' `micro_mctq` is a tidied, validated, and transformed version of
#' `raw_data("micro_mctq.csv")`.
#'
#' ## Guidelines
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ),
#' see Roenneberg, Wirz-Justice, & Merrow (2003), Roenneberg, Allebrandt,
#' Merrow, & Vetter (2012), Roenneberg et al. (2015), and Roenneberg, Pilz,
#' Zerbini, & Winnebeck (2019).
#'
#' To know about different MCTQ versions, see Juda, Vetter, & Roenneberg
#' (2013) and Ghotbi et.al (2020).
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
#' You can see the `micro_mctq` build and data wrangling processes
#' [here](https://github.com/ropensci/mctq/blob/main/data-raw/micro_mctq.R).
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
#' `pretty_mctq(micro_mctq)`.
#'
#' @format A tibble with `r ncol(mctq::micro_mctq)` columns and
#'  `r nrow(mctq::micro_mctq)` rows:
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
#'   \item{shift_work}{
#'   A `logical` value indicating if the respondent has been a shift- or
#'   night-worker in the past three months.
#'   \cr \cr
#'   Statement (`EN`): "I have been a shift- or night-worker in the past three
#'   months: Yes ( ___ ) No ( ___ )".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{wd}{
#'   Number of __workdays__ per week.
#'   \cr \cr
#'   Statement (`EN`): "Normally, I work ___ days/week".
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
#'   \item{so_w}{
#'   Local time of sleep onset on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "On WORKDAYS ... I normally fall asleep at ___ : ___
#'   AM/PM (this is NOT when you get into bed, but rather when you fall
#'   asleep)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_w}{
#'   Local time of sleep end on __workdays__.
#'   \cr \cr
#'   Statement (`EN`): "On WORKDAYS ... I normally wake up at ___ : ___ AM/PM
#'   (this is NOT when you get out of bed, but rather when you wake up)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sd_w}{
#'   Sleep duration on __workdays__.
#'   \cr \cr
#'   Type: Computed.
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
#'   \item{so_f}{
#'   Local time of sleep onset on __work-free days__ when the respondent
#'   __doesn't__ use an alarm clock to wake up.
#'   \cr \cr
#'   Statement (`EN`): "On WORK-FREE DAYS when I DON'T use an alarm clock ... I
#'   normally fall asleep at ___ : ___ AM/PM (this is NOT when you get into bed,
#'   but rather when you fall asleep)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{se_f}{
#'   Local time of sleep end on __work-free days__ when the respondent
#'   __doesn't__ use an alarm clock to wake up.
#'   \cr \cr
#'   Statement (`EN`): "On WORK-FREE DAYS when I DON'T use an alarm clock ... I
#'   normally wake up at ___ : ___ AM/PM (this is NOT when you get out of bed,
#'   but rather when you wake up)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sd_f}{
#'   Sleep duration on __work-free days__ when the respondent __doesn't__ use an
#'   alarm clock to wake up.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf}{
#'   Local time of mid-sleep on __work-free days__ when the respondent
#'   __doesn't__ use an alarm clock to wake up.
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
"micro_mctq"
