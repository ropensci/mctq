#' A fictional micro MCTQ dataset
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' A fictional dataset composed by micro Munich Chronotype Questionnaire
#' (MCTQ) basic/measurable and computed variables for testing and learning
#' purposes.
#'
#' This data was created following the guidelines in Ghotbi _et.al_ (2020), in
#' addition to the guidelines in Roenneberg, Wirz-Justice, & Merrow (2003),
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), and The World Wide
#' Experimental Platform (theWeP, n.d.). See References and Details sections to
#' learn more.
#'
#' @details
#'
#' `micro_mctq` is a tidied, validated, and transformed version of
#' `raw_data("micro_mctq.csv")`.
#'
#' ## Guidelines
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ),
#' _cf._ Roenneberg, Wirz-Justice, & Merrow (2003), Roenneberg, Allebrandt,
#' Merrow, & Vetter (2012), Roenneberg _et al._ (2015), and Roenneberg, Pilz,
#' Zerbini, & Winnebeck (2019).
#'
#' To know about different MCTQ versions, _cf._ Juda, Vetter, & Roenneberg
#' (2013) and Ghotbi _et.al_ (2020).
#'
#' If you are curious about the variable computations and want to have access to
#' the full questionnaire, _cf._ The Worldwide Experimental Platform (n.d.).
#'
#' ## Data building and data wrangling
#'
#' This dataset was created by randomized sampling and by manual insertions of
#' especial cases. Its purpose is to demonstrate common cases and data issues
#' that researchers may find in their MCTQ data, in addition to be a suggested
#' data structure for MCTQ data.
#'
#' You can see the `micro_mctq` build and data wrangling processes at
#' <https://github.com/gipsousp/mctq/blob/master/data-raw/micro_mctq.R>.
#'
#' ## Variable naming
#'
#' The naming of the variables took into account the naming schemes used in MCTQ
#' articles, in addition to the guidelines of the [tidyverse style
#' guide](https://style.tidyverse.org/).
#'
#' ## Variable classes
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. This classes can be found in the [hms][hms::hms-package]
#' and [lubridate][lubridate::lubridate-package] packages.
#'
#' @format A tibble with `r ncol(micro_mctq)` columns and `r nrow(micro_mctq)`
#'   rows:
#'
#' \describe{
#'   \item{id}{
#'   A unique `integer` value for the purpose of identifying each subject in the
#'   dataset.
#'   \cr \cr
#'   Type: Control.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'
#'   \item{shift_work}{
#'   A `logical` value indicating if the subject have been a shift- or
#'   night-worker in the past three months.
#'   \cr \cr
#'   Statement (`EN`): "I have been a a shift- or night-worker in the past three
#'   months".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{wd}{
#'   Number of workdays per week.
#'   \cr \cr
#'   Statement (`EN`): "Normally, I work ___ days/week".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'   \item{fd}{
#'   Number of work-free days per week.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `integer`.}
#'
#'
#'   \item{so_w}{
#'   Sleep onset on workdays.
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
#'   Sleep end on workdays.
#'   \cr \cr
#'   Statement (`EN`): "On WORKDAYS ... I normally wake up at ___ : ___ AM/PM
#'   (this is NOT when you get out of bed, but rather when you wake up)".
#'   \cr \cr
#'   Type: Basic.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'   \item{sd_w}{
#'   Sleep duration on workdays.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msw}{
#'   Mid-sleep on workdays.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `hms`.}
#'
#'
#'   \item{so_f}{
#'   Sleep onset on work-free days when the subject DON'T use an alarm clock.
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
#'   Sleep end on work-free days when the subject DON'T use an alarm clock.
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
#'   Sleep duration on work-free days when the subject DON'T use an alarm clock.
#'   \cr \cr
#'   Type: Computed.
#'   \cr \cr
#'   R class: `Duration`.}
#'
#'   \item{msf}{
#'   Mid-sleep on work-free days when the subject DON'T use an alarm clock.
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
#'   \item{msf_sc}{
#'   Chronotype or corrected mid-sleep on work-free days.
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
#' @source Prepared by Daniel Vartanian (package's author).
#' @family datasets
#' @template references_b
"micro_mctq"
