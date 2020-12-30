#' @noRd
build_mctq <- function() {

    mctq <- dplyr::tibble(
        `ID` = 1,
        `WORK REGULAR` = "Yes")

    mctq <- mctq %>% dplyr::add_row(
        `ID` = 2,
        `WORK REGULAR` = "No")

}

#' @noRd
tidy_mctq <- function(raw_data = build_mctq()) {



}

#' A MCTQ core dataset
#'
#' @description
#'
#' A fictional MCTQ core (the original) dataset for testing and learning.
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
#' To know about the different MCTQ versions, _cf._ Juda, Vetter &
#' Roenneberg ([2013](https://bit.ly/38IEEk4)) and Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)).
#'
#' If you curious about the computations and want to have access to
#' the full questionnaire, _cf._ The Worldwide Experimental Platform
#' ([n.d.](http://bit.ly/3pv8EH1)).
#'
#' @format A data frame with 10 rows and 18 variables:
#'
#' \describe{
#'   \item{__regular_work_schedule__ (`logical`)}{
#'   A logical value indicating if the subject have a regular work schedule.
#'
#'   Statement (EN): "I have a regular work schedule (this includes being, for
#'   example, a housewife or househusband)").}
#'
#'   \item{__wd__ (`integer`)}{
#'   Number of workdays per week.
#'
#'   Statement (EN): "I have a regular work schedule and work ___ days per
#'   week").}
#'
#'   \item{__bt_w__ (`hms`)}{
#'   Local time of going to bed on workdays.
#'
#'   Statement (EN): "I go to bed at ___ o'clock'".}
#'
#'   \item{__s_prep_w__ (`hms`)}{
#'   Local time of preparing to sleep on workdays.
#'
#'   Statement (EN): "I actually get ready to fall asleep at ___ o'clock".}
#'
#'   \item{__s_lat_w__ (`Duration`)}{
#'   Sleep latency on workdays.
#'
#'   Statement (EN): "I need ___ minutes to fall asleep".}
#'
#'   \item{__se_w__ (`hms`)}{
#'   Sleep end on workdays.
#'
#'   Statement (EN): "I wake up at ___ o'clock".}
#'
#'   \item{__si_w__ (`Duration`)}{
#'   Sleep inertia on workdays.
#'
#'   Statement (EN): "After ___ minutes, I get up".}
#'
#'   \item{__alarm_w__ (`logical`)}{
#'   A logical value indicating if the subject use an alarm clock on workdays.
#'
#'   Statement (EN): "I use an alarm clock on workdays".}
#'
#'   \item{__wake_before_alarm_w__ (`logical`)}{
#'   A logical value indicating if the subject regularly wake up BEFORE the
#'   alarm rings.
#'
#'   Statement (EN): "I regularly wake up BEFORE the alarm rings".}
#'
#'   \item{__le_w__ (`Duration`)}{
#'   Light exposure on workdays.
#'
#'   Statement (EN): "On average, I spend the following amount of time outdoors
#'   in daylight (without a roof above my head)".}
#'
#'   \item{__bt_f__ (`hms`)}{
#'   Local time of going to bed on work-free days.
#'
#'   Statement (EN): "I go to bed at ___ o'clock'".}
#'
#'   \item{__s_prep_f__ (`hms`)}{
#'   Local time of preparing to sleep on work-free days
#'
#'   Statement (EN): "I actually get ready to fall asleep at ___ o'clock".}
#'
#'   \item{__s_lat_f__ (`Duration`)}{
#'   Sleep latency on work-free days.
#'
#'   Statement (EN): "I need ___ minutes to fall asleep".}
#'
#'   \item{__se_f__ (`hms`)}{
#'   Sleep end on work-free days.
#'
#'   Statement (EN): "I wake up at ___ o'clock".}
#'
#'   \item{__si_f__ (`Duration`)}{
#'   Sleep inertia on work-free days.
#'
#'   Statement __(EN)__: "After ___ minutes, I get up".}
#'
#'   \item{__alarm_f__ (`logical`)}{
#'   A logical value indicating if the subject use an alarm clock on work-free
#'   days.
#'
#'   Statement (EN): "My wake-up time is due to the use of an alarm
#'   clock".}
#'
#'   \item{__reasons_f__ (`character`)}{
#'   Particular reasons why the subject cannot freely choose his sleep times on
#'   work-free days.
#'
#'   Statement (EN): "There are particular reasons why I cannot freely choose my
#'   sleep times on free days".}
#'
#'   \item{__le_f__ (`Duration`)}{
#'   Light exposure on work-free days.
#'
#'   Statement (EN): "On average, I spend the following amount of time outdoors
#'   in daylight (without a roof above my head)".}
#' }
#'
#' @usage data(mctq)
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
"mctq"

#' @noRd
build_mctq_shift <- function(){



}

#' @noRd
tidy_mctq_shift <- function(raw_data = build_mctq_shift()){



}

#' A MCTQ Shift dataset
#'
#' @description
#'
#' __UNDER DEVELOPMENT__
#'
#' A fictional MCTQ Shift dataset for dataset for testing and learning.
#'
#' This data was created according to Juda, Vetter & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)) and the guidelines of The World Wide
#' Experimental Platform ([n.d.](http://bit.ly/3pv8EH1)). See References and
#' Details sections to learn more.
#'
#' The naming of the variables took into account the standard used by theWeP and
#' the guidelines of the [tidyverse style guide](https://style.tidyverse.org/).
#'
#' @details
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ), _cf._
#' Roenneberg, Wirz-Justice & Merrow ([2003](https://bit.ly/3rLu195)).
#'
#' To know about the different MCTQ versions, _cf._ Juda, Vetter &
#' Roenneberg ([2013](https://bit.ly/38IEEk4)) and Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)).
#'
#' If you curious about the computations and want to have access to
#' the full questionnaire, _cf._ The Worldwide Experimental Platform
#' ([n.d.](http://bit.ly/3pv8EH1)).
#'
#' @format A data frame with `?` rows and `?` variables:
#'
#' \describe{
#'   \item{__`variable`__}{`Description.`}}
#'
#' @usage data(mctq_shift)
#' @source  Prepared by Daniel Vartanian (package's author).
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
#' Roenneberg, T., Wirz-Justice, A., & Merrow, M. (2003). Life between clocks:
#' daily temporal patterns of human chronotypes. _Journal of Biological
#' Rhythms_, _18_(1), 80-90. doi:
#' [10.1177/0748730402239679](https://doi.org/10.1177/0748730402239679).
#'
#' The Worldwide Experimental Platform (n.d.). MCTQ. Retrieved from
#' <https://www.thewep.org/documentations/mctq/>.
"mctq_shift"

#' @noRd
build_micro_mctq <- function(){



}

#' @noRd
tidy_micro_mctq <- function(raw_data = build_micro_mctq()){



}

#' A \strong{\eqn{\mu}}MCTQ dataset
#'
#' @description
#'
#' __UNDER DEVELOPMENT__
#'
#' A fictional \strong{\eqn{\mu}}MCTQ dataset for testing and learning.
#'
#' This data was created according to Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)). See References and Details sections to
#' learn more.
#'
#' @details
#'
#' To learn more about the Munich Chronotype Questionnaire (MCTQ), _cf._
#' Roenneberg, Wirz-Justice & Merrow ([2003](https://bit.ly/3rLu195)).
#'
#' To know about the different MCTQ versions, _cf._ Juda, Vetter &
#' Roenneberg ([2013](https://bit.ly/38IEEk4)) and Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)).
#'
#' If you curious about the computations and want to have access to
#' the full questionnaire, _cf._ The Worldwide Experimental Platform
#' ([n.d.](http://bit.ly/3pv8EH1)).
#'
#' @format A data frame with `?` rows and `?` variables:
#'
#' \describe{
#'   \item{__`variable`__}{`Description.`}}
#'
#' @usage data(micro_mctq)
#' @aliases mu_mctq
#' @source  Prepared by Daniel Vartanian (package's author).
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
"micro_mctq"
