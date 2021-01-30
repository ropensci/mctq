#' Compute MCTQ weekly sleep loss
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sloss_week()` computes the __weekly sleep loss__ for the standard and micro
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' Please note that this function is not appropriated for use with he MCTQ
#' Shift.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)) and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `sloss_week()` (\eqn{SLossweek}) computation are as follow.
#'
#' __\deqn{If (SDweek > SDw): (SDweek - SDw) * WD}__
#' __\deqn{If (SDweek <= SDw): (SDweek - SDf) * (7 - WD)}__
#'
#' Where:
#'
#' * \eqn{WD} = number of workdays per week.
#' * \eqn{SDw} = sleep duration on workdays.
#' * \eqn{SDf} = sleep duration on work-free days.
#' * \eqn{SDweek} = average weekly sleep duration.
#'
#' @return A `Duration` object corresponding to the weekly sleep loss.
#'
#' @inheritParams sd_week
#' @template details_b
#' @template section_a
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sloss_week(lubridate::dhours(6.5), lubridate::dhours(7), 4)
#' #> [1] "3085.71428571429s (~51.43 minutes)" # Expected
#' sloss_week(lubridate::dhours(7), lubridate::dhours(8), 5)
#' #> [1] "5142.85714285714s (~1.43 hours)" # Expected
#' sloss_week(lubridate::dhours(NA), lubridate::dhours(9.45), 7)
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' sd_w <- c(lubridate::dhours(7), lubridate::dhours(8))
#' sd_f <- c(lubridate::dhours(6.5), lubridate::dhours(8))
#' wd <- c(2, 0)
#' sloss_week(sd_w, sd_f, wd)
#' #> [1] "2571.42857142857s (~42.86 minutes)" # Expected
#' #> [2] "0s"
#'
#' ## __ Converting the output to `hms` __
#' x <- sloss_week(lubridate::dhours(4), lubridate::dhours(5), 3)
#' convert(x, "hms")
#' #> 01:42:51.428571 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- sloss_week(lubridate::dhours(5.8743), lubridate::dhours(7.4324), 6)
#' x
#' #> [1] "4807.85142857144s (~1.34 hours)" # Expected
#' round_time(x)
#' #> [1] "4808s (~1.34 hours)" # Expected
sloss_week <- function(sd_w, sd_f, wd) {

    assert_duration(sd_w)
    assert_duration(sd_f)
    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_identical(sd_w, sd_f, wd, type = "length")

    sd_week <- sd_week(sd_w, sd_f, wd)

    dplyr::case_when(
        sd_week > sd_w ~ (sd_week - sd_w) * wd,
        sd_week <= sd_w ~ (sd_week - sd_f) * fd(wd)
    )

}
