#' Compute MCTQ average weekly light exposure
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `le_week()` computes the __average weekly light exposure__ for the standard
#' and micro versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `sd_week()` (\eqn{LE_{week}}) computation are as follow.
#'
#' __\deqn{\frac{LE_w \times WD + LE_f \times FD}{7}}__
#'
#' Where:
#'
#' * \eqn{Le_w} = light exposure on workdays.
#' * \eqn{WD} = number of workdays per week.
#' * \eqn{LE_f} = light exposure on work-free days.
#' * \eqn{FD} = number of work-free days per week.
#'
#' @param le_w A `Duration` object corresponding to the __light exposure on work
#'   days__ value from a standard or micro version of the MCTQ questionnaire.
#' @param le_f A `Duration` object corresponding to the __light exposure on
#'   work-free days__ value from a standard or micro version of the MCTQ
#'   questionnaire.
#'
#' @return A `Duration` object corresponding to the average weekly light
#'   exposure.
#'
#' @inheritParams fd
#' @template mctq_b
#' @template mctq_c
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' le_week(5, lubridate::dhours(1.5), lubridate::dhours(3.7))
#' #> [1] "7662.85714285714s (~2.13 hours)" # Expected
#' le_week(6, lubridate::dhours(3), lubridate::dhours(1.5))
#' #> [1] "10028.5714285714s (~2.79 hours)" # Expected
#' le_week(3, lubridate::dhours(5.6), lubridate::as.duration(NA))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' wd <- c(4, 5)
#' le_w <- c(lubridate::dhours(3), lubridate::dhours(2.45))
#' le_f <- c(lubridate::dhours(3), lubridate::dhours(3.75))
#' le_week(wd, le_w, le_f)
#' #> [1] "10800s (~3 hours)" # Expected
#' #> [2] "10157.1428571429s (~2.82 hours)" # Expected
#'
#' ## __ Checking second output from vectorized example __
#' i <- 2
#' x <- c(le_w[i], le_f[i])
#' w <- c(wd[i], fd(wd[i]))
#' lubridate::as.duration(stats::weighted.mean(x, w))
#' #> [1] "10157.1428571429s (~2.82 hours)" # Expected
#'
#' ## __ Converting the output to `hms` __
#' x <- le_week(3, lubridate::dhours(1.25), lubridate::dhours(6.23))
#' convert_to(x, "hms")
#' #> 04:05:44.571429 # Expected
#' convert_to(as.integer(x), "hms") # if you want to discard the milliseconds.
#' #> 04:05:44 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- le_week(2, lubridate::dhours(3.4094), lubridate::dhours(6.2345))
#' x
#' #> [1] "19538.3828571429s (~5.43 hours)" # Expected
#' round_time(x)
#' #> [1] "19538s (~5.43 hours)" # Expected
le_week <- function(wd, le_w, le_f) {

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(le_w)
    assert_duration(le_f)
    assert_identical(wd, le_w, le_f, type = "length")

    ((le_w * wd) + (le_f * fd(wd))) / 7

}
