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
#' Roenneberg, Allebrandt, Merrow, & Vetter ([2012](http://bit.ly/3iGEgqX)),
#' Ghotbi _et.al_ ([2020](https://bit.ly/34VhA0l)), and theWeP
#' [(n.d.)](http://bit.ly/3pv8EH1) guidelines for `le_week()`
#' (\eqn{LE_{week}}{LE_week}) computation are as follow.
#'
#' ### Notes
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ### Computation
#'
#' __\deqn{\frac{LE_W \times WD + LE_F \times FD}{7}}{
#' (LE_W * WD + LE_F * FD) / 7}__
#'
#' Where:
#'
#' * \eqn{LE_W} = light exposure on workdays.
#' * \eqn{LE_F} = light exposure on work-free days.
#' * \eqn{WD} = number of workdays per week.
#' * \eqn{FD} = number of work-free days per week.
#'
#' \strong{*} \eqn{W} = work days; \eqn{F} = work-free days.
#'
#' @param le_w A `Duration` object corresponding to the __light exposure on work
#'   days__ value from a standard or micro version of the MCTQ questionnaire.
#' @param le_f A `Duration` object corresponding to the __light exposure on
#'   work-free days__ value from a standard or micro version of the MCTQ
#'   questionnaire.
#'
#' @return A `Duration` object corresponding to the weighted mean of `le` and
#'   `wd` and `fd(wd)` (weights).
#'
#' @inheritParams fd
#' @template details_b
#' @template section_a
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' le_week(lubridate::dhours(1.5), lubridate::dhours(3.7), 5)
#' #> [1] "7662.85714285714s (~2.13 hours)" # Expected
#' le_week(lubridate::dhours(3), lubridate::dhours(1.5), 6)
#' #> [1] "10028.5714285714s (~2.79 hours)" # Expected
#' le_week(lubridate::dhours(5.6), lubridate::as.duration(NA), 3)
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' le_w <- c(lubridate::dhours(3), lubridate::dhours(2.45))
#' le_f <- c(lubridate::dhours(3), lubridate::dhours(3.75))
#' wd <- c(4, 5)
#' le_week(le_w, le_f, wd)
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
#' x <- le_week(lubridate::dhours(1.25), lubridate::dhours(6.23), 3)
#' convert(x, "hms")
#' #> 04:05:44.571429 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- le_week(lubridate::dhours(3.4094), lubridate::dhours(6.2345), 2)
#' x
#' #> [1] "19538.3828571429s (~5.43 hours)" # Expected
#' round_time(x)
#' #> [1] "19538s (~5.43 hours)" # Expected
le_week <- function(le_w, le_f, wd) {

    assert_duration(le_w)
    assert_duration(le_f)
    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_identical(wd, le_w, le_f, type = "length")

    ((le_w * wd) + (le_f * fd(wd))) / 7

}
