#' Compute MCTQ weekly sleep loss
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sloss_week()` computes the __weekly sleep loss__ for the standard and micro
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012) and The Worldwide
#' Experimental Platform (n.d.) guidelines for `sloss_week()`
#' (\eqn{SLoss_{week}}{SLoss_week}) computation are as follows.
#'
#' ## Notes
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{\textrm{If } SD_{week} > SD_W \; , \; (SD_{week} - SD_W) \times WD}{
#' If SD_week > SD_W, (SD_week - SD_W) * WD}__
#' __\deqn{\textrm{If } SD_{week} \leq SD_W \; , \; (SD_{week} - SD_F) \times
#' FD}{If SD_week <= SD_W, (SD_week - SD_F) * FD}__
#'
#' Where:
#'
#' * \eqn{SD_W} = sleep duration on workdays.
#' * \eqn{SD_F} = sleep duration on work-free days.
#' * \eqn{SD_{week}}{SD_week} = average weekly sleep duration.
#' * \eqn{WD} = number of workdays per week ("I have a regular work schedule and
#' work ___ days per week").
#' * \eqn{FD} = number of work-free days per week.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
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
#' ## Scalar example
#'
#' sd_w <- lubridate::dhours(6.5)
#' sd_f <- lubridate::dhours(7)
#' wd <- 4
#' sloss_week(sd_w, sd_f, wd)
#' #> [1] "3085.71428571429s (~51.43 minutes)" # Expected
#'
#' sd_w <- lubridate::dhours(7)
#' sd_f <- lubridate::dhours(8)
#' wd <- 5
#' sloss_week(sd_w, sd_f, wd)
#' #> [1] "5142.85714285714s (~1.43 hours)" # Expected
#'
#' sd_w <- lubridate::dhours(NA)
#' sd_f <- lubridate::dhours(9.45)
#' wd <- 7
#' sloss_week(sd_w, sd_f, wd)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' sd_w <- c(lubridate::dhours(7), lubridate::dhours(8))
#' sd_f <- c(lubridate::dhours(6.5), lubridate::dhours(8))
#' wd <- c(2, 0)
#' sloss_week(sd_w, sd_f, wd)
#' #> [1] "2571.42857142857s (~42.86 minutes)" "0s" # Expected
#'
#' ## Converting the output to `hms`
#'
#' sd_w <- lubridate::dhours(4)
#' sd_f <- lubridate::dhours(5)
#' wd <- 3
#' x <- sloss_week(sd_w, sd_f, wd)
#' x
#' #> [1] "6171.42857142858s (~1.71 hours)" # Expected
#' hms::as_hms(as.numeric(x))
#' #> 01:42:51.428571 # Expected
#'
#' ## Rounding the output at the seconds level
#'
#' sd_w <- lubridate::dhours(5.8743)
#' sd_f <- lubridate::dhours(7.4324)
#' wd <- 6
#' x <- sloss_week(sd_w, sd_f, wd)
#' x
#' #> [1] "4807.85142857144s (~1.34 hours)" # Expected
#' round_time(x)
#' #> [1] "4808s (~1.34 hours)" # Expected
sloss_week <- function(sd_w, sd_f, wd) {
    assert_duration(sd_w)
    assert_duration(sd_f)
    checkmate::assert_integerish(wd)
    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_identical(sd_w, sd_f, wd, type = "length")

    ## `sum_1` and `sum_2` exists to remove unnecessary warnings of the
    ## lubridate package when subtracting objects of class `Duration`.

    wd <- as.integer(wd)
    sd_week <- sd_week(sd_w, sd_f, wd)
    sum_1 <- vct_sum_time(sd_week, - sd_w) %>% lubridate::as.duration()
    sum_2 <- vct_sum_time(sd_week, - sd_f) %>% lubridate::as.duration()

    dplyr::case_when(
        sd_week > sd_w ~ sum_1 * wd,
        sd_week <= sd_w ~ sum_2 * fd(wd)
    )
}
