#' Compute MCTQ chronotype or corrected midsleep on work-free days
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `msf_sc()` computes the __chronotype or corrected midsleep on free days__ for
#' standard, micro, and shift versions of the Munich Chronotype Questionnaire
#' (MCTQ).
#'
#' `chronotype()` is just a wrapper for `msf_sc()`.
#'
#' Note that, for epidemiological and genetic studies, `msf_sc` must be
#' normalized for age and sex to make populations of different age and sex
#' compositions comparable (Roenneberg, Allebrandt, Merrow, & Vetter,
#' [2012](http://bit.ly/3iGEgqX)).
#'
#' When using the shift version of the MCTQ, replace the value of `sd_week` to
#' `sd_overall`, as instructed in the Arguments section.
#'
#' Also note that the basis for estimating chronotype in shift-workers is the
#' mid-sleep time on free days after evening shifts (\eqn{MSF_E}). In case work
#' schedules do not comprise evening shifts, Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)) propose to derive it from the corrected
#' midsleep time on free days of other shifts (_e.g._ by using a linear model).
#' Unfortunately the `mctq` package can't help you with that, as it requires a
#' closer look at your data.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `msf_sc()` (\eqn{MSFsc}) computation are as follow.
#'
#' Note that, for all cases, \eqn{MSFsc} cannot be computed if the participant
#' wake up with an alarm clock on free days.
#'
#' ### For standard and micro versions of the MCTQ:
#'
#' __\deqn{If SDf <= SDw: MSF}__
#' __\deqn{If SDf > SDw: MSF - (SDf - SDweek) / 2}__
#'
#' Where:
#'
#' * \eqn{MSF} = mid-sleep on work-free days.
#' * \eqn{SDw} = sleep duration on work days.
#' * \eqn{SDf} = sleep duration on work-free days.
#' * \eqn{SDweek} = average weekly sleep duration.
#'
#' \eqn{MSFsc} is the participant chronotype in standard and micro versions of
#' the MCTQ.
#'
#' ### For the shift version of the MCTQ:
#'
#' __\deqn{If SDf <= SDw: MSF}__
#' __\deqn{If SDf > SDw: MSF - (SDf - OSD) / 2}__
#'
#' Where:
#'
#' * \eqn{MSF} = mid-sleep on free days after shift.
#' * \eqn{SDw} = sleep duration on shift.
#' * \eqn{SDf} = sleep duration on free days after shift.
#' * \eqn{OSD} = overall sleep duration.
#'
#' Note that the basis for estimating chronotype in shift-workers is the
#' mid-sleep time on free days after evening shifts (\eqn{MSF_E}). In case work
#' schedules do not comprise evening shifts, Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)) propose to derive it from the corrected
#' midsleep time on free days of other shifts (_e.g._ by using a linear model).
#' Unfortunately the `mctq` package can't help you with that, as it requires a
#' closer look at your data.
#'
#' @param msf A `hms` object corresponding to the __mid-sleep on work-free
#'   days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire (you can use [mctq::ms()] to compute it).
#' @param sd_w A `Duration` object corresponding to the __sleep duration on work
#'   days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire (you can use [mctq::sd()] to compute it).
#' @param sd_f A `Duration` object corresponding to the __sleep duration on
#'   work-free days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire (you can use [mctq::sd()] to compute it).
#' @param sd_week A `Duration` object corresponding to the __average weekly
#'   sleep duration__ value from a standard or micro version of the MCTQ
#'   questionnaire (you can use [mctq::sd_week()] to compute it) __or__ the
#'   __overall sleep duration__ value from a MCTQ shift (you can use
#'   [mctq::sd_overall()] to compute it).
#' @param alarm_f A `logical` object corresponding to the __alarm clock use on
#'   work-free days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire. Note that, if `alarm_f == TRUE`, `msf_sc` cannot be
#'   computed. `msf_sc()` will return `NA` for those cases.
#'
#' @return A `hms` object corresponding to the chronotype or corrected midsleep
#'   on free days.
#'
#' @template mctq_b
#' @template mctq_d
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' msf_sc(hms::parse_hms("04:00:00"), lubridate::dhours(6),
#'        lubridate::dhours(7), lubridate::dhours(6.29), FALSE)
#' #> 03:38:42 # Expected
#' msf_sc(hms::parse_hms("06:30:00"), lubridate::dhours(7.5),
#'        lubridate::dhours(7.5), lubridate::dhours(7.5), FALSE)
#' #> 06:30:00 # Expected
#' msf_sc(hms::parse_hm("01:00:00"), lubridate::dhours(5.5),
#'        lubridate::dhours(9), lubridate::dhours(6.75), FALSE)
#' #> 23:52:30 # Expected
#' msf_sc(hms::parse_hms("05:40:00"), lubridate::dhours(7.5),
#'        lubridate::dhours(10), lubridate::dhours(8.5), TRUE)
#' #> NA # Expected (chronotype cannot be computed if `alarm_w == TRUE`)
#'
#' ## __ Vectorized example __
#' msf <- c(hms::parse_hms("03:45:00"), hms::parse_hm("04:45:00"))
#' sd_w <- c(lubridate::dhours(5), lubridate::dhours(6.45))
#' sd_f <- c(lubridate::dhours(9), lubridate::dhours(10))
#' sd_week <- c(lubridate::dhours(8.5), lubridate::dhours(9.2))
#' alarm_f <- c(FALSE, FALSE)
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 03:30:00 # Expected
#' #> 04:21:00 # Expected
#'
#' ## __ A wrapper for msf_sc() __
#' chronotype(hms::parse_hms("07:00:00"), lubridate::dhours(6),
#'            lubridate::dhours(12), lubridate::dhours(9.45), FALSE)
#' #> 05:43:30 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- msf_sc(hms::parse_hms("05:40:00"), lubridate::dhours(5.43678),
#'             lubridate::dhours(9.345111), lubridate::dhours(7.5453), FALSE)
#' x
#' #> 04:46:02.340202 # Expected
#' round_time(x)
#' #> 04:46:02 # Expected
msf_sc <- function(msf, sd_w, sd_f, sd_week, alarm_f) {

    checkmate::assert_class(msf, "hms")
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_duration(sd_week)
    checkmate::assert_logical(alarm_f)
    assert_identical(msf, sd_w, sd_f, sd_week, alarm_f, type = "length")

    ## `sc` exists to remove unnecessary warnings of lubridate package when
    ## subtracting objects of class `Duration`.

    sc <- sum_time(sd_f, - sd_week, class = "Duration", vectorize = TRUE)
    sc <- sc / 2

    dplyr::case_when(
        alarm_f == TRUE ~ hms::as_hms(NA),
        sd_f <= sd_w ~ msf,
        sd_f > sd_w ~ sum_time(msf, - sc, class = "hms",
                               clock = TRUE, vectorize = TRUE)
    )

}

#' @rdname msf_sc
#' @export
chronotype <- function(msf, sd_w, sd_f, sd_week, alarm_f) {

    msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)

}
