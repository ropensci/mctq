#' Compute MCTQ local time of mid-sleep
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `ms()` computes the __local time of mid-sleep__ for standard, micro, and
#' shift versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi et al. (2020), Juda,
#' Vetter, & Roenneberg (2013), and The Worldwide Experimental Platform (n.d.)
#' guidelines for `ms()` (\eqn{MSW} or \eqn{MSF}) computation are as follows.
#'
#' ## Notes
#'
#' * The computation below must be applied to each section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{SO_{W/F} + \frac{SD_{W/F}}{2}}{SO_W/F + (SD_W/F / 2)}__
#'
#' Where:
#'
#' * \eqn{SO_{W/F}}{SO_W/F} = local time of sleep onset on work __or__ work-free
#' days.
#' * \eqn{SD_{W/F}}{SD_W/F} = sleep duration on work __or__ work-free days.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{SO_{W/F}^{M/E/N} + \frac{SD_{W/F}^{M/E/N}}{2}}{
#' SO_W/F_M/E/N + (SD_W/F_M/E/N / 2)}__
#'
#' Where:
#'
#' * \eqn{SO_{W/F}^{M/E/N}}{SO_W/F_M/E/N} = local time of sleep onset between
#' two days in a particular shift __or__ between two free days after a
#' particular shift.
#' * \eqn{SD_{W/F}^{M/E/N}}{SD_W/F_M/E/N} = sleep duration between two days in a
#' particular shift __or__ between two free days after a particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param so A `hms` object corresponding to the __local time of sleep onset__
#'   from a standard, micro, or shift version of the MCTQ questionnaire. You can
#'   use [mctq::so()] to compute it for the standard or shift version.
#' @param sd A `Duration` object corresponding to the __sleep duration__ from a
#'   standard, micro, or shift version of the MCTQ questionnaire. You can use
#'   [mctq::sd()] to compute it for any MCTQ version.
#'
#' @return A `hms` object corresponding to the vectorized sum of `so` and `(sd /
#'   2)` in a circular time frame of 24 hours.
#'
#' @aliases msw msf
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' so <- hms::parse_hm("23:30")
#' sd <- lubridate::dhours(8)
#' ms(so, sd)
#' #> 03:30:00 # Expected
#'
#' so <- hms::parse_hm("01:00")
#' sd <- lubridate::dhours(10)
#' ms(so, sd)
#' #> 06:00:00 # Expected
#'
#' so <- hms::as_hms(NA)
#' sd <- lubridate::dhours(7.5)
#' ms(so, sd)
#' #> NA # Expected
#'
#' ## Vector example
#'
#' so <- c(hms::parse_hm("00:10"), hms::parse_hm("01:15"))
#' sd <- c(lubridate::dhours(9.25), lubridate::dhours(5.45))
#' ms(so, sd)
#' #> [1] 04:47:30 # Expected
#' #> [1] 03:58:30 # Expected
ms <- function(so, sd) {
    checkmate::assert_class(so, "hms")
    assert_duration(sd)
    assert_identical(so, sd, type = "length")

    sum_time(so, (sd / 2), class = "hms", circular = TRUE, vectorize = TRUE)
}

#' Compute MCTQ corrected local time of mid-sleep on work-free days
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `msf_sc()` computes the __chronotype or corrected local time of mid-sleep on
#' work-free days__ for standard, micro, and shift versions of the Munich
#' Chronotype Questionnaire (MCTQ).
#'
#' `chronotype()` is just a wrapper for `msf_sc()`.
#'
#' When using the shift version of the MCTQ, replace the value of `sd_week` to
#' `sd_overall`, as instructed in the Arguments section.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi et al. (2020), Juda,
#' Vetter, & Roenneberg (2013), and The Worldwide Experimental Platform (n.d.)
#' guidelines for `msf_sc()` (\eqn{MSF_{sc}}{MSF_sc}) computation are as
#' follows.
#'
#' ## Notes
#'
#' * For all cases, \eqn{MSF_{sc}}{MSF_sc} cannot be computed if the participant
#' wakes up with an alarm clock on work-free days (\eqn{Alarm_F}{Alarm_F}).
#'
#' * For MCTQ\eqn{^{Shift}}{ Shift}, the computation below must be applied to
#' each shift section of the questionnaire.
#'
#' * \eqn{MSF_{sc}}{MSF_sc} is a proxy for the participant chronotype in
#' standard and micro versions of the MCTQ.
#'
#' * The basis for estimating chronotype in shift-workers is the mid-sleep on
#' work-free days after evening shifts (\eqn{MSF^E}{MSF_E}). In case work
#' schedules do not comprise evening shifts, Juda, Vetter, & Roenneberg (2013)
#' propose to derive it from the \eqn{MSF_{sc}}{MSF_sc} of other shifts (e.g.,
#' by using a linear model). Unfortunately, the `mctq` package can't help you
#' with that, as it requires a closer look at your data.
#'
#' * \eqn{MSF_{sc}}{MSF_sc} depends on developmental and environmental
#' conditions (e.g., age, light exposure). For epidemiological and genetic
#' studies, \eqn{MSF_{sc}}{MSF_sc} must be normalized for age and sex to make
#' populations of different age and sex compositions comparable (Roenneberg,
#' Allebrandt, Merrow, & Vetter, 2012).
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{\textrm{If } SD_F \leq SD_W \; , \; MSF}{If SD_F <= SD_W, MSF}__
#' __\deqn{\textrm{If } SD_F > SD_W \; , \; MSF - \frac{SD_F - SD_{week}}{2}}{
#' If SD_F > SD_W, MSF - (SD_F - SD_week) / 2}__
#'
#' Where:
#'
#' * \eqn{MSF} = local time of mid-sleep on work-free days.
#' * \eqn{SD_W} = sleep duration on workdays.
#' * \eqn{SD_F} = sleep duration on work-free days.
#' * \eqn{SD_{week}}{SD_week} = average weekly sleep duration.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{\textrm{If } SD_{F}^{M/E/N} \leq SD_{W}^{M/E/N} \; , \; MSF^{M/E/N}}{
#' If SD_F_M/E/N <= SD_W_M/E/N, MSF}__
#' __\deqn{\textrm{If } SD_{F}^{M/E/N} > SD_{W}^{M/E/N} \; , \; MSF^{M/E/N} -
#' \frac{SD_{F}^{M/E/N} - \emptyset SD^{M/E/N}}{2}}{If SD_F_M/E/N > SD_W_M/E/N,
#' MSF - (SD_F_M/E/N - OSD_M/E/N) / 2}__
#'
#' Where:
#'
#' * \eqn{MSF^{M/E/N}}{MSF_M/E/N} = local time of mid-sleep between two free
#' days after a particular shift.
#' * \eqn{SD_{W}^{M/E/N}}{SD_W_M/E/N} = sleep duration between two days in a
#' particular shift.
#' * \eqn{SD_{F}^{M/E/N}}{SD_F_M/E/N} = sleep duration between two free days
#' after a particular shift.
#' * \eqn{\emptyset SD^{M/E/N}}{OSD_M/E/N} = overall sleep duration of a
#' particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param msf A `hms` object corresponding to the __local time of mid-sleep on
#'   work-free days__ from a standard, micro, or shift version of the MCTQ
#'   questionnaire. You can use [mctq::ms()] to compute it.
#' @param sd_w A `Duration` object corresponding to the __sleep duration on work
#'   days__ from a standard, micro, or shift version of the MCTQ questionnaire.
#'   You can use [mctq::sd()] to compute it.
#' @param sd_f A `Duration` object corresponding to the __sleep duration on
#'   work-free days__ from a standard, micro, or shift version of the MCTQ
#'   questionnaire. You can use [mctq::sd()] to compute it.
#' @param sd_week A `Duration` object corresponding to the __average weekly
#'   sleep duration__ from a standard or micro version of the MCTQ questionnaire
#'   (you can use [mctq::sd_week()] to compute it) __or__ the __overall sleep
#'   duration of a particular shift__ from a shift version of the MCTQ
#'   questionnaire (you can use [mctq::sd_overall()] to compute it).
#' @param alarm_f A `logical` object corresponding to the __alarm clock use on
#'   work-free days__ from a standard, micro, or shift version of the MCTQ
#'   questionnaire. Note that, if `alarm_f == TRUE`, `msf_sc` cannot be
#'   computed, `msf_sc()` will return `NA` for those cases. For the
#'   \eqn{\mu}MCTQ, this value must be set as `FALSE` all times, since the
#'   questionnaire considers only the work-free days when the respondent does
#'   not use an alarm.
#'
#' @return A `hms` object corresponding to the MCTQ chronotype or corrected
#'   local time of mid-sleep on work-free days.
#'
#' @template details_b
#' @template section_a
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' msf <- hms::parse_hms("04:00:00")
#' sd_w <- lubridate::dhours(6)
#' sd_f <- lubridate::dhours(7)
#' sd_week <- lubridate::dhours(6.29)
#' alarm_f <- FALSE
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 03:38:42 # Expected
#'
#' msf <- hms::parse_hm("01:00:00")
#' sd_w <- lubridate::dhours(5.5)
#' sd_f <- lubridate::dhours(9)
#' sd_week <- lubridate::dhours(6.75)
#' alarm_f <- FALSE
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 23:52:30 # Expected
#'
#' msf <- hms::parse_hms("05:40:00")
#' sd_w <- lubridate::dhours(7.5)
#' sd_f <- lubridate::dhours(10)
#' sd_week <- lubridate::dhours(8.5)
#' alarm_f <- TRUE
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> NA # Expected (`msf_sc` cannot be computed if `alarm_f == TRUE`)
#'
#' ## Vector example
#'
#' msf <- c(hms::parse_hms("03:45:00"), hms::parse_hm("04:45:00"))
#' sd_w <- c(lubridate::dhours(9), lubridate::dhours(6.45))
#' sd_f <- c(lubridate::dhours(5), lubridate::dhours(10))
#' sd_week <- c(lubridate::dhours(8.5), lubridate::dhours(9.2))
#' alarm_f <- c(FALSE, FALSE)
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 03:45:00 # Expected
#' #> 04:21:00 # Expected
#'
#' ## A wrapper for msf_sc()
#'
#' msf <- hms::parse_hms("07:00:00")
#' sd_w <- lubridate::dhours(6)
#' sd_f <- lubridate::dhours(12)
#' sd_week <- lubridate::dhours(9.45)
#' alarm_f <- FALSE
#' chronotype(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 05:43:30 # Expected
#'
#' ## Rounding the output at the seconds level
#'
#' msf <- hms::parse_hms("05:40:00")
#' sd_w <- lubridate::dhours(5.43678)
#' sd_f <- lubridate::dhours(9.345111)
#' sd_week <- lubridate::dhours(7.5453)
#' alarm_f <- FALSE
#' x <- msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' x
#' #> 04:46:00.3402 # Expected
#' round_time(x)
#' #> 04:46:00 # Expected
msf_sc <- function(msf, sd_w, sd_f, sd_week, alarm_f) {
    checkmate::assert_class(msf, "hms")
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_duration(sd_week)
    checkmate::assert_logical(alarm_f)
    assert_identical(msf, sd_w, sd_f, sd_week, alarm_f, type = "length")

    ## `sc` exists to remove unnecessary warnings of the lubridate package when
    ## subtracting objects of class `Duration`.

    sc <- sum_time(sd_f, - sd_week, class = "Duration", vectorize = TRUE)
    sc <- sc / 2

    dplyr::case_when(
        alarm_f == TRUE ~ hms::as_hms(NA),
        sd_f <= sd_w ~ msf,
        sd_f > sd_w ~ sum_time(msf, - sc, class = "hms",
                               circular = TRUE, vectorize = TRUE)
    )
}

#' @rdname msf_sc
#' @export
chronotype <- msf_sc
