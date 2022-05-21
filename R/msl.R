#' Compute MCTQ local time of mid-sleep
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `msl()` computes the __local time of mid-sleep__ for standard, micro, and
#' shift versions of the Munich ChronoType Questionnaire (MCTQ).
#'
#' Please note that, although we tried to preserve the original authors' naming
#' pattern for the MCTQ functions, the name `ms` provokes a dangerous name
#' collision with the [`ms()`][lubridate::ms()] function (a function for parsing
#' minutes and seconds components). That's why we named it `msl`. `msl()` and
#' [`sdu()`][mctq::sdu()] are the only exceptions, all the other `mctq`
#' functions maintain a strong naming resemblance with the original authors'
#' naming pattern.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi et al. (2020), Juda,
#' Vetter, & Roenneberg (2013), and The Worldwide Experimental Platform (n.d.)
#' guidelines for `msl()` (\eqn{MSW} or \eqn{MSF}) computation are as follows.
#'
#' ## Notes
#'
#' * This computation must be applied to each section of the questionnaire.
#' * If you are visualizing this documentation in plain text, you may have some
#' trouble understanding the equations. You can see this documentation on the
#' package [website](https://docs.ropensci.org/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{MS_{W/F} = SO_{W/F} + \frac{SD_{W/F}}{2}}__
#'
#' Where:
#'
#' * \eqn{MS_{W/F}} = Local time of mid-sleep on work __or__ work-free days.
#' * \eqn{SO_{W/F}} = Local time of sleep onset on work __or__ work-free
#' days.
#' * \eqn{SD_{W/F}} = Sleep duration on work __or__ work-free days.
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{MS_{W/F}^{M/E/N} = SO_{W/F}^{M/E/N} + \frac{SD_{W/F}^{M/E/N}}{2}}__
#'
#' Where:
#'
#' * \eqn{MS_{W/F}^{M/E/N}}{SO_W/F_M/E/N} = Local time of mid-sleep between
#' two days in a particular shift __or__ between two free days after a
#' particular shift.
#' * \eqn{SO_{W/F}^{M/E/N}}{SO_W/F_M/E/N} = Local time of sleep onset between
#' two days in a particular shift __or__ between two free days after a
#' particular shift.
#' * \eqn{SD_{W/F}^{M/E/N}}{SD_W/F_M/E/N} = Sleep duration between two days in a
#' particular shift __or__ between two free days after a particular shift.
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days, \eqn{M} =
#' Morning shift; \eqn{E} = Evening shift; \eqn{N} = Night shift.
#'
#' @param so An [`hms`][hms::hms()] object corresponding to the __local time of
#'   sleep onset__ from a standard, micro, or shift version of the MCTQ
#'   questionnaire. You can use [`so()`][mctq::so()] to compute it for the
#'   standard or shift version.
#' @param sd A [`Duration`][lubridate::duration()] object corresponding to the
#'   __sleep duration__ from a standard, micro, or shift version of the MCTQ
#'   questionnaire. You can use [`sdu()`][mctq::sdu()] to compute it for any
#'   MCTQ version.
#'
#' @return An [`hms`][hms::hms()] object corresponding to the vectorized sum of
#'   `so` and `(sd / 2)` in a circular time frame of 24 hours.
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
#' msl(so, sd)
#' #> 03:30:00 # Expected
#'
#' so <- hms::parse_hm("01:00")
#' sd <- lubridate::dhours(10)
#' msl(so, sd)
#' #> 06:00:00 # Expected
#'
#' so <- hms::as_hms(NA)
#' sd <- lubridate::dhours(7.5)
#' msl(so, sd)
#' #> NA # Expected
#'
#' ## Vector example
#'
#' so <- c(hms::parse_hm("00:10"), hms::parse_hm("01:15"))
#' sd <- c(lubridate::dhours(9.25), lubridate::dhours(5.45))
#' msl(so, sd)
#' #> [1] 04:47:30 # Expected
#' #> [1] 03:58:30 # Expected
msl <- function(so, sd) {
    assert_hms(so, lower = hms::hms(0))
    assert_duration(sd, lower = lubridate::duration(0))
    assert_identical(so, sd, type = "length")

    vct_sum_time(so, (sd / 2), cycle = lubridate::ddays()) %>%
        as.numeric() %>%
        hms::hms()
}

#' Compute MCTQ sleep-corrected local time of mid-sleep on work-free days
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `msf_sc()` computes the __chronotype or sleep-corrected local time of
#' mid-sleep on work-free days__ for standard, micro, and shift versions of the
#' Munich ChronoType Questionnaire (MCTQ).
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
#' * For MCTQ\eqn{^{Shift}}{ Shift}, the computation below must be applied to
#' each shift section of the questionnaire.
#' * \eqn{MSF_{sc}}{MSF_sc} is a proxy for the subject chronotype in
#' standard and micro versions of the MCTQ.
#' * The basis for estimating chronotype in shift-workers is the mid-sleep on
#' work-free days after evening shifts (\eqn{MSF^E}{MSF_E}). In case work
#' schedules do not comprise evening shifts, Juda, Vetter, & Roenneberg (2013)
#' propose to derive it from the \eqn{MSF_{sc}}{MSF_sc} of other shifts (e.g.,
#' by using a linear model). Unfortunately, the `mctq` package can't help you
#' with that, as it requires a closer look at your data.
#' * \eqn{MSF_{sc}}{MSF_sc} depends on developmental and environmental
#' conditions (e.g., age, light exposure). For epidemiological and genetic
#' studies, \eqn{MSF_{sc}}{MSF_sc} must be normalized for age and sex to make
#' populations of different age and sex compositions comparable (Roenneberg,
#' Allebrandt, Merrow, & Vetter, 2012).
#' * If you are visualizing this documentation in plain text, you may have some
#' trouble understanding the equations. You can see this documentation on the
#' package [website](https://docs.ropensci.org/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{\textrm{If } Alarm_{F} = True \; , \; MSF_{sc} =
#' \textrm{Not Available (NA)}}__
#' __\deqn{\textrm{Else if } SD_F \leq SD_W \; , \; MSF_{sc} = MSF}__
#' __\deqn{\textrm{Else } \; , \; MSF_{sc} = MSF - \frac{SD_F - SD_{week}}{2}}__
#'
#' Where:
#'
#' * \eqn{MSF_{sc}} = Sleep-corrected local time of mid-sleep on work-free days.
#' * \eqn{Alarm_{F}} = A [`logical`][base::logical()] value indicating if the
#' respondent uses an alarm clock to wake up on work-free days.
#' * \eqn{MSF} = Local time of mid-sleep on work-free days.
#' * \eqn{SD_W} = Sleep duration on workdays.
#' * \eqn{SD_F} = Sleep duration on work-free days.
#' * \eqn{SD_{week}} = Average weekly sleep duration.
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days.
#'
#' Note that, since:
#'
#' __\deqn{MSF = SO_{F} + \frac{SD_{F}}{2}}__
#'
#' Where:
#'
#' * \eqn{SO_{F}} = Local time of sleep onset on work-free days.
#' * \eqn{SD_{F}} = Sleep duration on work-free days.
#'
#' The last condition of the \eqn{MSF_{sc}}{MSF_sc} computation can be
#' simplified to:
#'
#' __\deqn{MSF_{sc} = SO_{F} + \frac{SD_{F}}{2} -
#' \frac{SD_{F} - SD_{week}}{2}}__
#' __\deqn{MSF_{sc} = SO_{F} + \frac{SD_{F}}{2} - \frac{SD_{F}}{2} +
#' \frac{SD_{week}}{2}}__
#' __\deqn{MSF_{sc} = SO_{F} + \frac{SD_{week}}{2}}__
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{\textrm{If } Alarm_{F}^{M/E/N} = True \; , \; MSF_{sc}^{M/E/N} =
#' \textrm{Not Available (NA)}}__
#' __\deqn{\textrm{Else if } SD_{F}^{M/E/N} \leq SD_{W}^{M/E/N} \; , \;
#' MSF_{sc}^{M/E/N} = MSF^{M/E/N}}__
#' __\deqn{\textrm{Else } SD_{F}^{M/E/N} > SD_{W}^{M/E/N} \; , \;
#' MSF_{sc}^{M/E/N} = MSF^{M/E/N} -
#' \frac{SD_{F}^{M/E/N} - \emptyset SD^{M/E/N}}{2}}__
#'
#' Where:
#'
#' * \eqn{MSF_{sc}^{M/E/N}} = Sleep-corrected local time of mid-sleep between
#' two free days after a particular shift.
#' * \eqn{Alarm_{F}^{M/E/N}} = A [`logical`][base::logical()] value indicating
#' if the respondent uses an alarm clock to wake up between two free days after
#' a particular shift.
#' * \eqn{MSF^{M/E/N}}{MSF_M/E/N} = Local time of mid-sleep between two free
#' days after a particular shift.
#' * \eqn{SD_{W}^{M/E/N}}{SD_W_M/E/N} = Sleep duration between two days in a
#' particular shift.
#' * \eqn{SD_{F}^{M/E/N}}{SD_F_M/E/N} = Sleep duration between two free days
#' after a particular shift.
#' * \eqn{\emptyset SD^{M/E/N}}{OSD_M/E/N} = Overall sleep duration of a
#' particular shift.
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days, \eqn{M} =
#' Morning shift; \eqn{E} = Evening shift; \eqn{N} = Night shift.
#'
#' Note that, since:
#'
#' __\deqn{MSF^{M/E/N} = SO_{F}^{M/E/N} + \frac{SD_{F}^{M/E/N}}{2}}__
#'
#' Where:
#'
#' * \eqn{SO_{F}^{M/E/N}} = Local time of sleep onset between two free days
#' after a particular shift.
#' * \eqn{SD_{F}^{M/E/N}} = Sleep duration between two free days after a
#' particular shift.
#'
#' The last condition of the \eqn{MSF_{sc}^{M/E/N}}{MSF_sc_M/E/N} computation
#' can be simplified to:
#'
#' __\deqn{MSF_{sc}^{M/E/N} = SO_{F}^{M/E/N} + \frac{SD_{F}^{M/E/N}}{2} -
#' \frac{SD_{F}^{M/E/N} - \emptyset SD^{M/E/N}}{2}}__
#' __\deqn{MSF_{sc}^{M/E/N} = SO_{F}^{M/E/N} + \frac{SD_{F}^{M/E/N}}{2} -
#' \frac{SD_{F}^{M/E/N}}{2} + \frac{\emptyset SD^{M/E/N}}{2}}__
#' __\deqn{MSF_{sc}^{M/E/N} = SO_{F}^{M/E/N} + \frac{\emptyset SD^{M/E/N}}{2}}__
#'
#' @param msf An [`hms`][hms::hms()] object corresponding to the __local time of
#'   mid-sleep on work-free days__ from a standard, micro, or shift version of
#'   the MCTQ questionnaire. You can use [`msl()`][mctq::msl()] to compute it.
#' @param sd_w A [`Duration`][lubridate::duration()] object corresponding to the
#'   __sleep duration on work days__ from a standard, micro, or shift version of
#'   the MCTQ questionnaire. You can use [`sdu()`][mctq::sdu()] to compute it.
#' @param sd_f A [`Duration`][lubridate::duration()] object corresponding to the
#'   __sleep duration on work-free days__ from a standard, micro, or shift
#'   version of the MCTQ questionnaire. You can use [`sdu()`][mctq::sdu()] to
#'   compute it.
#' @param sd_week A [`Duration`][lubridate::duration()] object corresponding to
#'   the __average weekly sleep duration__ from a standard or micro version of
#'   the MCTQ questionnaire (you can use [`sd_week()`][mctq::sd_week()] to
#'   compute it) __or__ the __overall sleep duration of a particular shift__
#'   from a shift version of the MCTQ questionnaire (you can use
#'   [`sd_overall()`][mctq::sd_overall()] to compute it).
#' @param alarm_f A [`logical`][base::logical()] object corresponding to the
#'   __alarm clock use on work-free days__ from a standard, micro, or shift
#'   version of the MCTQ questionnaire. Note that, if `alarm_f == TRUE`,
#'   `msf_sc` cannot be computed, `msf_sc()` will return `NA` for these cases.
#'   For the \eqn{\mu}MCTQ, this value must be set as `FALSE` all times, since
#'   the questionnaire considers only the work-free days when the respondent
#'   does not use an alarm (e.g., `alarm_f = rep(FALSE, length(msf))`).
#'
#' @return An [`hms`][hms::hms()] object corresponding to the MCTQ chronotype or
#'   sleep-corrected local time of mid-sleep on work-free days.
#'
#' @template details_b
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
#' ## chronotype(): A wrapper for msf_sc()
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
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 04:46:00.3402 # Expected
#'
#' round_time(msf_sc(msf, sd_w, sd_f, sd_week, alarm_f))
#' #> 04:46:00 # Expected
msf_sc <- function(msf, sd_w, sd_f, sd_week, alarm_f) {
    assert_hms(msf, lower = hms::hms(0))
    assert_duration(sd_w, lower = lubridate::duration(0))
    assert_duration(sd_f, lower = lubridate::duration(0))
    assert_duration(sd_week, lower = lubridate::duration(0))
    checkmate::assert_logical(alarm_f)
    assert_identical(msf, sd_w, sd_f, sd_week, alarm_f, type = "length")

    ## `sc` exists to remove unnecessary warnings of the {lubridate} package
    ## when subtracting objects of class `Duration`.

    sc <- vct_sum_time(sd_f, - sd_week, cycle = lubridate::ddays())
    sc <- sc / 2

    dplyr::case_when(
        alarm_f == TRUE ~ hms::as_hms(NA),
        sd_f <= sd_w ~ msf,
        sd_f > sd_w ~ hms::hms(as.numeric(
            vct_sum_time(msf, - sc, cycle = lubridate::ddays())))
    )
}

#' @rdname msf_sc
#' @export
chronotype <- msf_sc
