#' Compute MCTQ local time of sleep onset
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `so()` computes the __local time of sleep onset__ for standard and shift
#' versions of the Munich ChronoType Questionnaire (MCTQ).
#'
#' Note that this value is collected directly from the questionnaire if you're
#' using the \eqn{\mu}MCTQ.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Juda, Vetter, & Roenneberg
#' (2013), and The Worldwide Experimental Platform (n.d.) guidelines for `so()`
#' (\eqn{SO}) computation are as follows.
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
#' __\deqn{SO_{W/F} = SPrep_{W/F} + SLat_{W/F}}__
#'
#' Where:
#'
#' * \eqn{SO_{W/F}} = Local time of sleep onset on work __or__ work-free days.
#' * \eqn{SPrep_{W/F}} = Local time of preparing to sleep on work __or__
#' work-free days ("I actually get ready to fall asleep at ___ o'clock").
#' * \eqn{SLat_{W/F}} = Sleep latency or time to fall asleep after preparing to
#' sleep on work __or__ work-free days ("I need ___ min to fall asleep").
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{SO_{W/F}^{M/E/N} = SPrep_{W/F}^{M/E/N} + SLat_{W/F}^{M/E/N}}__
#'
#' Where:
#'
#' * \eqn{SO_{W/F}^{M/E/N}} = Local time of sleep onset between two days in a
#' particular shift __or__ between two free days after a particular shift.
#' * \eqn{SPrep_{W/F}^{M/E/N}} = Local time of preparing to sleep between two
#' days in a particular shift __or__ between two free days after a particular
#' shift ("I actually get ready to fall asleep at ___ o'clock").
#' * \eqn{SLat_{W/F}^{M/E/N}} = Sleep latency or time to fall asleep after
#' preparing to sleep between two days in a particular shift __or__ between two
#' free days after a particular shift ("I need ___ min to fall asleep").
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days, \eqn{M} =
#' Morning shift; \eqn{E} = Evening shift; \eqn{N} = Night shift.
#'
#' @param sprep An [`hms`][hms::hms()] object corresponding to the __local time
#'   of preparing to sleep__ from a standard or shift version of the MCTQ
#'   questionnaire.
#' @param slat A [`Duration`][lubridate::duration()] object corresponding to the
#'   __sleep latency or time to fall asleep after preparing to sleep__ from a
#'   standard or shift version of the MCTQ questionnaire.
#'
#' @return An [`hms`][hms::hms()] object corresponding to the vectorized sum of
#'   `sprep` and `slat` in a circular time frame of 24 hours.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' sprep <- hms::parse_hm("22:00")
#' slat <- lubridate::dminutes(15)
#' so(sprep, slat)
#' #> 22:15:00 # Expected
#'
#' sprep <- hms::parse_hm("23:30")
#' slat <- lubridate::dminutes(45)
#' so(sprep, slat)
#' #> 00:15:00 # Expected
#'
#' sprep <- hms::parse_hm("20:45")
#' slat <- lubridate::as.duration(NA)
#' so(sprep, slat)
#' #> NA # Expected
#'
#' ## Vector example
#'
#' sprep <- c(hms::parse_hm("21:30"), hms::parse_hm("22:15"))
#' slat <- c(lubridate::dminutes(45), lubridate::dminutes(5))
#' so(sprep, slat)
#' #> 22:15:00 # Expected
#' #> 22:20:00 # Expected
so <- function(sprep, slat) {
    assert_hms(sprep, lower = hms::hms(0))
    assert_duration(slat, lower = lubridate::duration(0))
    assert_identical(sprep, slat, type = "length")

    vct_sum_time(sprep, slat, cycle = lubridate::ddays()) %>%
        as.numeric() %>%
        hms::hms()
}
