#' Compute MCTQ local time of getting out of bed
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `gu()` computes the __local time of getting out of bed__ for standard and
#' shift versions of the Munich ChronoType Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Juda, Vetter, & Roenneberg
#' (2013), and The Worldwide Experimental Platform (n.d.) guidelines for `gu()`
#' (\eqn{GU}) computation are as follows.
#'
#' ## Notes
#'
#' * This computation must be applied to each section of the questionnaire.
#' * MCTQ\eqn{^{Shift}}{ Shift} uses \eqn{TGU} (time to get up) instead of
#' \eqn{SI} (sleep inertia). For the purpose of this computation, both represent
#' the same thing.
#' * If you are visualizing this documentation in plain text, you may have some
#' trouble understanding the equations. You can see this documentation on the
#' package [website](https://docs.ropensci.org/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{GU_{W/F} = SE_{W/F} + SI_{W/F}}__
#'
#' Where:
#'
#' * \eqn{GU_{W/F}} = Local time of getting out of bed on work __or__ work-free
#' days.
#' * \eqn{SE_{W/F}} = Local time of sleep end on work __or__ work-free
#' days.
#' * \eqn{SI_{W/F}} = Sleep inertia on work __or__ work-free days
#' ("after ___ min, I get up").
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{GU_{W/F}^{M/E/N} = SE_{W/F}^{M/E/N} + TGU_{W/F}^{M/E/N}}__
#'
#' Where:
#'
#' * \eqn{GU_{W/F}^{M/E/N}} = Local time of getting out of bed between two days
#' in a particular shift __or__ between two free days after a particular shift.
#' * \eqn{SE_{W/F}^{M/E/N}} = Local time of sleep end between two days in a
#' particular shift __or__ between two free days after a particular shift.
#' * \eqn{TGU_{W/F}^{M/E/N}} = Time to get up after sleep end between two days
#' in a particular shift __or__ between two free days after a particular shift
#' ("after ___ min, I get up").
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days, \eqn{M} =
#' Morning shift; \eqn{E} = Evening shift; \eqn{N} = Night shift.
#'
#' @param se An [`hms`][hms::hms()] object corresponding to the __local time of
#'   sleep end__ from a standard or shift version of the MCTQ questionnaire.
#' @param si A [`Duration`][lubridate::duration()] object corresponding to the
#'   "__sleep inertia__" or __time to get up__ from a standard or shift version
#'   of the MCTQ questionnaire.
#'
#' @return An [`hms`][hms::hms()] object corresponding to the vectorized sum of
#'   `se` and `si` in a circular time frame of 24 hours.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' gu(hms::parse_hm("08:00"), lubridate::dminutes(10))
#' #> 08:10:00 # Expected
#' gu(hms::parse_hm("11:45"), lubridate::dminutes(90))
#' #> 13:15:00 # Expected
#' gu(hms::as_hms(NA), lubridate::dminutes(90))
#' #> NA # Expected
#'
#' ## Vector example
#'
#' se <- c(hms::parse_hm("12:30"), hms::parse_hm("23:45"))
#' si <- c(lubridate::dminutes(10), lubridate::dminutes(70))
#' gu(se, si)
#' #> 12:40:00 # Expected
#' #> 00:55:00 # Expected
gu <- function(se, si) {
    assert_hms(se, lower = hms::hms(0))
    assert_duration(si, lower = lubridate::duration(0))
    assert_identical(se, si, type = "length")

    vct_sum_time(se, si, cycle = lubridate::ddays()) %>%
        as.numeric() %>%
        hms::hms()
}
