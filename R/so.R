#' Compute MCTQ local time of sleep onset
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `so()` computes the __local time of sleep onset__ for standard and shift
#' versions of the Munich Chronotype Questionnaire (MCTQ).
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
#' * The computation below must be applied to each section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{SPrep_{W/F} + SLat_{W/F}}{SPrep_W/F + SLat_W/F}__
#'
#' Where:
#'
#' * \eqn{SPrep_{W/F}}{SPrep_W/F} = local time of preparing to sleep on work
#' __or__ work-free days ("I actually get ready to fall asleep at ___ o'clock").
#' * \eqn{SLat_{W/F}}{SLat_W/F} = sleep latency or time to fall asleep after
#' preparing to sleep on work __or__ work-free days ("I need ___ min to fall
#' asleep").
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{SPrep_{W/F}^{M/E/N} + SLat_{W/F}^{M/E/N}}{SPrep_W/F_M/E/N +
#' SLat_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{SPrep_{W/F}^{M/E/N}}{SPrep_W/F_M/E/N} = local time of preparing to
#' sleep between two days in a particular shift __or__ between two free days
#' after a particular shift ("I actually get ready to fall asleep at ___
#' o'clock").
#' * \eqn{SLat_{W/F}^{M/E/N}}{SLat_W/F_M/E/N} = sleep latency or time to fall
#' asleep after preparing to sleep between two days in a particular shift __or__
#' between two free days after a particular shift ("I need ___ min to fall
#' asleep").
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#'
#' @param sprep A `hms` object corresponding to the __local time of preparing to
#'   sleep__ from a standard or shift version of the MCTQ questionnaire.
#' @param slat A `Duration` object corresponding to the __sleep latency or time
#'   to fall asleep after preparing to sleep__ from a standard or shift version
#'   of the MCTQ questionnaire.
#'
#' @return A `hms` object corresponding to the vectorized sum of `sprep` and
#'   `slat` in a circular time frame of 24 hours.
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
    checkmate::assert_class(sprep, "hms")
    assert_duration(slat)
    assert_identical(sprep, slat, type = "length")

    sum_time(sprep, slat, class = "hms", circular = TRUE, vectorize = TRUE)
}
