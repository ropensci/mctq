#' Compute MCTQ local time of getting out of bed
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `gu()` computes the __local time of getting out of bed__ for standard and
#' shift versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Juda, Vetter, & Roenneberg
#' (2013), and The Worldwide Experimental Platform (n.d.) guidelines for `gu()`
#' (\eqn{GU}) computation are as follows.
#'
#' ## Notes
#'
#' * The computation below must be applied to each section of the
#' questionnaire.
#'
#' * MCTQ\eqn{^{Shift}}{ Shift} uses \eqn{TGU} (time to get up) instead of
#' \eqn{SI} (sleep inertia). For the purpose of this computation, both represent
#' the same thing.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{SE_{W/F} + SI_{W/F}}{SE_W/F + SI_W/F}__
#'
#' Where:
#'
#' * \eqn{SE_{W/F}}{SE_W/F} = local time of sleep end on work __or__ work-free
#' days.
#' * \eqn{SI_{W/F}}{SI_W/F} = sleep inertia on work __or__ work-free days
#' ("after ___ min, I get up").
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{SE_{W/F}^{M/E/N} + TGU_{W/F}^{M/E/N}}{SE_W/F_M/E/N + TGU_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{SE_{W/F}^{M/E/N}}{SE_W/F_M/E/N} = local time of sleep end between two
#' days in a particular shift __or__ between two free days after a particular
#' shift.
#' * \eqn{TGU_{W/F}^{M/E/N}}{TGU_W/F_M/E/N} = time to get up after sleep end
#' between two days in a particular shift __or__ between two free days after a
#' particular shift ("after ___ min, I get up").
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param se A `hms` object corresponding to the __local time of sleep end__
#'   from a standard or shift version of the MCTQ questionnaire.
#' @param si A `Duration` object corresponding to the __sleep inertia__ or
#'   __time to get up__ from a standard or shift version of the MCTQ
#'   questionnaire.
#'
#' @return A `hms` object corresponding to the vectorized sum of `se` and `si`
#'   in a circular time frame of 24 hours.
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
    checkmate::assert_class(se, "hms")
    assert_duration(si)
    assert_identical(se, si, type = "length")

    sum_time(se, si, class = "hms", circular = TRUE, vectorize = TRUE)
}
