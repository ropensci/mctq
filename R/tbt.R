#' Compute MCTQ total time in bed
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `tbt()` computes the __total time in bed__ for standard and shift versions of
#' the Munich ChronoType Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Juda, Vetter, & Roenneberg
#' (2013), and The Worldwide Experimental Platform (n.d.) guidelines for `tbt()`
#' (\eqn{TBT}) computation are as follows.
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
#' __\deqn{GU_{W/F} - BT_{W/F}}{GU_W/F - BT_W/F}__
#'
#' Where:
#'
#' * \eqn{BT_{W/F}}{BT_W/F} = Local time of going to bed on work __or__
#' work-free days ("I go to bed at ___ o'clock").
#' * \eqn{GU_{W/F}}{GU_W/F} = Local time of getting out of bed on work __or__
#' work-free days.
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{GU_{W/F}^{M/E/N} - BT_{W/F}^{M/E/N}}{GU_W/F_M/E/N - BT_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{BT_{W/F}^{M/E/N}}{BT_W/F_M/E/N} = Local time of going to bed between
#' two days in a particular shift __or__ between two free days after a
#' particular shift  ("I go to bed at ___ o'clock").
#' * \eqn{GU_{W/F}^{M/E/N}}{GU_W/F_M/E/N} = Local time of getting out of bed
#' between two days in a particular shift __or__ between two free days after a
#' particular shift.
#'
#' \strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days, \eqn{M} =
#' Morning shift; \eqn{E} = Evening shift; \eqn{N} = Night shift.
#'
#'
#' @param bt An [`hms`][hms::hms()] object corresponding to the __local time of
#'   going to bed__ from a standard or shift version of the MCTQ questionnaire.
#' @param gu An [`hms`][hms::hms()] object corresponding to the __local time of
#'   getting out of bed__ from a standard or shift version of the MCTQ
#'   questionnaire. You can use [`gu()`][mctq::gu()] to compute it.
#'
#' @return A [`Duration`][lubridate::duration()] object corresponding to the
#'   vectorized difference between `gu` and `bt` in a circular time frame of 24
#'   hours.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' bt <- hms::parse_hm("22:10")
#' gu <- hms::parse_hm("06:15")
#' tbt(bt, gu)
#' #> [1] "29100s (~8.08 hours)" # Expected
#'
#' bt <- hms::parse_hm("01:20")
#' gu <- hms::parse_hm("14:00")
#' tbt(bt, gu)
#' #> [1] "45600s (~12.67 hours)" # Expected
#'
#' bt <- hms::as_hms(NA)
#' gu <- hms::parse_hm("07:20")
#' tbt(bt, gu)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' bt <- c(hms::parse_hm("23:50"), hms::parse_hm("02:30"))
#' gu <- c(hms::parse_hm("09:30"), hms::parse_hm("11:25"))
#' tbt(bt, gu)
#' #> [1] "34800s (~9.67 hours)" "32100s (~8.92 hours)" # Expected
tbt <- function(bt, gu) {
    assert_hms(bt, lower = hms::hms(0))
    assert_hms(gu, lower = hms::hms(0))
    assert_identical(bt, gu, type = "length")

    vct_sum_time(gu, - bt, cycle = lubridate::ddays())
}
