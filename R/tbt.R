#' Compute MCTQ total time in bed
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `tbt()` computes the __total time in bed__ for standard and shift versions of
#' the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Juda, Vetter, & Roenneberg
#' (2013), and The Worldwide Experimental Platform (n.d.) guidelines for `tbt()`
#' (\eqn{TBT}) computation are as follows.
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
#' __\deqn{GU_{W/F} - BT_{W/F}}{GU_W/F - BT_W/F}__
#'
#' Where:
#'
#' * \eqn{BT_{W/F}}{BT_W/F} = local time of going to bed on work __or__
#' work-free days ("I go to bed at ___ o'clock").
#' * \eqn{GU_{W/F}}{GU_W/F} = local time of getting out of bed on work __or__
#' work-free days.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{GU_{W/F}^{M/E/N} - BT_{W/F}^{M/E/N}}{GU_W/F_M/E/N - BT_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{BT_{W/F}^{M/E/N}}{BT_W/F_M/E/N} = local time of going to bed between
#' two days in a particular shift __or__ between two free days after a
#' particular shift  ("I go to bed at ___ o'clock").
#' * \eqn{GU_{W/F}^{M/E/N}}{GU_W/F_M/E/N} = local time of getting out of bed
#' between two days in a particular shift __or__ between two free days after a
#' particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#'
#' @param bt A `hms` object corresponding to the __local time of going to bed__
#'   from a standard or shift version of the MCTQ questionnaire.
#' @param gu A `hms` object corresponding to the __local time of getting out of
#'   bed__ from a standard or shift version of the MCTQ questionnaire. You can
#'   use [mctq::gu()] to compute it.
#'
#' @return A `Duration` object corresponding to the vectorized difference
#'   between `gu` and `bt` in a circular time frame of 24 hours.
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
    checkmate::assert_class(bt, "hms")
    checkmate::assert_class(gu, "hms")
    assert_identical(bt, gu, type = "length")

    sum_time(gu, - bt, class = "Duration", circular = TRUE, vectorize = TRUE)
}
