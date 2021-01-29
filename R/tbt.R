#' Compute MCTQ total time in bed
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tbt()` computes the __total time in bed__ for standard and shift versions of
#' the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `tbt()` (\eqn{TBT}) computation are as follow.
#'
#' __\deqn{GU - BT}__
#'
#' Where:
#'
#' * \eqn{BT} = Local time of going to bed ("I go to bed at ... o'clock").
#' * \eqn{GU} = local time of getting out of bed.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{GUw - BTw}).
#'
#' @param bt A `hms` object corresponding to the __local time of going to bed__
#'   value from a standard or shift version of the MCTQ questionnaire.
#' @param gu A `hms` object corresponding to the __local time of getting out of
#'   bed__ value from a standard or shift version of the MCTQ questionnaire. You
#'   can use [mctq::gu()] to compute it.
#'
#' @return A `Duration` object corresponding to the difference between `gu` and
#'   `bt` rolled on a 24-hour clock basis.
#'
#' @template mctq_b
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' tbt(hms::parse_hms("22:10:00"), hms::parse_hms("06:15:00"))
#' #> [1] "29100s (~8.08 hours)" # Expected
#' tbt(hms::parse_hms("01:20:00"), hms::parse_hms("14:00:00"))
#' #> [1] "45600s (~12.67 hours)" # Expected
#' tbt(hms::as_hms(NA), hms::parse_hms("07:20:00"))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' bt <- c(hms::parse_hms("23:50:00"), hms::parse_hms("02:30:00"))
#' gu <- c(hms::parse_hms("09:30:00"), hms::parse_hms("11:25:00"))
#' tbt(bt, gu)
#' #> [1] "34800s (~9.67 hours)" "32100s (~8.92 hours)" # Expected
tbt <- function(bt, gu) {

    checkmate::assert_class(bt, "hms")
    checkmate::assert_class(gu, "hms")
    assert_identical(bt, gu, type = "length")

    sum_time(gu, - bt, class = "Duration", clock = TRUE, vectorize = TRUE)

}
