#' Compute MCTQ local time of getting out of bed
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `gu()` computes the __local time of getting out of bed__ for standard and
#' shift versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `gu()` (\eqn{GU}) computation are as follow.
#'
#' __\deqn{SE + SI}__
#'
#' Where:
#'
#' * \eqn{SE} = sleep end.
#' * \eqn{SI} = sleep inertia ("after ... min, I get up").
#'
#' MCTQ Shift uses \eqn{TGU} (time to get up) instead of \eqn{SI}, but both
#' represent the same thing.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SEw + SIw}).
#'
#' @param se A `hms` object corresponding to the __sleep end__ value from a
#'   standard or shift version of the MCTQ questionnaire.
#' @param si A `Duration` object corresponding to the __sleep inertia__ or
#'   __time to get up__ value from a standard or shift version of the MCTQ
#'   questionnaire.
#'
#' @return A `hms` object corresponding to the sum of `se` and `si` rolled on a
#'   24-hour clock basis.
#'
#' @template mctq_b
#' @template mctq_c
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' gu(hms::parse_hms("08:00:00"), lubridate::dminutes(10))
#' #> 08:10:00 # Expected
#' gu(hms::parse_hms("11:45:00"), lubridate::dminutes(90))
#' #> 13:15:00 # Expected
#' gu(hms::as_hms(NA), lubridate::dminutes(90))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' se <- c(hms::parse_hms("12:30:00"), hms::parse_hms("06:40:00"))
#' si <- c(lubridate::dminutes(10), lubridate::dminutes(10))
#' gu(se, si)
#' #> 12:40:00 # Expected
#' #> 06:50:00 # Expected
gu <- function(se, si) {

    checkmate::assert_class(se, "hms")
    assert_duration(si)
    assert_identical(se, si, type = "length")

    sum_time(se, si, class = "hms", clock = TRUE, vectorize = TRUE)

}
