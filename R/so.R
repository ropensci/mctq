#' Compute MCTQ sleep onset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `so()` computes the __time of sleep onset__ for standard and shift
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' Note that this value is collected directly from the questionnaire if you're
#' using the \eqn{\mu}MCTQ.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `so()` (\eqn{SO}) computation are as follow.
#'
#' __\deqn{SPrep + SLat}__
#'
#' Where:
#'
#' * \eqn{SPrep} = local time of preparing to sleep.
#' * \eqn{SLat} = sleep latency ("I need ... min to fall asleep").
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SPrepw + SPrepw}).
#'
#' @param sprep A `hms` object corresponding to the __local time of getting out
#'   of bed__ value from a standard or shift version of the MCTQ questionnaire.
#' @param slat A `Duration` object corresponding to the __sleep latency__ value
#'   from a standard or shift version of the MCTQ questionnaire.
#'
#' @return A `hms` object corresponding to the sum of `sprep` and `slat` rolled
#'   in a 24-hour clock basis.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' so(hms::parse_hms("22:00:00"), lubridate::dminutes(15))
#' #> 22:15:00 # Expected
#' so(hms::parse_hms("23:30:00"), lubridate::dminutes(45))
#' #> 00:15:00 # Expected
#' so(hms::parse_hms("20:45:00"), lubridate::as.duration(NA))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' sprep <- c(hms::parse_hms("21:30:00"), hms::parse_hms("22:15:00"))
#' slat <- c(lubridate::dminutes(45), lubridate::dminutes(5))
#' so(sprep, slat)
#' #> 22:15:00 # Expected
#' #> 22:20:00 # Expected
so <- function(sprep, slat) {

    checkmate::assert_class(sprep, "hms")
    assert_duration(slat)
    assert_identical(sprep, slat, type = "length")

    sum_time(sprep, slat, class = "hms", clock = TRUE, vectorize = TRUE)

}
