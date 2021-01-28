#' Compute MCTQ mid-sleep
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `ms()` computes the __mid-sleep__ for standard, micro, and shift versions of
#' the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `ms()` (\eqn{MS}) computation are as follow.
#'
#' __\deqn{SO + (SD / 2)}__
#'
#' Where:
#'
#' * \eqn{SO} = sleep onset.
#' * \eqn{SD} = sleep duration.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SOw + (SDw / 2)}).
#'
#' @param so A `hms` object corresponding to the __sleep onset__ value from a
#'   standard, micro, or shift version of the MCTQ questionnaire. You can use
#'   [mctq::so()] to compute it for the standard or shift version.
#' @param sd A `Duration` object corresponding to the __sleep duration__ value
#'   from a standard, micro, or shift version of the MCTQ questionnaire. You can
#'   use [mctq::sd()] to compute it for any MCTQ version.
#'
#' @return A `hms` object corresponding to the sum between `so` and (`sd` / 2)
#'   rolled on a 24-hour clock basis.
#'
#' @aliases msw msf
#' @template mctq_b
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' ms(hms::parse_hms("23:30:00"), lubridate::dhours(8))
#' #> 03:30:00 # Expected
#' ms(hms::parse_hms("01:00:00"), lubridate::dhours(10))
#' #> 06:00:00 # Expected
#' ms(hms::as_hms(NA), lubridate::dhours(7.5))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' so <- c(hms::parse_hms("00:10:00"), hms::parse_hms("01:15:00"))
#' sd <- c(lubridate::dhours(9.25), lubridate::dhours(5.45))
#' ms(so, sd)
#' #> [1] 04:47:30 # Expected
#' #> [1] 03:58:30 # Expected
ms <- function(so, sd) {

    checkmate::assert_class(so, "hms")
    assert_duration(sd)
    assert_identical(so, sd, type = "length")

    sum_time(so, (sd / 2), class = "hms", clock = TRUE, vectorize = TRUE)

}
