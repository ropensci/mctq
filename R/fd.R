#' Compute MCTQ work-free days
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `fd()` computes the __number of work-free days per week__ for standard
#' and micro versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012) and theWeP (n.d.) guidelines
#' for `fd()` (\eqn{FD}) computation are as follow.
#'
#' __\deqn{7 - WD}__
#'
#' Where:
#'
#' * \eqn{WD} = number of workdays ("I have a regular work schedule and work ___
#' days per week").
#'
#' @param wd An [integerish][checkmate::test_integerish()] `integer` or
#'   `numeric` object corresponding to the __number of work days per week__
#'   value from a standard or micro version of the MCTQ questionnaire.
#'
#' @return An `integer` value equivalent to `7 - wd`, _i.e._ the difference
#'   between the number of days in a week and the number of work days.
#'
#' @template details_a
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' fd(5)
#' #> [1] 2 # Expected
#' fd(0:7)
#' #> [1] 7 6 5 4 3 2 1 0 # Expected
#' fd(c(1, NA))
#' #> [1]  6 NA # Expected
fd <- function(wd) {

    assert_numeric_(wd)
    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    checkmate::assert_integerish(wd)

    wd <- as.integer(wd)
    as.integer(7 - wd)

}
