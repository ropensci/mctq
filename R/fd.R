#' Compute MCTQ work-free days
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `fd()` computes the __number of work-free days per week__ for standard
#' and micro versions of the Munich ChronoType Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012) and The Worldwide
#' Experimental Platform (n.d.) guidelines for `fd()` (\eqn{FD}) computation are
#' as follows.
#'
#' \deqn{FD = 7 - WD}
#'
#' Where:
#'
#' * \eqn{FD} = Number of work-free days per week.
#' * \eqn{WD} = Number of workdays per week ("I have a regular work schedule and
#' work ___ days per week").
#'
#' @param wd An [integerish][checkmate::test_integerish()]
#'   [`numeric`][base::numeric()] object or an [`integer`][base::integer()]
#'   object corresponding to the __number of workdays per week__ from a standard
#'   or micro version of the MCTQ questionnaire.
#'
#' @return An [`integer`][base::integer()] object corresponding to the
#'   difference between the number of days in a week (7) and the number of
#'   workdays (`wd`).
#'
#' @template details_a
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' fd(5)
#' #> [1] 2 # Expected
#' fd(4)
#' #> [1] 3 # Expected
#' fd(as.numeric(NA))
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' fd(0:7)
#' #> [1] 7 6 5 4 3 2 1 0 # Expected
#' fd(c(1, NA))
#' #> [1]  6 NA # Expected
fd <- function(wd) {
  assert_numeric_(wd)
  checkmate::assert_integerish(wd, lower = 0, upper = 7)

  wd <- as.integer(wd)
  as.integer(7 - wd)
}
