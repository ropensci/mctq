#' @details
#'
#' __Standard MCTQ__ functions were created following the guidelines in
#' Roenneberg, Wirz-Justice, & Merrow (2003), Roenneberg, Allebrandt, Merrow, &
#' Vetter (2012), and from The Worldwide Experimental Platform (theWeP, n.d.).
#'
#' __\eqn{\mu}MCTQ__ functions were created following the guidelines in Ghotbi
#' et al. (2020), in addition to the guidelines used for the standard MCTQ.
#'
#' __MCTQ\eqn{^{Shift}}{ Shift}__ functions were created following the
#' guidelines in Juda, Vetter, & Roenneberg (2013), in addition to the
#' guidelines used for the standard MCTQ.
#'
#' See the References section to learn more.
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. These classes can be found in the
#' [lubridate][lubridate::lubridate-package] and [hms][hms::hms-package]
#' packages. Please refer to those package documentations to learn more about
#' them.
#'
#' ## Rounding and fractional time
#'
#' Some operations may produce an output with fractional time (e.g.,
#' `"19538.3828571429s (~5.43 hours)"`, `01:15:44.505`). If you want, you
#' can round it with `mctq:::round_time()`.
#'
#' Our recommendation is to avoid rounding, but, if you do, make sure that you
#' only round your values after all computations are done. That way you avoid
#' [round-off errors](https://en.wikipedia.org/wiki/Round-off_error).
