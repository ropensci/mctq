#' @details
#'
#' __Standard MCTQ__ functions were created following the guidelines in
#' Roenneberg, Wirz-Justice, & Merrow (2003), Roenneberg, Allebrandt, Merrow, &
#' Vetter (2012), and from The Worldwide Experimental Platform (theWeP, n.d.).
#'
#' __\eqn{\mu}MCTQ__ functions were created following the guidelines in Ghotbi
#' _et.al_ (2020), in addition to the guidelines used for the standard MCTQ.
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
#' hold time values. These classes can be found in the [hms][hms::hms-package]
#' and [lubridate][lubridate::lubridate-package] package. If your data do not
#' conform to the object classes required, you can use the `mctq`
#' [mctq::convert()] function to convert it.
#'
#' ## Rounding and fractional time
#'
#' Some operations may produce an output with fractional time (_e.g._,
#' `"19538.3828571429s (~5.43 hours)"`; `01:15:44.505`). If you want, you
#' can round it with the `mctq` function [mctq::round_time()].
#'
#' Our recommendation is to avoid rounding, but, if you do, make sure that you
#' only round your values after all computations are done, that way you can
#' avoid [round-off errors](https://en.wikipedia.org/wiki/Round-off_error).
