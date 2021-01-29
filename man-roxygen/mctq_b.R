#' @details
#'
#' __Standard MCTQ__ functions were created following the guidelines in
#' Roenneberg, Wirz-Justice, & Merrow ([2003](https://bit.ly/3rLu195)),
#' Roenneberg, Allebrandt, Merrow, & Vetter ([2012](http://bit.ly/3iGEgqX)),
#' and The World Wide Experimental Platform
#' (theWeP, [n.d.](http://bit.ly/3pv8EH1)).
#'
#' __\eqn{\mu}MCTQ__ functions were created following the guidelines in
#' Ghotbi _et.al_ ([2020](https://bit.ly/34VhA0l)), in addition to the
#' guidelines used for the standard MCTQ.
#'
#' __MCTQ\eqn{^{Shift}}{ Shift}__ functions were created following the
#' guidelines in Juda, Vetter, & Roenneberg ([2013](https://bit.ly/38IEEk4)), in
#' addition to the guidelines used for the standard MCTQ.
#'
#' See References section to learn more.
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. This classes can be found on [hms::hms-package] and
#' [lubridate::lubridate-package]. If your data do not conform to the object
#' classes required, you can use [mctq::convert_to()] to convert it
#' (see `vignette("converting-data", package = "mctq")`).
#'
#' ## Rounding and fractional time
#'
#' Some operations may produce an output with fractional time (_e.g._
#' `"19538.3828571429s (~5.43 hours)"`; `01:15:44.505`). If you want, you
#' can round it with [mctq::round_time()].
#'
#' Our recommendation is to avoid rounding, but, if you do, make sure that you
#' only round your values after all computations are done, that way you can
#' avoid [round-off errors](https://en.wikipedia.org/wiki/Round-off_error).
#'
#' @family MCTQ functions
