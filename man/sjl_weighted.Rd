% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sjl.R
\name{sjl_weighted}
\alias{sjl_weighted}
\title{Compute MCTQ absolute social jetlag across all shifts}
\usage{
sjl_weighted(sjl, n_w)
}
\arguments{
\item{sjl}{A \code{\link[base:list]{list}} object with
\code{\link[lubridate:duration]{Duration}} elements corresponding to the \strong{social
jetlag in each shift} from a shift version of the MCTQ questionnaire (you
can use \code{\link[=sjl]{sjl()}} to compute it). \code{sjl} elements and values must be
paired with \code{n} elements and values.}

\item{n_w}{A \code{\link[base:list]{list}} object with
\link[checkmate:checkIntegerish]{integerish} \code{\link[base:integer]{integer}} or
\code{\link[base:double]{double}} elements corresponding to the \strong{number of days
worked in each shift within a shift cycle} from a shift version of the
MCTQ questionnaire. \code{n} elements and values must be paired with \code{sjl}
elements and values.}
}
\value{
A \code{\link[lubridate:duration]{Duration}} object corresponding to the
vectorized weighted mean of \code{sjl} with \code{n_w} as weights.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}

\code{sjl_weighted()} computes the \strong{absolute social jetlag across all shifts}
for the shift version of the Munich ChronoType Questionnaire (MCTQ).
}
\details{
\strong{Standard MCTQ} functions were created following the guidelines in
Roenneberg, Wirz-Justice, & Merrow (2003), Roenneberg, Allebrandt, Merrow, &
Vetter (2012), and from The Worldwide Experimental Platform (theWeP, n.d.).

\strong{\eqn{\mu}MCTQ} functions were created following the guidelines in Ghotbi
et al. (2020), in addition to the guidelines used for the standard MCTQ.

\strong{MCTQ\eqn{^{Shift}}{ Shift}} functions were created following the
guidelines in Juda, Vetter, & Roenneberg (2013), in addition to the
guidelines used for the standard MCTQ.

See the References section to learn more.
\subsection{Class requirements}{

The \code{mctq} package works with a set of object classes specially created to
hold time values. These classes can be found in the
\link[lubridate:lubridate-package]{lubridate} and \link[hms:hms-package]{hms}
packages. Please refer to those package documentations to learn more about
them.
}

\subsection{Rounding and fractional time}{

Some operations may produce an output with fractional time (e.g.,
\code{"19538.3828571429s (~5.43 hours)"}, \code{01:15:44.505}). If you want, you
can round it with \code{mctq:::round_time()}.

Our recommendation is to avoid rounding, but, if you do, make sure that you
only round your values after all computations are done. That way you avoid
\href{https://en.wikipedia.org/wiki/Round-off_error}{round-off errors}.
}
}
\section{Operation}{


The shift version of the MCTQ was developed for shift-workers rotating
through morning-, evening-, and night-shifts, but it also allows adaptations
to other shift schedules (Juda, Vetter, & Roenneberg, 2013). For this reason,
\code{sjl_weighted()} must operate with any shift combination.

Considering the requirement above, \code{sjl_weighted()} was developed to only
accept \code{\link[base:list]{list}} objects as arguments. For this approach to
work, both \code{sjl} and \code{n_w} arguments must be lists with paired elements and
values, i.e., the first element of \code{sjl} (e.g., \code{sjl_m}) must be paired with
the first element of \code{n_w} (e.g., \code{n_w_m}). The function will do the work of
combining them and output a weighted mean.
}

\section{Guidelines}{


Juda, Vetter, & Roenneberg (2013) and The Worldwide Experimental Platform
(n.d.) guidelines for \code{sjl_weighted()} (\eqn{\emptyset
SJL_{weighted}}{OSJL_weighted}) computation are as follows.
\subsection{Notes}{
\itemize{
\item The absolute social jetlag across all shifts (\eqn{\emptyset
SJL_{weighted}}{OSJL_weighted}) is the weighted average of all absolute
social jetlags.
\item The authors describe an equation for a three-shift schedule, but this may
not be your case. That's why this function works a little bit differently
(see the Operation section), allowing you to compute a weighted average with
any shift combination.
\item If you are visualizing this documentation in plain text, you may have some
trouble understanding the equations. You can see this documentation on the
package \href{https://docs.ropensci.org/mctq/reference/}{website}.
}
}

\subsection{Computation}{

\deqn{\emptyset SJL_{weighted} = \frac{(| SJL^{M} | \times n_{W}^{M}) +
(| SJL^{E} | \times n_{W}^{E}) + (| SJL^{N} | \times n_{W}^{N})}{n_{W}^{M} +
n_{W}^{E} + n_{W}^{N}}}

Where:
\itemize{
\item \eqn{\emptyset SJL_{weighted}} = Absolute social jetlag across all shifts.
\item \eqn{SJL^{M/E/N}} = Absolute social jetlag in each shift.
\item \eqn{n_{W}^{M/E/N}} = Number of days worked in each shift within a shift
cycle.
}

\strong{*} \eqn{W} = Workdays; \eqn{F} = Work-free days, \eqn{M} =
Morning shift; \eqn{E} = Evening shift; \eqn{N} = Night shift.
}
}

\examples{
## Scalar example

sjl <- list(sjl_m = lubridate::dhours(1.25),
            sjl_e = lubridate::dhours(0.5),
            sjl_n = lubridate::dhours(3))
n_w <- list(n_w_m = 3, n_w_e = 1, n_w_n = 4)
sjl_weighted(sjl, n_w)
#> [1] "7312.5s (~2.03 hours)" # Expected

sjl <- list(sjl_m = lubridate::dhours(1.25),
            sjl_e = lubridate::as.duration(NA),
            sjl_n = lubridate::dhours(3))
n_w <- list(n_w_m = 3, n_w_e = 1, n_w_n = 4)
sjl_weighted(sjl, n_w)
#> [1] NA # Expected

## Vector example

sjl <- list(sjl_m = c(lubridate::dhours(2), lubridate::dhours(2.45)),
            sjl_e = c(lubridate::dhours(3.21), lubridate::as.duration(NA)),
            sjl_n = c(lubridate::dhours(1.2), lubridate::dhours(5.32)))
n_w <- list(n_w_m = c(1, 3), n_w_e = c(4, 1), n_w_n = c(3, 3))
sjl_weighted(sjl, n_w)
#> [1] "8298s (~2.31 hours)" NA # Expected

## Checking the first output from vector example

if (requireNamespace("stats", quietly = TRUE)) {
    i <- 1
    x <- c(sjl[["sjl_m"]][i], sjl[["sjl_e"]][i], sjl[["sjl_n"]][i])
    w <- c(n_w[["n_w_m"]][i], n_w[["n_w_e"]][i], n_w[["n_w_n"]][i])
    lubridate::as.duration(stats::weighted.mean(x, w))
}
#> [1] "8298s (~2.31 hours)" # Expected

## Converting the output to hms

sjl <- list(sjl_m = lubridate::dhours(0.25),
            sjl_e = lubridate::dhours(1.2),
            sjl_n = lubridate::dhours(4.32))
n_w <- list(n_w_m = 4, n_w_e = 2, n_w_n = 1)

sjl_weighted(sjl, n_w)
#> [1] "3970.28571428571s (~1.1 hours)" # Expected

hms::as_hms(as.numeric(sjl_weighted(sjl, n_w)))
#> 01:06:10.285714 # Expected

## Rounding the output at the seconds level

mctq:::round_time(sjl_weighted(sjl, n_w))
#> [1] "3970s (~1.1 hours)" # Expected

mctq:::round_time(hms::as_hms(as.numeric(sjl_weighted(sjl, n_w))))
#> 01:06:10 # Expected
}
\references{
Ghotbi, N., Pilz, L. K., Winnebeck, E. C., Vetter, C., Zerbini, G., Lenssen,
D., Frighetto, G., Salamanca, M., Costa, R., Montagnese, S., & Roenneberg, T.
(2020). The \eqn{\mu}MCTQ: an ultra-short version of the Munich ChronoType
Questionnaire. \emph{Journal of Biological Rhythms}, \emph{35}(1), 98-110.
\doi{10.1177/0748730419886986}

Juda, M., Vetter, C., & Roenneberg, T. (2013). The Munich ChronoType
Questionnaire for shift-workers (MCTQ\eqn{^{Shift}}{ Shift}). \emph{Journal of
Biological Rhythms}, \emph{28}(2), 130-140. \doi{10.1177/0748730412475041}

Roenneberg T., Allebrandt K. V., Merrow M., & Vetter C. (2012). Social jetlag
and obesity. \emph{Current Biology}, \emph{22}(10), 939-43.
\doi{10.1016/j.cub.2012.03.038}

Roenneberg, T., Wirz-Justice, A., & Merrow, M. (2003). Life between clocks:
daily temporal patterns of human chronotypes. \emph{Journal of Biological
Rhythms}, \emph{18}(1), 80-90. \doi{10.1177/0748730402239679}

The Worldwide Experimental Platform (n.d.). MCTQ.
\url{https://www.thewep.org/documentations/mctq/}
}
\seealso{
Other MCTQ functions: 
\code{\link{fd}()},
\code{\link{gu}()},
\code{\link{le_week}()},
\code{\link{msf_sc}()},
\code{\link{msl}()},
\code{\link{napd}()},
\code{\link{sd24}()},
\code{\link{sd_overall}()},
\code{\link{sd_week}()},
\code{\link{sdu}()},
\code{\link{sjl}()},
\code{\link{sjl_sc}()},
\code{\link{so}()},
\code{\link{tbt}()}
}
\concept{MCTQ functions}
