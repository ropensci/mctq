#' Compute MCTQ social jet lag
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sjl()` computes the __relative social jetlag__ or the __absolute social
#' jetlag__ for standard, micro, and shift versions of the Munich Chronotype
#' Questionnaire (MCTQ).
#'
#' `sjl_rel()` it's just a wrapper for `sjl()` with `abs = FALSE`.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `sjl()` (\eqn{SJLrel} and \eqn{SJL}) computation are as
#' follow.
#'
#' For the relative social jetlag:
#'
#' __\deqn{MSF - MSW}__
#'
#' For the absolute social jetlag:
#'
#' __\deqn{| MSF - MSW |}__
#'
#' Where:
#'
#' * \eqn{MSW} = mid-sleep on work days.
#' * \eqn{MSF} = mid-sleep on work-free days.
#'
#' Note that, due to time arithmetic issues, `sjl()` does a slight different
#' computation than those propose by the authors mentioned above. See
#' `vignette("social_jet_lag", package = "mctq")` for more details.
#'
#' @param msw A `hms` object corresponding to the __mid-sleep on work days__
#'   from a standard, micro, or shift version of the MCTQ questionnaire (you can
#'   use [mctq::ms()] to compute it).
#' @param abs (optional) a `logical` value indicating if the function must
#'   return an absolute SJL (always positive) or a relative SJL (the SJL with no
#'   changes) (default: `TRUE`).
#'
#' @return A `Duration` object corresponding to the relative or absolute (if
#'   `abs = TRUE`) value of the shortest interval between `msf` and `msw`. See
#' `vignette("social-jet-lag", package = "mctq")` for more details.
#'
#' @inheritParams msf_sc
#' @template mctq_b
#' @template mctq_d
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sjl(hms::parse_hms("03:30:00"), hms::parse_hms("05:00:00"))
#' #> [1] "5400s (~1.5 hours)" # Expected
#' sjl(hms::parse_hms("04:30:00"), hms::parse_hms("23:30:00"), abs = FALSE)
#' #> [1] "-18000s (~-5 hours)"
#' sjl(hms::parse_hms("01:15:00"), hms::parse_hms("03:45:00"))
#' #> [1] "9000s (~2.5 hours)" # Expected
#' sjl(hms::as_hms(NA), hms::parse_hms("05:15:00"))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' msw <- c(hms::parse_hms("04:05:00"), hms::parse_hms("04:05:00"))
#' msf <- c(hms::parse_hms("03:05:00"), hms::parse_hms("04:05:00"))
#' sjl(msw, msf, abs = FALSE)
#' #> [1] "-3600s (~-1 hours)" "0s" # Expected
#'
#' sjl_rel(hms::parse_hms("05:10:00"), hms::parse_hms("05:05:00"))
#' #> [1] "-300s (~-5 minutes)" # Expected
#'
#' ## __ Converting the output to `hms` __
#' x <- sjl(hms::parse_hms("01:15:00"), hms::parse_hms("03:25:05"))
#' convert_to(x, "hms")
#' #> 02:10:05 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- sjl(hms::parse_hms("04:19:33.1234"), hms::parse_hms("2:55:05"))
#' x
#' #> [1] "5071.12339782715s (~1.41 hours)" # Expected
#' round_time(x)
#' #> [1] "5071s (~1.41 hours)" # Expected
sjl <- function(msw, msf, abs = TRUE) {

    checkmate::assert_class(msw, "hms")
    checkmate::assert_class(msf, "hms")
    checkmate::assert_flag(abs)
    assert_identical(msw, msf, type = "length")

    shortest_interval <- shortest_interval(msw, msf, class = "Interval")
    int_start <- convert_to(lubridate::int_start(shortest_interval), "hms")
    out <- convert_to(shortest_interval, class = "Duration")

    out <- dplyr::case_when(
        msw == msf ~ out,
        int_start == msw ~ out,
        int_start == msf ~ - out
    )

    if (isTRUE(abs)) abs(out) else out

}

#' @rdname sjl
#' @export
sjl_rel <- function(msw, msf) {

    sjl(msw, msf, abs = FALSE)

}

#' Compute MCTQ overall social jetlag (only for MCTQ Shift)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sjl_overall()` computes the __absolute social jetlag across all shifts__ for
#' the shift version of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Operation:
#'
#' The shift version of the MCTQ was developed for shift-workers rotating
#' through morning-, evening-, and night-shifts (transition times at 6:00 a.m.,
#' 2:00 p.m., and 10:00 p.m.), but it also allows adaptations to other shift
#' schedules (Juda, Vetter, & Roenneberg, [2013](https://bit.ly/38IEEk4)). For
#' that reason, `sjl_overall()` must operate considering any shift combination.
#'
#' Considering the requirement above, `sjl_overall()` was developed to only
#' accept lists values as arguments. For this approach to work, both `n` and
#' `sjl` arguments must be lists with paired elements and values between `n` and
#' `sjl`, _i.e._ the first element of `n` (_e.g._ `n_m`) must be paired with
#' the first element of `sjl` (_e.g._ `sjl_m`). The function will do the work
#' of combining them and output a weighted mean.
#'
#' @section Guidelines:
#'
#' For reference, Juda, Vetter, & Roenneberg ([2013](https://bit.ly/38IEEk4))
#' and theWeP [(n.d.)](http://bit.ly/3pv8EH1) guidelines for `sjl_overall()`
#' computation are as follow.
#'
#' __\deqn{((|SJL_M| * nwM) + (|SJL_E| * nwE) + (|SJL_N| * nwN)) /
#' (nwM + nwE + nwN)}__
#'
#' Where:
#'
#' * \eqn{SJL_*} = absolute social jetlag in each shift (MEN`*`).
#' * \eqn{nw*} = number of days worked in each shift (MEN`*`) within a shift
#' cycle.
#'
#' `*` \eqn{M}: morning; \eqn{E}: evening; \eqn{N}: night.
#'
#' Note that the overall social jetlag is the weighted average of
#' shift-specific absolute social jetlags. The authors shows a formula for a
#' three shift schedule, but this may not be your case. That's way this
#' function works a little bit different (see Operation section), allowing to
#' compute a weighted average of any shift combination.
#'
#' @param n A `list` object with [integerish][rlang::is_integerish()] `integer`
#'   or `numeric` elements corresponding to the __number of days from each work
#'   shift__ from a MCTQ shift questionnaire. `n` elements and values must be
#'   paired with `sjl` elements and values.
#' @param sjl A `list` object with `Duration` elements corresponding to the
#'   __absolute social jetlag in each shift__ from a MCTQ shift questionnaire
#'   (you can use [mctq::sjl()] to compute it). `sjl` elements and values must
#'   be paired with `n` elements and values.
#'
#' @return A `Duration` object corresponding to the weighted mean of `sjl` and
#'   `n` (weights).
#'
#' @template mctq_b
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' n <- list(n_m = 3, n_e = 1, n_n = 4)
#' sjl <- list(sjl_m = lubridate::dhours(1.25), sjl_e = lubridate::dhours(0.5),
#'            sjl_n = lubridate::dhours(3))
#' sjl_overall(n, sjl)
#' #> [1] "7312.5s (~2.03 hours)" # Expected
#'
#' ## __ Vectorized example __
#' n <- list(n_m = c(1, 3), n_e = c(4, 1), n_n = c(3, 3))
#' sjl <- list(sjl_m = c(lubridate::dhours(2), lubridate::dhours(2.45)),
#'             sjl_e = c(lubridate::dhours(3.21), lubridate::dhours(0)),
#'             sjl_n = c(lubridate::dhours(1.2), lubridate::dhours(5.32)))
#' sjl_overall(n, sjl)
#' #> [1] "8298s (~2.31 hours)"  "11988s (~3.33 hours)"  # Expected
#'
#' ## __ Checking the second output from vectorized example __
#' i <- 2
#' x <- c(sjl[["sjl_m"]][i], sjl[["sjl_e"]][i], sjl[["sjl_n"]][i])
#' w <- c(n[["n_m"]][i], n[["n_e"]][i], n[["n_n"]][i])
#' lubridate::as.duration(stats::weighted.mean(x, w))
#' #> [1] "11988s (~3.33 hours)" # Expected
#'
#' ## __ Converting the output to hms __
#' n <- list(n_m = 4, n_e = 2, n_n = 1)
#' sjl <- list(sjl_m = lubridate::dhours(0.25), sjl_e = lubridate::dhours(1.2),
#'            sjl_n = lubridate::dhours(4.32))
#' sjl_overall(n, sjl)
#' #> [1] "3970.28571428571s (~1.1 hours)" # Expected
#' convert_to(sjl_overall(n, sjl), "hms")
#' #> 01:06:10.285714 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' round_time(sjl_overall(n, sjl))
#' #> [1] "3970s (~1.1 hours)" # Expected
#' round_time(convert_to(sjl_overall(n, sjl), "hms"))
#' #> 01:06:10 # Expected
sjl_overall <- function(n, sjl) {

    checkmate::assert_list(n, len = length(sjl))
    checkmate::assert_list(sjl, len = length(n))
    lapply(n, checkmate::assert_integerish)
    lapply(sjl, assert_duration)
    lapply(sjl, assert_numeric, lower = 0)
    mapply(assert_identical, n, sjl, MoreArgs = list(type = "length"))

    lapply(sjl, abs)

    foo <- function(x, y) {
        out <- Reduce("*", list(x, y))
        lubridate::as.duration(out)
    }

    sjl <- mapply(foo, n, sjl, SIMPLIFY = FALSE)
    sjl <- Reduce("+", sjl)
    n <- Reduce("+", n)

    sjl / n

}

