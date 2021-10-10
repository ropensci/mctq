#' Compute MCTQ social jetlag
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sjl()` computes the __relative or absolute social jetlag__ for standard,
#' micro, and shift versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' `sjl_rel()` it's just a wrapper for `sjl()` with `abs = FALSE`.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Juda, Vetter, & Roenneberg
#' (2013), and The Worldwide Experimental Platform (n.d.) guidelines for `sjl()`
#' (\eqn{SJL_{rel}}{SJL_rel} and \eqn{SJL}) computation are as follows.
#'
#' ## Notes
#'
#' * For MCTQ\eqn{^{Shift}}{ Shift}, the computation below must be applied to
#' each shift section of the questionnaire.
#'
#' * Due to time arithmetic issues, `sjl()` does a slightly different
#' computation by default than those proposed by the authors mentioned above.
#' See `vignette("sjl-computation", package = "mctq")` for more details.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' Relative social jetlag (\eqn{SJL_{rel}}{SJL_rel}):
#'
#' __\deqn{MSF - MSW}__
#'
#' Absolute social jetlag (\eqn{SJL}):
#'
#' __\deqn{| MSF - MSW |}__
#'
#' Where:
#'
#' * \eqn{MSW} = local time of mid-sleep on workdays.
#' * \eqn{MSF} = local time of mid-sleep on work-free days.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' Relative social jetlag (\eqn{SJL_{rel}}{SJL_rel}):
#'
#' __\deqn{MSF^{M/E/N} - MSW^{M/E/N}}{MSF_M/E/N - MSW_M/E/N}__
#'
#' Absolute social jetlag (\eqn{SJL}):
#'
#' __\deqn{| MSF^{M/E/N} - MSW^{M/E/N} |}{| MSF_M/E/N - MSW_M/E/N |}__
#'
#' Where:
#'
#' * \eqn{MSW^{M/E/N}}{MSW_M/E/N} = local time of mid-sleep between two days in
#' a particular shift.
#' * \eqn{MSF^{M/E/N}}{MSF_M/E/N} = local time of mid-sleep between two free
#' days after a particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @section Methods for computing the social jetlag:
#'
#' There are different approaches to compute the social jetlag (\eqn{SJL}). By
#' default, `sjl()` uses an approach that we call "the shorter interval
#' approach" (`"shorter"`).
#'
#' The topics below provide a simple explanation of each method supported by
#' `sjl()`. To get a detail understating of this methods, see
#' `vignette("sjl-computation", package = "mctq")`.
#'
#' Please note that none of the approaches below are related to Jankowski's
#' (2017) social jetlag sleep-corrected proposal. Since Jankowski's alternative
#' is still disputed (Roenneberg, Pilz, Zerbini, & Winnebeck, 2019), the `mctq`
#' package currently doesn't provide a function for it. Future versions of the
#' package may include it.
#'
#' * `"difference"`
#'
#' By using `method = "difference"`, `sjl()` will do the exact computation
#' proposed by the MCTQ authors, i.e., \eqn{SJL} will be computed as the linear
#' difference between \eqn{MSF} and \eqn{MSW} (see the Guidelines section).
#'
#' __We do not recommend using this method__, as it has many limitations.
#'
#' * `"shorter"`
#'
#' This is the default method for `sjl()`. It's based on the shorter
#' interval between \eqn{MSW} and \eqn{MSF}, solving most of the issues
#' relating to \eqn{SJL} computation.
#'
#' * `"longer"`
#'
#' The `"longer"` method uses the same logic of the `"shorter"` method, but,
#' instead of using the shorter interval between \eqn{MSW} and \eqn{MSF}, it
#' uses the longer interval between the two, considering a two-day window.
#'
#' This method may help with special contexts, like when dealing with
#' shift-workers that have a greater than 12 hours distance between their
#' mid-sleep hours.
#'
#' @param msw A `hms` object corresponding to the __local time of mid-sleep on
#'   workdays__ from a standard, micro, or shift version of the MCTQ
#'   questionnaire. You can use [mctq::ms()] to compute it.
#' @param abs (optional) a `logical` object indicating if the function must
#'   return an absolute social jetlag (default: `TRUE`).
#' @param method (optional) a string indicating which method the function must
#'   use to compute the social jetlag. See the Methods section to learn
#'   more (default: `"shorter"`).
#'
#' @return
#'
#' * If `abs = TRUE`, a `Duration` object corresponding to the absolute social
#' jetlag.
#' * If `abs = FALSE`, a `Duration` object corresponding to the relative social
#' jetlag.
#'
#' The output may also vary depending on the `method` used.
#'
#' @inheritParams msf_sc
#' @template details_b
#' @template section_a
#' @template references_c
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' msw <- hms::parse_hm("03:30")
#' msf <- hms::parse_hm("05:00")
#' sjl(msw, msf)
#' #> [1] "5400s (~1.5 hours)" # Expected
#' sjl(msw, msf, abs = FALSE)
#' #> [1] "5400s (~1.5 hours)" # Expected
#' sjl_rel(msw, msf) # Wrapper function
#' #> [1] "5400s (~1.5 hours)" # Expected
#'
#' msw <- hms::parse_hm("04:30")
#' msf <- hms::parse_hm("23:30")
#' sjl(msw, msf)
#' #> [1] "18000s (~5 hours)" # Expected
#' sjl(msw, msf, abs = FALSE)
#' #> [1] "18000s (~-5 hours)" # Expected
#' sjl_rel(msw, msf) # Wrapper function
#' #> [1] "18000s (~-5 hours)" # Expected
#'
#' msw <- hms::as_hms(NA)
#' msf <- hms::parse_hm("05:15")
#' sjl(msw, msf)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' msw <- c(hms::parse_hm("02:05"), hms::parse_hm("04:05"))
#' msf <- c(hms::parse_hm("23:05"), hms::parse_hm("04:05"))
#' sjl(msw, msf)
#' #> [1] "10800s (~3 hours)" "0s" # Expected
#' sjl(msw, msf, abs = FALSE)
#' #> [1] "-10800s (~-3 hours)" "0s" # Expected
#' sjl_rel(msw, msf) # Wrapper function
#' #> [1] "-10800s (~-3 hours)" "0s" # Expected
#'
#' ## Using different methods
#'
#' msw <- hms::parse_hm("19:15")
#' msf <- hms::parse_hm("02:30")
#' sjl(msw, msf, abs = FALSE, method = "difference")
#' #> [1] "-60300s (~-16.75 hours)" # Expected
#' sjl(msw, msf, abs = FALSE, method = "shorter") # default method
#' #> [1] "26100s (~7.25 hours)" # Expected
#' sjl(msw, msf, abs = FALSE, method = "longer")
#' #> [1] "-60300s (~-16.75 hours)" # Expected
#'
#' msw <- hms::parse_hm("02:45")
#' msf <- hms::parse_hm("04:15")
#' sjl(msw, msf, abs = FALSE, method = "difference")
#' #> [1] "5400s (~1.5 hours)" # Expected
#' sjl(msw, msf, abs = FALSE, method = "shorter") # default method
#' #> [1] "5400s (~1.5 hours)" # Expected
#' sjl(msw, msf, abs = FALSE, method = "longer")
#' #> [1] "-81000s (~-22.5 hours)" # Expected
#'
#' ## Converting the output to `hms`
#'
#' msw <- hms::parse_hm("01:15")
#' msf <- hms::parse_hm("03:25")
#' x <- sjl(msw, msf)
#' x
#' #> [1] "7800s (~2.17 hours)" # Expected
#' convert(x, "hms")
#' #> 02:10:00 # Expected
#'
#' ## Rounding the output at the seconds level
#'
#' msw <- hms::parse_hms("04:19:33.1234")
#' msf <- hms::parse_hms("02:55:05")
#' x <- sjl(msw, msf)
#' x
#' #> [1] "5068.12339997292s (~1.41 hours)" # Expected
#' round_time(x)
#' #> [1] "5068s (~1.41 hours)" # Expected
sjl <- function(msw, msf, abs = TRUE, method = "shorter") {
    choices <- c("difference", "shorter", "longer")

    checkmate::assert_class(msw, "hms")
    checkmate::assert_class(msf, "hms")
    checkmate::assert_flag(abs)
    checkmate::assert_choice(method, choices)
    assert_identical(msw, msf, type = "length")

    if (method == "difference") {
        out <- vct_sum_time(msf, - msw) %>%
            lubridate::as.duration()
    } else {
        if (method == "shorter") {
            interval <- shush(shorter_interval(msw, msf))
        } else if (method == "longer") {
            interval <- shush(longer_interval(msw, msf))
        }

        int_start <- hms::as_hms(lubridate::int_start(interval))
        out <- lubridate::as.duration(interval)

        out <- dplyr::case_when(
            msw == msf ~ out,
            int_start == msw ~ out,
            int_start == msf ~ - out
        )
    }

    if (isTRUE(abs)) abs(out) else out
}

#' @rdname sjl
#' @export
sjl_rel <- function(msw, msf, method = "shorter") {
    sjl(msw, msf, abs = FALSE, method = method)
}

#' Compute MCTQ absolute social jetlag across all shifts
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sjl_weighted()` computes the __absolute social jetlag across all shifts__
#' for the shift version of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Operation:
#'
#' The shift version of the MCTQ was developed for shift-workers rotating
#' through morning-, evening-, and night-shifts, but it also allows adaptations
#' to other shift schedules (Juda, Vetter, & Roenneberg, 2013). For this reason,
#' `sjl_weighted()` must operate with any shift combination.
#'
#' Considering the requirement above, `sjl_weighted()` was developed to only
#' accept list objects as arguments. For this approach to work, both `sjl` and
#' `n_w` arguments must be lists with paired elements and values, i.e., the
#' first element of `sjl` (e.g., `sjl_m`) must be paired with the first element
#' of `n_w` (e.g., `n_w_m`). The function will do the work of combining them
#' and output a weighted mean.
#'
#' @section Guidelines:
#'
#' Juda, Vetter, & Roenneberg (2013) and The Worldwide Experimental Platform
#' (n.d.) guidelines for `sjl_weighted()` (\eqn{\emptyset
#' SJL_{weighted}}{OSJL_weighted}) computation are as follows.
#'
#' ## Notes
#'
#' * The absolute social jetlag across all shifts (\eqn{\emptyset
#' SJL_{weighted}}{OSJL_weighted}) is the weighted average of all absolute
#' social jetlags.
#'
#' * The authors describe an equation for a three-shift schedule, but this may
#' not be your case. That's why this function works a little bit differently
#' (see the Operation section), allowing you to compute a weighted average with
#' any shift combination.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{\frac{| SJL^M | \times n_W^M + | SJL^E | \times n_W^E + | SJL^N |
#' \times n_W^N}{n_W^M + n_W^E + n_W^N}}{(| SJL_M | * n_W_M + | SJL_E | *
#' n_W_E + | SJL_N | * n_W_N) / (n_W_M + n_W_E + n_W_N)}__
#'
#' Where:
#'
#' * \eqn{SJL^{M/E/N}}{SJL_M/E/N} = absolute social jetlag in each shift.
#' * \eqn{n_W^{M/E/N}}{n_W_M/E/N} = number of days worked in each shift within a
#' shift cycle.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param sjl A `list` object with `Duration` elements corresponding to the
#'   __absolute social jetlag in each shift__ from a shift version of the MCTQ
#'   questionnaire (you can use [mctq::sjl()] to compute it). `sjl` elements and
#'   values must be paired with `n` elements and values.
#' @param n_w A `list` object with [integerish][checkmate::test_integerish()]
#'   `numeric` elements or `integer` elements corresponding to the __number of
#'   days worked in each shift within a shift cycle__ from a shift version of
#'   the MCTQ questionnaire. `n` elements and values must be paired with `sjl`
#'   elements and values.
#'
#' @return A `Duration` object corresponding to the vectorized weighted mean of
#'   `sjl` with `n_w` as weights.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' sjl <- list(sjl_m = lubridate::dhours(1.25),
#'             sjl_e = lubridate::dhours(0.5),
#'             sjl_n = lubridate::dhours(3))
#' n_w <- list(n_w_m = 3, n_w_e = 1, n_w_n = 4)
#' sjl_weighted(sjl, n_w)
#' #> [1] "7312.5s (~2.03 hours)" # Expected
#'
#' sjl <- list(sjl_m = lubridate::dhours(1.25),
#'             sjl_e = lubridate::as.duration(NA),
#'             sjl_n = lubridate::dhours(3))
#' n_w <- list(n_w_m = 3, n_w_e = 1, n_w_n = 4)
#' sjl_weighted(sjl, n_w)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' sjl <- list(sjl_m = c(lubridate::dhours(2), lubridate::dhours(2.45)),
#'             sjl_e = c(lubridate::dhours(3.21), lubridate::as.duration(NA)),
#'             sjl_n = c(lubridate::dhours(1.2), lubridate::dhours(5.32)))
#' n_w <- list(n_w_m = c(1, 3), n_w_e = c(4, 1), n_w_n = c(3, 3))
#' sjl_weighted(sjl, n_w)
#' #> [1] "8298s (~2.31 hours)" NA # Expected
#'
#' ## Checking the first output from vector example
#'
#' if (requireNamespace("stats", quietly = TRUE)) {
#'     i <- 1
#'     x <- c(sjl[["sjl_m"]][i], sjl[["sjl_e"]][i], sjl[["sjl_n"]][i])
#'     w <- c(n_w[["n_w_m"]][i], n_w[["n_w_e"]][i], n_w[["n_w_n"]][i])
#'     lubridate::as.duration(stats::weighted.mean(x, w))
#' }
#' #> [1] "8298s (~2.31 hours)" # Expected
#'
#' ## Converting the output to hms
#'
#' sjl <- list(sjl_m = lubridate::dhours(0.25),
#'             sjl_e = lubridate::dhours(1.2),
#'             sjl_n = lubridate::dhours(4.32))
#' n_w <- list(n_w_m = 4, n_w_e = 2, n_w_n = 1)
#' sjl_weighted(sjl, n_w)
#' #> [1] "3970.28571428571s (~1.1 hours)" # Expected
#' convert(sjl_weighted(sjl, n_w), "hms")
#' #> 01:06:10.285714 # Expected
#'
#' ## Rounding the output at the seconds level
#'
#' round_time(sjl_weighted(sjl, n_w))
#' #> [1] "3970s (~1.1 hours)" # Expected
#' round_time(convert(sjl_weighted(sjl, n_w), "hms"))
#' #> 01:06:10 # Expected
sjl_weighted <- function(sjl, n_w) {
    checkmate::assert_list(sjl, len = length(n_w))
    checkmate::assert_list(n_w, len = length(sjl))
    lapply(sjl, assert_duration)
    lapply(n_w, assert_whole_number)
    mapply(assert_identical, sjl, n_w, MoreArgs = list(type = "length"))

    sjl <- lapply(sjl, abs)
    n_w <- lapply(n_w, as.integer)

    reduce <- function(x, y) {
        out <- Reduce("*", list(x, y))
        lubridate::as.duration(out)
    }

    sjl <- mapply(reduce, n_w, sjl, SIMPLIFY = FALSE)
    sjl <- Reduce("+", sjl)
    n_w <- Reduce("+", n_w)

    sjl / n_w
}
