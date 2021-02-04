#' Compute MCTQ sleep duration
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sd()` computes the __sleep duration__ for standard, micro, and shift
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi
#' _et.al_ (2020), Juda, Vetter, & Roenneberg (2013), and theWeP (n.d.)
#' guidelines for `sd()` (\eqn{SD}) computation are as follow.
#'
#' ## Notes
#'
#' * The computation below must be applied to each section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{SE_{W/F} - SO_{W/F}}{SE_W/F - SO_W/F}__
#'
#' Where:
#'
#' * \eqn{SE_{W/F}}{SE_W/F} = sleep end on work or work-free days.
#' * \eqn{SO_{W/F}}{SO_W/F}  = sleep onset on work or work-free days.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{SE_{W/F}^{M/E/N} - SO_{W/F}^{M/E/N}}{SE_W/F_M/E/N - SO_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{SE_{W/F}^{M/E/N}}{SE_W/F_M/E/N} = sleep end between two days in a
#' particular shift __or__ between two free days after a particular shift.
#' * \eqn{SO_{W/F}^{M/E/N}}{SO_W/F_M/E/N}  = sleep onset between two days in a
#' particular shift __or__ between two free days after a particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param so A `hms` object corresponding to the __sleep onset__ value
#'   from a standard, micro, or shift version of the MCTQ questionnaire. You can
#'   use [mctq::so()] to compute it for the standard or shift version.
#' @param se A `hms` object corresponding to the __sleep end__ value
#'   from a standard, micro, or shift version of the MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the difference between
#'   `se` and `so` rolled in a 24-hour clock basis.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sd(hms::parse_hms("23:00:00"), hms::parse_hms("08:00:00"))
#' #> [1] "32400s (~9 hours)" # Expected
#' sd(hms::parse_hms("02:00:00"), hms::parse_hms("12:30:00"))
#' #> [1] "37800s (~10.5 hours)" # Expected
#' sd(hms::parse_hms("03:15:00"), hms::as_hms(NA))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' so <- c(hms::parse_hms("04:12:00"), hms::parse_hms("21:20:00"))
#' se <- c(hms::parse_hms("14:30:00"), hms::parse_hms("03:45:00"))
#' sd(so, se)
#' #> [1] "37080s (~10.3 hours)" "23100s (~6.42 hours)" # Expected
sd <- function(so, se) {

    checkmate::assert_class(so, "hms")
    checkmate::assert_class(se, "hms")
    assert_identical(so, se, type = "length")

    sum_time(se, - so, class = "Duration", clock = TRUE, vectorize = TRUE)

}

#' Compute MCTQ nap duration (only for MCTQ Shift)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `napd()` computes the __nap duration__ for the shift version of the Munich
#' Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Juda, Vetter & Roenneberg (2013), and theWeP (n.d.) guidelines for `napd()`
#' (\eqn{NapD}) computation are as follow.
#'
#' ## Notes
#'
#' * The computation below must be applied to each shift section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{NapE_{W/F}^{M/E/N} - NapO_{W/F}^{M/E/N}}{
#' NapE_W/F_M/E/N - NapO_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{NapO_{W/F}^{M/E/N}}{NapO_W/F_M/E/N} = local time of nap onset between
#' two days in a particular shift __or__ between two free days after a
#' particular shift ("I take a nap from ___ o'clock \[...\]").
#' * \eqn{NapE_{W/F}^{M/E/N}}{NapE_W/F_M/E/N} = local time of nap end between
#' two days in a particular shift __or__ between two free days after a
#' particular shift ("\[...\] to ___ o'clock").
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param napo A `hms` object corresponding to the __nap onset__ value
#'   from the shift version of the MCTQ questionnaire.
#' @param nape A `hms` object corresponding to the __nap end__ value
#'   from the shift version of the MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the difference between
#'   `nape` and `napo` rolled in a 24-hour clock basis.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' napd(hms::parse_hms("12:30:00"), hms::parse_hms("14:20:00"))
#' #> [1] "6600s (~1.83 hours)"" # Expected
#' napd(hms::parse_hms("23:45:00"), hms::parse_hms("00:30:00"))
#' #> [1] "2700s (~45 minutes)" # Expected
#' napd(hms::parse_hms("10:20:00"), hms::as_hms(NA))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' napo <- c(hms::parse_hms("01:25:00"), hms::parse_hms("23:50:00"))
#' nape <- c(hms::parse_hms("03:10:00"), hms::parse_hms("01:10:00"))
#' napd(napo, nape)
#' #> [1] "6300s (~1.75 hours)" "4800s (~1.33 hours)"  # Expected
napd <- function(napo, nape) {

    checkmate::assert_class(napo, "hms")
    checkmate::assert_class(nape, "hms")
    assert_identical(napo, nape, type = "length")

    sum_time(nape, - napo, class = "Duration", clock = TRUE, vectorize = TRUE)

}

#' Compute MCTQ 24h sleep duration (only for MCTQ Shift)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sd24()` computes the __24h sleep duration__ for the shift version of the
#' Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Juda, Vetter & Roenneberg (2013), and theWeP (n.d.) guidelines for `sd24()`
#' (\eqn{SD24}) computation are as follow.
#'
#' ## Notes
#'
#' * The computation below must be applied to each shift section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{SD_{W/F}^{M/E/N} + NapD_{W/F}^{M/E/N}}{
#' SD_W/F_M/E/N + NapD_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{SD_{W/F}^{M/E/N}}{SD_W/F_M/E/N} = sleep duration between two days in a
#' particular shift __or__ between two free days after a particular shift.
#' * \eqn{NapD_{W/F}^{M/E/N}}{NapD_W/F_M/E/N} = nap duration between two days in
#' a particular shift __or__ between two free days after a particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param sd A `Duration` object corresponding to the __sleep duration__ value
#'   from the shift version of the MCTQ questionnaire. You can use [mctq::sd()]
#'   to compute it.
#' @param napd A `Duration` object corresponding to the __nap duration__ value
#'   from the shift version of the MCTQ questionnaire. You can use
#'   [mctq::napd()] to compute it.
#'
#' @return A `Duration` object corresponding to the sum of `sd` and `napd`.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sd24(lubridate::dhours(6), lubridate::dhours(0.5))
#' #> [1] "23400s (~6.5 hours)" # Expected
#' sd24(lubridate::dhours(9), lubridate::dhours(1.5))
#' #> [1] "37800s (~10.5 hours)" # Expected
#' sd24(lubridate::as.duration(NA), lubridate::dhours(2.3))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' sd <- c(lubridate::dhours(7.5), lubridate::dhours(8))
#' napd <- c(lubridate::dhours(0.75), lubridate::dhours(1))
#' sd24(sd, napd)
#' #> [1] "29700s (~8.25 hours)" "32400s (~9 hours)"   # Expected
sd24 <- function(sd, napd) {

    checkmate::assert_class(sd, "Duration")
    checkmate::assert_class(napd, "Duration")
    assert_identical(sd, napd, type = "length")

    sum_time(sd, napd, class = "Duration", clock = FALSE, vectorize = TRUE)

}

#' Compute MCTQ average weekly sleep duration
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sd_week()` computes the __average weekly sleep duration__ for the standard
#' and micro versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' See [mctq::sd_overall()] to compute the overall sleep duration of a
#' particular shift for the shift version of the MCTQ.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi _et.al_ (2020), and
#' theWeP (n.d.) guidelines for `sd_week()` (\eqn{SD_{week}}{SD_week})
#' computation are as follow.
#'
#' ## Notes
#'
#' * The average weekly sleep duration is the weighted average of the sleep
#' durations on work and work-free days in a week.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{\frac{SD_w \times WD + SD_f \times FD}{7}}{
#' (SD_W * WD + SD_F * FD) / 7}__
#'
#' Where:
#'
#' * \eqn{SD_w} = sleep duration on workdays.
#' * \eqn{SD_f} = sleep duration on work-free days.
#' * \eqn{WD} = number of workdays per week ("I have a regular work schedule and
#' work ___ days per week").
#' * \eqn{FD} = number of work-free days per week.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' @param sd_w A `Duration` object corresponding to the __sleep duration on work
#'   days__ value from a standard or micro version of the MCTQ questionnaire.
#'   You can use [mctq::sd()] to compute it.
#' @param sd_f A `Duration` object corresponding to the __sleep duration on
#'   work-free days__ value from a standard or micro version of the MCTQ
#'   questionnaire. You can use [mctq::sd()] to compute it.
#'
#' @return A `Duration` object corresponding to the weighted mean of `sd_w` and
#'   `sd_f` with `wd` and `fd(wd)` as weights.
#'
#' @inheritParams fd
#' @template details_b
#' @template section_a
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sd_week(lubridate::dhours(4), lubridate::dhours(8), 5)
#' #> [1] "18514.2857142857s (~5.14 hours)" # Expected
#' sd_week(lubridate::dhours(7), lubridate::dhours(7), 4)
#' #> [1] "25200s (~7 hours)" # Expected
#' sd_week(lubridate::as.duration(NA), lubridate::dhours(10), 6)
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' sd_w <- c(lubridate::dhours(4.5), lubridate::dhours(5.45))
#' sd_f <- c(lubridate::dhours(8), lubridate::dhours(7.3))
#' wd <- c(3, 7)
#' sd_week(sd_w, sd_f, wd)
#' #> [1] "23400s (~6.5 hours)"  "19620s (~5.45 hours)" # Expected
#'
#' ## __ Checking second output from vectorized example __
#' i <- 2
#' x <- c(sd_w[i], sd_f[i])
#' w <- c(wd[i], fd(wd[i]))
#' lubridate::as.duration(stats::weighted.mean(x, w))
#' #> [1] "19620s (~5.45 hours)" # Expected
#'
#' ## __ Converting the output to hms __
#' x <- sd_week(lubridate::dhours(5.45), lubridate::dhours(9.5), 5)
#' convert(x, "hms")
#' #> 06:36:25.714286 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- sd_week(lubridate::dhours(4.5), lubridate::dhours(7.8), 3)
#' x
#' #> [1] "22988.5714285714s (~6.39 hours)" # Expected
#' round_time(x)
#' #> [1] "22989s (~6.39 hours)" # Expected
sd_week <- function(sd_w, sd_f, wd) {

    assert_duration(sd_w)
    assert_duration(sd_f)
    checkmate::assert_integerish(wd)
    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_identical(wd, sd_w, sd_f, type = "length")

    wd <- as.integer(wd)

    ((sd_w * wd) + (sd_f * fd(wd))) / 7

}

#' Compute MCTQ overall sleep duration (only for MCTQ Shift)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sd_overall()` computes the __overall sleep duration in a particular shift__
#' for the shift version of the Munich Chronotype Questionnaire (MCTQ).
#'
#' See [mctq::sd_week()] to compute the average weekly sleep duration for the
#' standard and micro versions of the MCTQ.
#'
#' @section Guidelines:
#'
#' Juda, Vetter, & Roenneberg (2013) and theWeP (n.d.) guidelines for
#' `sd_overall()` (\eqn{\emptyset SD^{M/E/N}}{OSD_M/E/N}) computation are as
#' follow.
#'
#' ## Notes
#'
#' * The computation below must be applied to each shift section of the
#' questionnaire. If you are using the three shift design propose by the authors,
#' you need to compute three overall sleep duration (_e.g._
#' \eqn{\emptyset SD^M}{OSD_M}; \eqn{\emptyset SD^E}{OSD_E};
#' \eqn{\emptyset SD^N}{OSD_N}).
#'
#' * The overall sleep duration is the weighted average of the shift-specific
#' mean sleep durations.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{\frac{SD_W^{M/E/N} \times n_W^{M/E/N} + SD_F^{M/E/N} \times
#' n_F^{M/E/N}}{n_W^{M/E/N} + n_F^{M/E/N}}}{(SD_W_M/E/N * n_W_M/E/N +
#' SD_F_M/E/N * n_F_M/E/N) / (n_W_M/E/N + n_F_M/E/N)}__
#'
#' Where:
#'
#' * \eqn{SD_W^{M/E/N}}{SD_W_M/E/N} = sleep duration between two days in a
#' particular shift.
#' * \eqn{SD_F^{M/E/N}}{SD_F_M/E/N} = sleep duration between two free days after
#' a particular shift.
#' * \eqn{n_W^{M/E/N}}{n_W_M/E/N} = number of days worked in a particular shift
#' within a shift cycle.
#' * \eqn{n_F^{M/E/N}}{n_F_M/E/N} = number of free days after a particular shift
#' within a shift cycle.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param sd_w A `Duration` object corresponding to the __sleep duration between
#'   two days in a particular shift__ value from a shift version of the MCTQ
#'   questionnaire. You can use [mctq::sd()] to compute it.
#' @param sd_f A `Duration` object corresponding to the __sleep duration between
#'   two free days after a particular shift__ value from a shift version of the
#'   MCTQ questionnaire. You can use [mctq::sd()] to compute it.
#' @param n_w An [integerish][checkmate::test_integerish()] `integer` or
#'   `numeric` object corresponding to the __number of days worked in a
#'   particular shift within a shift cycle__ value from a shift version of the
#'   MCTQ questionnaire.
#' @param n_f An [integerish][checkmate::test_integerish()] `integer` or
#'   `numeric` object corresponding to the __number of free days after a
#'   particular shift within a shift cycle__ value from a shift version of the
#'   MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the weighted mean of `sd_w` and
#'   `sd_f` with `n_w` and `n_f` as weights.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sd_overall(lubridate::dhours(5), lubridate::dhours(9), 2, 2)
#' #> [1] "25200s (~7 hours)" # Expected
#' sd_overall(lubridate::dhours(3.45), lubridate::dhours(10), 3, 1)
#' #> [1] "18315s (~5.09 hours)" # Expected
#' sd_overall(lubridate::as.duration(NA), lubridate::dhours(12), 4, 4)
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' sd_w <- c(lubridate::dhours(4), lubridate::dhours(7))
#' sd_f <- c(lubridate::dhours(12), lubridate::dhours(9))
#' n_w <- c(3, 4)
#' n_f <- c(2, 4)
#' sd_overall(sd_w, sd_f, n_w, n_f)
#' #> [1] "25920s (~7.2 hours)" "28800s (~8 hours)"  # Expected
#'
#' ## __ Checking second output from vectorized example __
#' i <- 2
#' x <- c(sd_w[i], sd_f[i])
#' w <- c(n_w[i], n_f[i])
#' lubridate::as.duration(stats::weighted.mean(x, w))
#' #> [1] "28800s (~8 hours)" # Expected
#'
#' ## __ Converting the output to hms __
#' x <- sd_overall(lubridate::dhours(4.75), lubridate::dhours(10), 5, 2)
#' convert(x, "hms")
#' #> 06:15:00 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- sd_overall(lubridate::dhours(5.9874), lubridate::dhours(9.3), 3, 2)
#' x
#' #> [1] "26324.784s (~7.31 hours)" # Expected
#' round_time(x)
#' #> [1] "26325s (~7.31 hours)" # Expected
sd_overall <- function(sd_w, sd_f, n_w, n_f) {

    assert_duration(sd_w)
    assert_duration(sd_f)
    checkmate::assert_integerish(n_w)
    checkmate::assert_integerish(n_f)
    checkmate::assert_numeric(n_w, lower = 0)
    checkmate::assert_numeric(n_f, lower = 0)
    assert_identical(n_w, n_f, sd_w, sd_f, type = "length")

    n_w <- as.integer(n_w)
    n_f <- as.integer(n_f)

    ((sd_w * n_w) + (sd_f * n_f)) / (n_w + n_f)

}
