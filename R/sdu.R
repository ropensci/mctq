#' Compute MCTQ sleep duration
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sdu()` computes the __sleep duration__ for standard, micro, and shift
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' Please note that, although we tried to preserve the original authors' naming
#' pattern for the MCTQ functions, the name `sd` provokes a dangerous name
#' collision with the widely used [stats::sd()] function (standard deviation).
#' That's why we named it `sdu`. `sdu()` and [msl()] are the only exceptions,
#' all the other `mctq` functions maintain a strong naming resemblance with the
#' original authors' naming pattern.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi et al. (2020), Juda,
#' Vetter, & Roenneberg (2013), and The Worldwide Experimental Platform (n.d.)
#' guidelines for `sdu()` (\eqn{SD}) computation are as follows.
#'
#' ## Notes
#'
#' * The computation below must be applied to each section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## For standard and micro versions of the MCTQ
#'
#' __\deqn{SE_{W/F} - SO_{W/F}}{SE_W/F - SO_W/F}__
#'
#' Where:
#'
#' * \eqn{SE_{W/F}}{SE_W/F} = local time of sleep end on work __or__ work-free
#' days.
#' * \eqn{SO_{W/F}}{SO_W/F}  = local time of sleep onset on work __or__
#' work-free days.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' ## For the shift version of the MCTQ
#'
#' __\deqn{SE_{W/F}^{M/E/N} - SO_{W/F}^{M/E/N}}{SE_W/F_M/E/N - SO_W/F_M/E/N}__
#'
#' Where:
#'
#' * \eqn{SE_{W/F}^{M/E/N}}{SE_W/F_M/E/N} = local time of sleep end between two
#' days in a particular shift __or__ between two free days after a particular
#' shift.
#' * \eqn{SO_{W/F}^{M/E/N}}{SO_W/F_M/E/N}  = local time of sleep onset between
#' two days in a particular shift __or__ between two free days after a
#' particular shift.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days, \eqn{M} =
#' morning shift; \eqn{E} = evening shift; \eqn{N} = night shift.
#'
#' @param so A `hms` object corresponding to the __local time of sleep onset__
#'   from a standard, micro, or shift version of the MCTQ questionnaire. You can
#'   use [mctq::so()] to compute it for the standard or shift version.
#' @param se A `hms` object corresponding to the __local time of sleep end__
#'   from a standard, micro, or shift version of the MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the vectorized difference
#'   between `se` and `so` in a circular time frame of 24 hours.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' so <- hms::parse_hm("23:00")
#' se <- hms::parse_hm("08:00")
#' sdu(so, se)
#' #> [1] "32400s (~9 hours)" # Expected
#'
#' so <- hms::parse_hm("02:00")
#' se <- hms::parse_hm("12:30")
#' sdu(so, se)
#' #> [1] "37800s (~10.5 hours)" # Expected
#'
#' so <- hms::parse_hm("03:15")
#' se <- hms::as_hms(NA)
#' sdu(so, se)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' so <- c(hms::parse_hm("04:12"), hms::parse_hm("21:20"))
#' se <- c(hms::parse_hm("14:30"), hms::parse_hm("03:45"))
#' sdu(so, se)
#' #> [1] "37080s (~10.3 hours)" "23100s (~6.42 hours)" # Expected
sdu <- function(so, se) {
    assert_hms(so, lower = hms::hms(0))
    assert_hms(se, lower = hms::hms(0))
    assert_identical(so, se, type = "length")

    vct_sum_time(se, - so, cycle = lubridate::ddays())
}

#' Compute MCTQ nap duration (only for MCTQ\eqn{^{Shift}}{ Shift})
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `napd()` computes the __nap duration__ for the shift version of the Munich
#' Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Juda, Vetter & Roenneberg (2013) and The Worldwide Experimental Platform
#' (n.d.) guidelines for `napd()` (\eqn{NapD}) computation are as follows.
#'
#' ## Notes
#'
#' * The computation below must be applied to each shift section of the
#' questionnaire.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
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
#' @param napo A `hms` object corresponding to the __local time of nap onset__
#'   from the shift version of the MCTQ questionnaire.
#' @param nape A `hms` object corresponding to the __local time of nap end__
#'   from the shift version of the MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the vectorized difference
#'   between `nape` and `napo` in a circular time frame of 24 hours.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' napo <- hms::parse_hm("12:30")
#' nape <- hms::parse_hm("14:20")
#' napd(napo, nape)
#' #> [1] "6600s (~1.83 hours)"" # Expected
#'
#' napo <- hms::parse_hm("23:45")
#' nape <- hms::parse_hm("00:30")
#' napd(napo, nape)
#' #> [1] "2700s (~45 minutes)" # Expected
#'
#' napo <- hms::parse_hm("10:20")
#' nape <- hms::as_hms(NA)
#' napd(napo, nape)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' napo <- c(hms::parse_hm("01:25"), hms::parse_hm("23:50"))
#' nape <- c(hms::parse_hm("03:10"), hms::parse_hm("01:10"))
#' napd(napo, nape)
#' #> [1] "6300s (~1.75 hours)" "4800s (~1.33 hours)"  # Expected
napd <- function(napo, nape) {
    assert_hms(napo, lower = hms::hms(0))
    assert_hms(nape, lower = hms::hms(0))
    assert_identical(napo, nape, type = "length")

    vct_sum_time(nape, - napo, cycle = lubridate::ddays())
}

#' Compute MCTQ 24 hours sleep duration (only for MCTQ\eqn{^{Shift}}{ Shift})
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sd24()` computes the __24 hours sleep duration__ for the shift version of
#' the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' Juda, Vetter & Roenneberg (2013) and The Worldwide Experimental Platform
#' (n.d.) guidelines for `sd24()` (\eqn{SD24}) computation are as follows.
#'
#' ## Notes
#'
#' * The computation below must be applied to each shift section of the
#' questionnaire.
#'
#' * If the respondent don't usually take a nap in a particular shift __or__
#' between two free days after a particular shift, `sd24()` will return only
#' \eqn{SD_{W/F}^{M/E/N}}{SD_W/F_M/E/N}.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
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
#' @param sd A `Duration` object corresponding to the __sleep duration__ from
#'   the shift version of the MCTQ questionnaire. You can use [mctq::sdu()] to
#'   compute it.
#' @param napd A `Duration` object corresponding to the __nap duration__ from
#'   the shift version of the MCTQ questionnaire. You can use [mctq::napd()] to
#'   compute it.
#' @param nap A `logical` value corresponding to the __"I usually take a nap"__
#'   from the shift version of the MCTQ questionnaire.
#'
#' @return
#'
#' * If `nap == TRUE`, a `Duration` object corresponding to the vectorized sum
#' of `sd` and `napd` in a circular time frame of 24 hours.
#' * If `nap == FALSE`, a `Duration` object equal to `sd`.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' sd <- lubridate::dhours(6)
#' napd <- lubridate::dhours(0.5)
#' nap <- TRUE
#' sd24(sd, napd, nap)
#' #> [1] "23400s (~6.5 hours)" # Expected
#'
#' sd <- lubridate::dhours(9)
#' napd <- lubridate::dhours(1.5)
#' nap <- TRUE
#' sd24(sd, napd, nap)
#' #> [1] "37800s (~10.5 hours)" # Expected
#'
#' sd <- lubridate::dhours(6.5)
#' napd <- lubridate::as.duration(NA)
#' nap <- FALSE
#' sd24(sd, napd, nap)
#' #> [1] "23400s (~6.5 hours)" # Expected
#'
#' sd <- lubridate::as.duration(NA)
#' napd <- lubridate::dhours(2.3)
#' nap <- TRUE
#' sd24(sd, napd, nap)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' sd <- c(lubridate::dhours(7.5), lubridate::dhours(8))
#' napd <- c(lubridate::dhours(0.75), lubridate::dhours(1))
#' nap <- c(TRUE, TRUE)
#' sd24(sd, napd, nap)
#' #> [1] "29700s (~8.25 hours)" "32400s (~9 hours)" # Expected
sd24 <- function(sd, napd, nap) {
    assert_duration(sd, lower = lubridate::duration(0))
    assert_duration(napd, lower = lubridate::duration(0))
    checkmate::assert_logical(nap)
    assert_identical(sd, napd, nap, type = "length")

    # `case_when` is used here to to ensure that the function returns a
    # result when `nap` is `FALSE`.

    dplyr::case_when(
        nap == FALSE ~ sd,
        TRUE ~ vct_sum_time(sd, napd)
    )
}

#' Compute MCTQ average weekly sleep duration
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sd_week()` computes the __average weekly sleep duration__ for the standard
#' and micro versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' See [mctq::sd_overall()] to compute the overall sleep duration of a
#' particular shift for the shift version of the MCTQ.
#'
#' @section Guidelines:
#'
#' Roenneberg, Allebrandt, Merrow, & Vetter (2012), Ghotbi et al. (2020), and
#' The Worldwide Experimental Platform (n.d.) guidelines for `sd_week()`
#' (\eqn{SD_{week}}{SD_week}) computation are as follows.
#'
#' ## Notes
#'
#' * The average weekly sleep duration is the weighted average of the sleep
#' durations on work and work-free days in a week.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
#'
#' ## Computation
#'
#' __\deqn{\frac{SD_W \times WD + SD_F \times FD}{7}}{
#' (SD_W * WD + SD_F * FD) / 7}__
#'
#' Where:
#'
#' * \eqn{SD_W} = sleep duration on workdays.
#' * \eqn{SD_F} = sleep duration on work-free days.
#' * \eqn{WD} = number of workdays per week ("I have a regular work schedule and
#' work ___ days per week").
#' * \eqn{FD} = number of work-free days per week.
#'
#' \strong{*} \eqn{W} = workdays; \eqn{F} = work-free days.
#'
#' @param sd_w A `Duration` object corresponding to the __sleep duration on
#'   workdays__ from a standard or micro version of the MCTQ questionnaire. You
#'   can use [mctq::sdu()] to compute it.
#' @param sd_f A `Duration` object corresponding to the __sleep duration on
#'   work-free days__ from a standard or micro version of the MCTQ
#'   questionnaire. You can use [mctq::sdu()] to compute it.
#'
#' @return A `Duration` object corresponding to the vectorized weighted mean of
#'   `sd_w` and `sd_f` with `wd` and `fd(wd)` as weights.
#'
#' @inheritParams fd
#' @template details_b
#' @template section_a
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' sd_w <- lubridate::dhours(4)
#' sd_f <- lubridate::dhours(8)
#' wd <- 5
#' sd_week(sd_w, sd_f, wd)
#' #> [1] "18514.2857142857s (~5.14 hours)" # Expected
#'
#' sd_w <- lubridate::dhours(7)
#' sd_f <- lubridate::dhours(7)
#' wd <- 4
#' sd_week(sd_w, sd_f, wd)
#' #> [1] "25200s (~7 hours)" # Expected
#'
#' sd_w <- lubridate::as.duration(NA)
#' sd_f <- lubridate::dhours(10)
#' wd <- 6
#' sd_week(sd_w, sd_f, wd)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' sd_w <- c(lubridate::dhours(4.5), lubridate::dhours(5.45))
#' sd_f <- c(lubridate::dhours(8), lubridate::dhours(7.3))
#' wd <- c(3, 7)
#' sd_week(sd_w, sd_f, wd)
#' #> [1] "23400s (~6.5 hours)"  "19620s (~5.45 hours)" # Expected
#'
#' ## Checking second output from vector example
#'
#' if (requireNamespace("stats", quietly = TRUE)) {
#'     i <- 2
#'     x <- c(sd_w[i], sd_f[i])
#'     w <- c(wd[i], fd(wd[i]))
#'     lubridate::as.duration(stats::weighted.mean(x, w))
#' }
#' #> [1] "19620s (~5.45 hours)" # Expected
#'
#' ## Converting the output to `hms`
#'
#' sd_w <- lubridate::dhours(5.45)
#' sd_f <- lubridate::dhours(9.5)
#' wd <- 5
#' x <- sd_week(sd_w, sd_f, wd)
#' x
#' #> [1] "23785.7142857143s (~6.61 hours)" # Expected
#' hms::as_hms(as.numeric(x))
#' #> 06:36:25.714286 # Expected
#'
#' ## Rounding the output at the seconds level
#'
#' sd_w <- lubridate::dhours(4.5)
#' sd_f <- lubridate::dhours(7.8)
#' wd <- 3
#' x <- sd_week(sd_w, sd_f, wd)
#' x
#' #> [1] "22988.5714285714s (~6.39 hours)" # Expected
#' round_time(x)
#' #> [1] "22989s (~6.39 hours)" # Expected
sd_week <- function(sd_w, sd_f, wd) {
    assert_duration(sd_w, lower = lubridate::duration(0))
    assert_duration(sd_f, lower = lubridate::duration(0))
    assert_numeric_(wd)
    checkmate::assert_integerish(wd, lower = 0, upper = 7)
    assert_identical(sd_w, sd_f, wd, type = "length")

    wd <- as.integer(wd)

    ((sd_w * wd) + (sd_f * fd(wd))) / 7
}

#' Compute MCTQ overall sleep duration (only for MCTQ\eqn{^{Shift}}{ Shift})
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sd_overall()` computes the __overall sleep duration in a particular shift__
#' for the shift version of the Munich Chronotype Questionnaire (MCTQ).
#'
#' See [mctq::sd_week()] to compute the average weekly sleep duration for the
#' standard and micro versions of the MCTQ.
#'
#' @section Guidelines:
#'
#' Juda, Vetter, & Roenneberg (2013) and The Worldwide Experimental Platform
#' (n.d.) guidelines for `sd_overall()` (\eqn{\emptyset SD^{M/E/N}}{OSD_M/E/N})
#' computation are as follows.
#'
#' ## Notes
#'
#' * The computation below must be applied to each shift section of the
#' questionnaire. If you're using the three-shift design proposed by the
#' authors, you need to compute three overall sleep duration (e.g.,
#' \eqn{\emptyset SD^M}{OSD_M}; \eqn{\emptyset SD^E}{OSD_E}; \eqn{\emptyset
#' SD^N}{OSD_N}).
#'
#' * The overall sleep duration is the weighted average of the shift-specific
#' mean sleep durations.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble understanding the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipso.github.io/mctq/reference/).
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
#'   two days in a particular shift__ from a shift version of the MCTQ
#'   questionnaire. You can use [mctq::sdu()] to compute it.
#' @param sd_f A `Duration` object corresponding to the __sleep duration between
#'   two free days after a particular shift__ from a shift version of the MCTQ
#'   questionnaire. You can use [mctq::sdu()] to compute it.
#' @param n_w An [integerish][checkmate::test_integerish()] `numeric` object or
#'   an `integer` object corresponding to the __number of days worked in a
#'   particular shift within a shift cycle__ from a shift version of the MCTQ
#'   questionnaire.
#' @param n_f An [integerish][checkmate::test_integerish()] `numeric` object or
#'   an `integer` object corresponding to the __number of free days after a
#'   particular shift within a shift cycle__ from a shift version of the MCTQ
#'   questionnaire.
#'
#' @return A `Duration` object corresponding to the vectorized weighted mean of
#'   `sd_w` and `sd_f` with `n_w` and `n_f` as weights.
#'
#' @template details_b
#' @template references_a
#' @family MCTQ functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' sd_w <- lubridate::dhours(5)
#' sd_f <- lubridate::dhours(9)
#' n_w <- 2
#' n_f <- 2
#' sd_overall(sd_w, sd_f, n_w, n_f)
#' #> [1] "25200s (~7 hours)" # Expected
#'
#' sd_w <- lubridate::dhours(3.45)
#' sd_f <- lubridate::dhours(10)
#' n_w <- 3
#' n_f <- 1
#' sd_overall(sd_w, sd_f, n_w, n_f)
#' #> [1] "18315s (~5.09 hours)" # Expected
#'
#' sd_w <- lubridate::as.duration(NA)
#' sd_f <- lubridate::dhours(12)
#' n_w <- 4
#' n_f <- 4
#' sd_overall(sd_w, sd_f, n_w, n_f)
#' #> [1] NA # Expected
#'
#' ## Vector example
#'
#' sd_w <- c(lubridate::dhours(4), lubridate::dhours(7))
#' sd_f <- c(lubridate::dhours(12), lubridate::dhours(9))
#' n_w <- c(3, 4)
#' n_f <- c(2, 4)
#' sd_overall(sd_w, sd_f, n_w, n_f)
#' #> [1] "25920s (~7.2 hours)" "28800s (~8 hours)"  # Expected
#'
#' ## Checking second output from vector example
#'
#' if (requireNamespace("stats", quietly = TRUE)) {
#'     i <- 2
#'     x <- c(sd_w[i], sd_f[i])
#'     w <- c(n_w[i], n_f[i])
#'     lubridate::as.duration(stats::weighted.mean(x, w))
#' }
#' #> [1] "28800s (~8 hours)" # Expected
#'
#' ## Converting the output to `hms`
#'
#' sd_w <- lubridate::dhours(4.75)
#' sd_f <- lubridate::dhours(10)
#' n_w <- 5
#' n_f <- 2
#' x <- sd_overall(sd_w, sd_f, n_w, n_f)
#' x
#' #> [1] "22500s (~6.25 hours)" # Expected
#' hms::as_hms(as.numeric(x))
#' #> 06:15:00 # Expected
#'
#' ## Rounding the output at the seconds level
#'
#' sd_w <- lubridate::dhours(5.9874)
#' sd_f <- lubridate::dhours(9.3)
#' n_w <- 3
#' n_f <- 2
#' x <- sd_overall(sd_w, sd_f, n_w, n_f)
#' x
#' #> [1] "26324.784s (~7.31 hours)" # Expected
#' round_time(x)
#' #> [1] "26325s (~7.31 hours)" # Expected
sd_overall <- function(sd_w, sd_f, n_w, n_f) {
    assert_duration(sd_w, lower = lubridate::duration(0))
    assert_duration(sd_f, lower = lubridate::duration(0))
    assert_numeric_(n_w)
    assert_numeric_(n_f)
    checkmate::assert_integerish(n_w, lower = 0)
    checkmate::assert_integerish(n_f, lower = 0)
    assert_identical(sd_w, sd_f, n_w, n_f, type = "length")

    n_w <- as.integer(n_w)
    n_f <- as.integer(n_f)

    ((sd_w * n_w) + (sd_f * n_f)) / (n_w + n_f)
}
