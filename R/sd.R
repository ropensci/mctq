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
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `sd()` (\eqn{SD}) computation are as follow.
#'
#' __\deqn{SE - SO}__
#'
#' Where:
#'
#' * \eqn{SE} = sleep end.
#' * \eqn{SO} = sleep onset.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SEw - SOw}).
#'
#' @param so A `hms` object corresponding to the __sleep onset__ value
#'   from a standard, micro, or shift version of the MCTQ questionnaire. You can
#'   use [mctq::so()] to compute it for the standard or shift version.
#' @param se A `hms` object corresponding to the __sleep end__ value
#'   from a standard, micro, or shift version of the MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the difference between
#'   `se` and `so` rolled on a 24-hour clock basis.
#'
#' @template mctq_b
#' @template references_a
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
#' For reference, Juda, Vetter & Roenneberg ([2013](https://bit.ly/38IEEk4)),
#' and theWeP [(n.d.)](http://bit.ly/3pv8EH1) guidelines for `napd()`
#' (\eqn{NapD}) computation are as follow.
#'
#' __\deqn{NapE - NapO}__
#'
#' Where:
#'
#' * \eqn{NapO} = local time of nap onset ("I take a nap from ... o'clock
#' [...]").
#' * \eqn{NapE} = local time of nap end ("[...] to ... o'clock").
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{NapEwm - NapOwm}).
#'
#' @param napo A `hms` object corresponding to the __nap onset__ value
#'   from the shift version of the MCTQ questionnaire.
#' @param nape A `hms` object corresponding to the __nap end__ value
#'   from the shift version of the MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the difference between
#'   `nape` and `napo` rolled on a 24-hour clock basis.
#'
#' @template mctq_b
#' @template references_a
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
#' For reference, Juda, Vetter & Roenneberg ([2013](https://bit.ly/38IEEk4)),
#' and theWeP [(n.d.)](http://bit.ly/3pv8EH1) guidelines for `sd24()`
#' (\eqn{SD24}) computation are as follow.
#'
#' __\deqn{SD + Napd}__
#'
#' Where:
#'
#' * \eqn{SD} = sleep duration.
#' * \eqn{NapD} = nap duration.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SDwm + NapDwm}).
#'
#' @param sd A `Duration` object corresponding to the __sleep duration__ value
#'   from the shift version of the MCTQ questionnaire. You can use [mctq::sd()]
#'   to compute it.
#' @param napd A `Duration` object corresponding to the __nap duration__ value
#'   from the shift version of the MCTQ questionnaire. You can use
#'   [mctq::napd()] to compute it.
#'
#' @return A `Duration` object corresponding to the sum between `sd` and `napd`.
#'
#' @template mctq_b
#' @template references_a
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
#' See [mctq::sd_overall()] to compute the overall sleep duration for
#' the shift version of the MCTQ.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `sd_week()` (\eqn{SD_{week}}) computation are as follow.
#'
#' __\deqn{\frac{SD_w \times WD + SD_f \times FD}{7}}__
#'
#' Where:
#'
#' * \eqn{SD_w} = sleep duration on workdays.
#' * \eqn{WD} = number of workdays per week.
#' * \eqn{SD_f} = sleep duration on work-free days.
#' * \eqn{FD} = number of work-free days per week.
#'
#' @param sd_w A `Duration` object corresponding to the __sleep duration on work
#'   days__ value from a standard or micro version of the MCTQ questionnaire
#'   (you can use [mctq::sd()] to compute it).
#' @param sd_f A `Duration` object corresponding to the __sleep duration on
#'   work-free days__ value from a standard or micro version of the MCTQ
#'   questionnaire (you can use [mctq::sd()] to compute it).
#'
#' @return A `Duration` object corresponding to the average weekly sleep
#'   duration.
#'
#' @inheritParams fd
#' @template mctq_b
#' @template mctq_c
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sd_week(5, lubridate::dhours(4), lubridate::dhours(8))
#' #> [1] "18514.2857142857s (~5.14 hours)" # Expected
#' sd_week(4, lubridate::dhours(7), lubridate::dhours(7))
#' #> [1] "25200s (~7 hours)" # Expected
#' sd_week(6, lubridate::as.duration(NA), lubridate::dhours(10))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' wd <- c(3, 7)
#' sd_w <- c(lubridate::dhours(4.5), lubridate::dhours(5.45))
#' sd_f <- c(lubridate::dhours(8), lubridate::dhours(7.3))
#' sd_week(wd, sd_w, sd_f)
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
#' x <- sd_week(5, lubridate::dhours(5.45), lubridate::dhours(9.5))
#' convert_to(x, "hms")
#' #> 06:36:25.714286 # Expected
#' convert_to(as.integer(x), "hms") # if you want to discard the milliseconds.
#' #> 06:36:25 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- sd_week(3, lubridate::dhours(4.5), lubridate::dhours(7.8))
#' x
#' #> [1] "22988.5714285714s (~6.39 hours)" # Expected
#' round_time(x)
#' #> [1] "22989s (~6.39 hours)" # Expected
sd_week <- function(wd, sd_w, sd_f) {

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_identical(wd, sd_w, sd_f, type = "length")

    ((sd_w * wd) + (sd_f * fd(wd))) / 7

}

#' Compute MCTQ overall sleep duration (only for MCTQ Shift)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sd_overall()` computes the __overall sleep duration__ for the shift version
#' of the Munich Chronotype Questionnaire (MCTQ).
#'
#' See [mctq::sd_week()] to compute the average weekly sleep duration for the
#' standard and micro versions of the MCTQ.
#'
#' @section Operation:
#'
#' The shift version of the MCTQ was developed for shift-workers rotating
#' through morning-, evening-, and night-shifts (transition times at 6:00 a.m.,
#' 2:00 p.m., and 10:00 p.m.), but it also allows adaptations to other shift
#' schedules (Juda, Vetter, & Roenneberg, [2013](https://bit.ly/38IEEk4)). For
#' that reason, `sd_overall()` must operate considering any shift combination.
#'
#' Considering the requirement above, `sd_overall()` was developed to only
#' accept lists values as arguments. For this approach to work, both `n` and
#' `sd` arguments must be lists with paired elements and values between `n` and
#' `sd`, _i.e._ the first element of `n` (_e.g._ `n_wm`) must be paired with the
#' first element of `sd` (_e.g._ `sd_wm`). The function will do the work of
#' combining them and output a weighted mean.
#'
#' @section Guidelines:
#'
#' For reference, Juda, Vetter, & Roenneberg ([2013](https://bit.ly/38IEEk4))
#' and theWeP [(n.d.)](http://bit.ly/3pv8EH1) guidelines for `sd_overall()`
#' computation are as follow.
#'
#' __\deqn{((SDwMEN * nwMEN) + (SDfMEN * nfMEN)) / (nwMEN + nfMEN)}__
#'
#' Where:
#'
#' * \eqn{SDwMEN} = sleep duration in each shift (MEN`*`).
#' * \eqn{nwMEN} = number of days worked in each shift (MEN`*`) within a shift
#' cycle.
#' * \eqn{SDfMEN} = sleep duration between two free days after each shift
#' (MEN`*`).
#' * \eqn{nwMEN} = number of free days after each shift (MEN`*`) within a shift
#' cycle.
#'
#' `*` \eqn{M}: morning; \eqn{E}: evening; \eqn{N}: night.
#'
#' Note that the overall sleep duration is the weighted average of
#' shift-specific sleep durations, _i.e._ \eqn{(SDwMEN * nwMEN)} and
#' \eqn{(SDfMEN * nfMEN)} must be unfold for each shift and non-shift
#' (_e.g._ \eqn{(SDwM * nw_M) + (SDwE * nwE) ...}).
#'
#' @param n A `list` object with [integerish][rlang::is_integerish()] `integer`
#'   or `numeric` elements corresponding to the __number of days from each
#'   shift__ and __number of free days after each shift__ from a MCTQ shift
#'   questionnaire. `n` elements and values must be paired with `sd` elements
#'   and values.
#' @param sd A `list` object with `Duration` elements corresponding to the
#'   __sleep duration in each shift__ and __sleep duration between two free days
#'   after each shift__ from a MCTQ shift questionnaire (you can use
#'   [mctq::sd()] to compute it). `sd` elements and values must be paired with
#'   `n` elements and values.
#'
#' @return A `Duration` object corresponding to the weighted mean of `sd` and
#'   `n` (weights).
#'
#' @template mctq_b
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' n <- list(n_wm = 2, n_we = 4, n_wn = 3, n_fm = 2, n_fw = 4, n_fn = 2)
#' sd <- list(sd_wm = lubridate::dhours(5), sd_we = lubridate::dhours(6),
#'            sd_wn = lubridate::dhours(5), sd_fm = lubridate::dhours(10),
#'            sd_fe = lubridate::dhours(9), sd_fn = lubridate::dhours(8.5))
#' sd_overall(n, sd)
#' #> [1] "25835.2941176471s (~7.18 hours)" # Expected
#'
#' ## __ Vectorized example __
#' n <- list(n_wm = c(4, 3), n_we = c(2, 3), n_fm = c(2, 2), n_fe = c(4, 4))
#' sd <- list(sd_wm = c(lubridate::dhours(6), lubridate::dhours(7)),
#'            sd_we = c(lubridate::dhours(5.5), lubridate::dhours(6)),
#'            sd_fm = c(lubridate::dhours(9), lubridate::dhours(8.5)),
#'            sd_fe = c(lubridate::dhours(7), lubridate::dhours(10)))
#' sd_overall(n, sd)
#' #> [1] "24300s (~6.75 hours)" "28800s (~8 hours)"  # Expected
#'
#' ## __ Checking the second output from vectorized example __
#' i <- 2
#' x <- c(sd[["sd_wm"]][i], sd[["sd_we"]][i], sd[["sd_fm"]][i],
#'        sd[["sd_fe"]][i])
#' w <- c(n[["n_wm"]][i], n[["n_we"]][i], n[["n_fm"]][i], n[["n_fe"]][i])
#' lubridate::as.duration(stats::weighted.mean(x, w))
#' #> [1] "28800s (~8 hours)" # Expected
#'
#' ## __ Converting the output to hms __
#' n <- list(n_wm = 3, n_we = 2, n_wn = 2, n_fm = 1, n_fw = 3, n_fn = 5)
#' sd <- list(sd_wm = lubridate::dhours(4.12), sd_we = lubridate::dhours(6),
#'            sd_wn = lubridate::dhours(3.43), sd_fm = lubridate::dhours(9.5),
#'            sd_fe = lubridate::dhours(7.32), sd_fn = lubridate::dhours(10.5))
#' sd_overall(n, sd)
#' #> [1] "25915.5s (~7.2 hours)" # Expected
#' convert_to(sd_overall(n, sd), "hms")
#' #> 07:11:55.5 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' round_time(sd_overall(n, sd))
#' #> [1] "25916s (~7.2 hours)" # Expected
#' round_time(convert_to(sd_overall(n, sd), "hms"))
#' #> 07:11:56 # Expected
sd_overall <- function(n, sd) {

    checkmate::assert_list(n, len = length(sd))
    checkmate::assert_list(sd, len = length(n))
    lapply(n, checkmate::assert_integerish)
    lapply(sd, assert_duration)
    mapply(assert_identical, n, sd, MoreArgs = list(type = "length"))

    foo <- function(x, y) {
        out <- Reduce("*", list(x, y))
        lubridate::as.duration(out)
    }

    sd <- mapply(foo, n, sd, SIMPLIFY = FALSE)
    sd <- Reduce("+", sd)
    n <- Reduce("+", n)

    sd / n

}
