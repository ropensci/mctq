#' Compute MCTQ work-free days
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `fd()` computes the __number of work-free days per week__ for standard
#' and micro versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)) and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `fd()` (\eqn{FD}) computation are as follow.
#'
#' __\deqn{7 - WD}__
#'
#' Where:
#'
#' * \eqn{WD} = number of workdays.
#'
#' @param wd An integerish number corresponding to the __number of work days per
#'   week__ value from a standard or micro version of the MCTQ questionnaire.
#'
#' @return A numeric value equivalent to `7 - wd`, _i.e._ the difference between
#'   the number of days in a week and the number of work days.
#'
#' @template mctq_a
#' @template references_a
#' @export
#'
#' @examples
#' fd(5)
#' #> [1] 2 # Expected
#' fd(0:7)
#' #> [1] 7 6 5 4 3 2 1 0 # Expected
#' fd(c(1, NA))
#' #> [1]  6 NA # Expected
fd <- function(wd) {

    checkmate::assert_numeric(wd, lower = 0, upper = 7)

    as.integer(7 - wd)

}

#' Compute MCTQ sleep onset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `so()` computes the __time of sleep onset__ for standard and shift
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' Note that this value is collected directly from the questionnaire if you're
#' using the \eqn{\mu}MCTQ.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `so()` (\eqn{SO}) computation are as follow.
#'
#' __\deqn{SPrep + SLat}__
#'
#' Where:
#'
#' * \eqn{SPrep} = local time of preparing to sleep.
#' * \eqn{SLat} = sleep latency ("I need ... min to fall asleep").
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SPrepw + SPrepw}).
#'
#' @param sprep A `hms` object corresponding to the __local time of getting out
#'   of bed__ value from a standard or shift version of the MCTQ questionnaire.
#' @param slat A `Duration` object corresponding to the __sleep latency__ value
#'   from a standard or shift version of the MCTQ questionnaire.
#'
#' @return A `hms` object corresponding to the sum of `sprep` and `slat` rolled
#'   on a 24-hour clock basis.
#'
#' @template mctq_b
#' @template mctq_c
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' so(hms::parse_hms("22:00:00"), lubridate::dminutes(15))
#' #> 22:15:00 # Expected
#' so(hms::parse_hms("23:30:00"), lubridate::dminutes(45))
#' #> 00:15:00 # Expected
#' so(hms::parse_hms("20:45:00"), lubridate::as.duration(NA))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' sprep <- c(hms::parse_hms("21:30:00"), hms::parse_hms("22:15:00"))
#' slat <- c(lubridate::dminutes(45), lubridate::dminutes(5))
#' so(sprep, slat)
#' #> 22:15:00 # Expected
#' #> 22:20:00 # Expected
so <- function(sprep, slat) {

    checkmate::assert_class(sprep, "hms")
    assert_duration(slat)
    assert_identical(sprep, slat, type = "length")

    sum_time(sprep, slat, class = "hms", clock = TRUE, vectorize = TRUE)

}

#' Compute MCTQ local time of getting out of bed
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `gu()` computes the __local time of getting out of bed__ for standard and
#' shift versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `gu()` (\eqn{GU}) computation are as follow.
#'
#' __\deqn{SE + SI}__
#'
#' Where:
#'
#' * \eqn{SE} = sleep end.
#' * \eqn{SI} = sleep inertia ("after ... min, I get up").
#'
#' MCTQ Shift uses \eqn{TGU} (time to get up) instead of \eqn{SI}, but both
#' represent the same thing.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SEw + SIw}).
#'
#' @param se A `hms` object corresponding to the __sleep end__ value from a
#'   standard or shift version of the MCTQ questionnaire.
#' @param si A `Duration` object corresponding to the __sleep inertia__ or
#'   __time to get up__ value from a standard or shift version of the MCTQ
#'   questionnaire.
#'
#' @return A `hms` object corresponding to the sum of `se` and `si` rolled on a
#'   24-hour clock basis.
#'
#' @template mctq_b
#' @template mctq_c
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' gu(hms::parse_hms("08:00:00"), lubridate::dminutes(10))
#' #> 08:10:00 # Expected
#' gu(hms::parse_hms("11:45:00"), lubridate::dminutes(90))
#' #> 13:15:00 # Expected
#' gu(hms::as_hms(NA), lubridate::dminutes(90))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' se <- c(hms::parse_hms("12:30:00"), hms::parse_hms("06:40:00"))
#' si <- c(lubridate::dminutes(10), lubridate::dminutes(10))
#' gu(se, si)
#' #> 12:40:00 # Expected
#' #> 06:50:00 # Expected
gu <- function(se, si) {

    checkmate::assert_class(se, "hms")
    assert_duration(si)
    assert_identical(se, si, type = "length")

    sum_time(se, si, class = "hms", clock = TRUE, vectorize = TRUE)

}

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
#' @template mctq_c
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
#' @template mctq_c
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
#' @template mctq_c
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

#' Compute MCTQ total time in bed
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tbt()` computes the __total time in bed__ for standard and shift versions of
#' the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `tbt()` (\eqn{TBT}) computation are as follow.
#'
#' __\deqn{GU - BT}__
#'
#' Where:
#'
#' * \eqn{BT} = Local time of going to bed ("I go to bed at ... o'clock").
#' * \eqn{GU} = local time of getting out of bed.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{GUw - BTw}).
#'
#' @param bt A `hms` object corresponding to the __local time of going to bed__
#'   value from a standard or shift version of the MCTQ questionnaire.
#' @param gu A `hms` object corresponding to the __local time of getting out of
#'   bed__ value from a standard or shift version of the MCTQ questionnaire. You
#'   can use [mctq::gu()] to compute it.
#'
#' @return A `Duration` object corresponding to the difference between `gu` and
#'   `bt` rolled on a 24-hour clock basis.
#'
#' @template mctq_b
#' @template mctq_c
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' tbt(hms::parse_hms("22:10:00"), hms::parse_hms("06:15:00"))
#' #> [1] "29100s (~8.08 hours)" # Expected
#' tbt(hms::parse_hms("01:20:00"), hms::parse_hms("14:00:00"))
#' #> [1] "45600s (~12.67 hours)" # Expected
#' tbt(hms::as_hms(NA), hms::parse_hms("07:20:00"))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' bt <- c(hms::parse_hms("23:50:00"), hms::parse_hms("02:30:00"))
#' gu <- c(hms::parse_hms("09:30:00"), hms::parse_hms("11:25:00"))
#' tbt(bt, gu)
#' #> [1] "34800s (~9.67 hours)" "32100s (~8.92 hours)" # Expected
tbt <- function(bt, gu) {

    checkmate::assert_class(bt, "hms")
    checkmate::assert_class(gu, "hms")
    assert_identical(bt, gu, type = "length")

    sum_time(gu, - bt, class = "Duration", clock = TRUE, vectorize = TRUE)

}

#' Compute MCTQ mid-sleep
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `ms()` computes the __mid-sleep__ for standard, micro, and shift versions of
#' the Munich Chronotype Questionnaire (MCTQ).
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `ms()` (\eqn{MS}) computation are as follow.
#'
#' __\deqn{SO + (SD / 2)}__
#'
#' Where:
#'
#' * \eqn{SO} = sleep onset.
#' * \eqn{SD} = sleep duration.
#'
#' Note that this computation must be applied to each section of the
#' questionnaire (_e.g._ \eqn{SOw + (SDw / 2)}).
#'
#' @param so A `hms` object corresponding to the __sleep onset__ value from a
#'   standard, micro, or shift version of the MCTQ questionnaire. You can use
#'   [mctq::so()] to compute it for the standard or shift version.
#' @param sd A `Duration` object corresponding to the __sleep duration__ value
#'   from a standard, micro, or shift version of the MCTQ questionnaire. You can
#'   use [mctq::sd()] to compute it for any MCTQ version.
#'
#' @return A `hms` object corresponding to the sum between `so` and (`sd` / 2)
#'   rolled on a 24-hour clock basis.
#'
#' @aliases msw msf
#' @template mctq_b
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' ms(hms::parse_hms("23:30:00"), lubridate::dhours(8))
#' #> 03:30:00 # Expected
#' ms(hms::parse_hms("01:00:00"), lubridate::dhours(10))
#' #> 06:00:00 # Expected
#' ms(hms::as_hms(NA), lubridate::dhours(7.5))
#' #> NA # Expected
#'
#' ## __ Vectorized example __
#' so <- c(hms::parse_hms("00:10:00"), hms::parse_hms("01:15:00"))
#' sd <- c(lubridate::dhours(9.25), lubridate::dhours(5.45))
#' ms(so, sd)
#' #> [1] 04:47:30 # Expected
#' #> [1] 03:58:30 # Expected
ms <- function(so, sd) {

    checkmate::assert_class(so, "hms")
    assert_duration(sd)
    assert_identical(so, sd, type = "length")

    sum_time(so, (sd / 2), class = "hms", clock = TRUE, vectorize = TRUE)

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
#' guidelines for `sd_week()` (\eqn{SDweek}) computation are as follow.
#'
#' __\deqn{((SDw x WD) + (SDf x FD)) / 7}__
#'
#' Where:
#'
#' * \eqn{SDw} = sleep duration on workdays.
#' * \eqn{WD} = number of workdays per week.
#' * \eqn{SDf} = sleep duration on work-free days.
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
#' @template mctq_d
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
#' Please check Operation section if you have any questions about `sd_overall()`
#' operation.
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
#' Note that the overall sleep duration is the weighted average of the
#' shift-specific mean sleep durations, _i.e._ \eqn{(SDwMEN * nwMEN)} and
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
#'   after each shift__ from a MCTQ shift questionnaire. `sd` elements and
#'   values must be paired with `n` elements and values.
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
#' ## __ Checking second output from vectorized example __
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

#' Compute MCTQ chronotype or corrected midsleep on work-free days
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `msf_sc()` computes the __chronotype or corrected midsleep on free days__ for
#' standard, micro, and shift versions of the Munich Chronotype Questionnaire
#' (MCTQ).
#'
#' `chronotype()` is just a wrapper for `msf_sc()`.
#'
#' Note that, for epidemiological and genetic studies, `msf_sc` must be
#' normalized for age and sex to make populations of different age and sex
#' compositions comparable (Roenneberg, Allebrandt, Merrow, & Vetter,
#' [2012](http://bit.ly/3iGEgqX)).
#'
#' When using the shift version of the MCTQ, replace the value of `sd_week` to
#' `sd_overall`, as instructed in the Arguments section.
#'
#' Also note that the basis for estimating chronotype in shift-workers is the
#' mid-sleep time on free days after evening shifts (\eqn{MSF_E}). In case work
#' schedules do not comprise evening shifts, Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)) propose to derive it from the corrected
#' midsleep time on free days of other shifts (_e.g._ by using a linear model).
#' Unfortunately the `mctq` package can't help you with that, as it requires a
#' closer look at your data.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)), Ghotbi _et.al_
#' ([2020](https://bit.ly/34VhA0l)), Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)), and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `msf_sc()` (\eqn{MSFsc}) computation are as follow.
#'
#' Note that, for all cases, \eqn{MSFsc} cannot be computed if the participant
#' wake up with an alarm clock on free days.
#'
#' ### For standard and micro versions of the MCTQ:
#'
#' __\deqn{If SDf <= SDw: MSF}__
#' __\deqn{If SDf > SDw: MSF - (SDf - SDweek) / 2}__
#'
#' Where:
#'
#' * \eqn{MSF} = mid-sleep on work-free days.
#' * \eqn{SDw} = sleep duration on work days.
#' * \eqn{SDf} = sleep duration on work-free days.
#' * \eqn{SDweek} = average weekly sleep duration.
#'
#' \eqn{MSFsc} is the participant chronotype in standard and micro versions of
#' the MCTQ.
#'
#' ### For the shift version of the MCTQ:
#'
#' __\deqn{If SDf <= SDw: MSF}__
#' __\deqn{If SDf > SDw: MSF - (SDf - OSD) / 2}__
#'
#' Where:
#'
#' * \eqn{MSF} = mid-sleep on free days after shift.
#' * \eqn{SDw} = sleep duration on shift.
#' * \eqn{SDf} = sleep duration on free days after shift.
#' * \eqn{OSD} = overall sleep duration.
#'
#' Note that the basis for estimating chronotype in shift-workers is the
#' mid-sleep time on free days after evening shifts (\eqn{MSF_E}). In case work
#' schedules do not comprise evening shifts, Juda, Vetter, & Roenneberg
#' ([2013](https://bit.ly/38IEEk4)) propose to derive it from the corrected
#' midsleep time on free days of other shifts (_e.g._ by using a linear model).
#' Unfortunately the `mctq` package can't help you with that, as it requires a
#' closer look at your data.
#'
#' @param msf A `hms` object corresponding to the __mid-sleep on work-free
#'   days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire (you can use [mctq::ms()] to compute it).
#' @param sd_w A `Duration` object corresponding to the __sleep duration on work
#'   days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire (you can use [mctq::sd()] to compute it).
#' @param sd_f A `Duration` object corresponding to the __sleep duration on
#'   work-free days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire (you can use [mctq::sd()] to compute it).
#' @param sd_week A `Duration` object corresponding to the __average weekly
#'   sleep duration__ value from a standard or micro version of the MCTQ
#'   questionnaire (you can use [mctq::sd_week()] to compute it) __or__ the
#'   __overall sleep duration__ value from a MCTQ shift (you can use
#'   [mctq::sd_overall()] to compute it).
#' @param alarm_f A `logical` object corresponding to the __alarm clock use on
#'   work-free days__ value from a standard, micro, or shift version of the MCTQ
#'   questionnaire. Note that, if `alarm_f == TRUE`, `msf_sc` cannot be
#'   computed. `msf_sc()` will return `NA` for those cases.
#'
#' @return A `hms` object corresponding to the chronotype or corrected midsleep
#'   on free days.
#'
#' @template mctq_b
#' @template mctq_d
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' msf_sc(hms::parse_hms("04:00:00"), lubridate::dhours(6),
#'        lubridate::dhours(7), lubridate::dhours(6.29), FALSE)
#' #> 03:38:42 # Expected
#' msf_sc(hms::parse_hms("06:30:00"), lubridate::dhours(7.5),
#'        lubridate::dhours(7.5), lubridate::dhours(7.5), FALSE)
#' #> 06:30:00 # Expected
#' msf_sc(hms::parse_hm("01:00:00"), lubridate::dhours(5.5),
#'        lubridate::dhours(9), lubridate::dhours(6.75), FALSE)
#' #> 23:52:30 # Expected
#' msf_sc(hms::parse_hms("05:40:00"), lubridate::dhours(7.5),
#'        lubridate::dhours(10), lubridate::dhours(8.5), TRUE)
#' #> NA # Expected (chronotype cannot be computed if `alarm_w == TRUE`)
#'
#' ## __ Vectorized example __
#' msf <- c(hms::parse_hms("03:45:00"), hms::parse_hm("04:45:00"))
#' sd_w <- c(lubridate::dhours(5), lubridate::dhours(6.45))
#' sd_f <- c(lubridate::dhours(9), lubridate::dhours(10))
#' sd_week <- c(lubridate::dhours(8.5), lubridate::dhours(9.2))
#' alarm_f <- c(FALSE, FALSE)
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 03:30:00 # Expected
#' #> 04:21:00 # Expected
#'
#' ## __ A wrapper for msf_sc() __
#' chronotype(hms::parse_hms("07:00:00"), lubridate::dhours(6),
#'            lubridate::dhours(12), lubridate::dhours(9.45), FALSE)
#' #> 05:43:30 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- msf_sc(hms::parse_hms("05:40:00"), lubridate::dhours(5.43678),
#'             lubridate::dhours(9.345111), lubridate::dhours(7.5453), FALSE)
#' x
#' #> 04:46:02.340202 # Expected
#' round_time(x)
#' #> 04:46:02 # Expected
msf_sc <- function(msf, sd_w, sd_f, sd_week, alarm_f) {

    checkmate::assert_class(msf, "hms")
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_duration(sd_week)
    checkmate::assert_logical(alarm_f)
    assert_identical(msf, sd_w, sd_f, sd_week, alarm_f, type = "length")

    ## `sc` exists to remove unnecessary warnings of lubridate package when
    ## subtracting objects of class `Duration`.

    sc <- sum_time(sd_f, - sd_week, class = "Duration", vectorize = TRUE)
    sc <- sc / 2

    dplyr::case_when(
        alarm_f == TRUE ~ hms::as_hms(NA),
        sd_f <= sd_w ~ msf,
        sd_f > sd_w ~ sum_time(msf, - sc, class = "hms",
                               clock = TRUE, vectorize = TRUE)
    )

}

#' @rdname msf_sc
#' @export
chronotype <- function(msf, sd_w, sd_f, sd_week, alarm_f) {

    msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)

}

#' Compute MCTQ weekly sleep loss
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sloss_week()` computes the __weekly sleep loss__ for the standard and micro
#' versions of the Munich Chronotype Questionnaire (MCTQ).
#'
#' Please note that this function is not appropriated for use with he MCTQ
#' Shift.
#'
#' @section Guidelines:
#'
#' For reference, Roenneberg, Allebrandt, Merrow, & Vetter
#' ([2012](http://bit.ly/3iGEgqX)) and theWeP [(n.d.)](http://bit.ly/3pv8EH1)
#' guidelines for `sloss_week()` (\eqn{SLossweek}) computation are as follow.
#'
#' __\deqn{If SDweek > SDw: (SDweek - SDw) * WD}__
#' __\deqn{If SDweek <= SDw: (SDweek - SDf) * (7 - WD)}__
#'
#' Where:
#'
#' * \eqn{WD} = number of workdays per week.
#' * \eqn{SDw} = sleep duration on workdays.
#' * \eqn{SDf} = sleep duration on work-free days.
#' * \eqn{SDweek} = average weekly sleep duration.
#'
#' @return A `Duration` object corresponding to the weekly sleep loss.
#'
#' @inheritParams sd_week
#' @template mctq_b
#' @template mctq_d
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' sloss_week(4, lubridate::dhours(6.5), lubridate::dhours(7))
#' #> [1] "3085.71428571429s (~51.43 minutes)" # Expected
#' sloss_week(5, lubridate::dhours(7), lubridate::dhours(8))
#' #> [1] "5142.85714285714s (~1.43 hours)" # Expected
#' sloss_week(7, lubridate::dhours(NA), lubridate::dhours(9.45))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' wd <- c(2, 0)
#' sd_w <- c(lubridate::dhours(7), lubridate::dhours(8))
#' sd_f <- c(lubridate::dhours(6.5), lubridate::dhours(8))
#' sloss_week(wd, sd_w, sd_f)
#' #> [1] "2571.42857142857s (~42.86 minutes)" # Expected
#' #> [2] "0s"
#'
#' ## __ Converting the output to `hms` __
#' x <- sloss_week(3, lubridate::dhours(4), lubridate::dhours(5))
#' convert_to(x, "hms")
#' #> 01:42:51.428571 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- sloss_week(6, lubridate::dhours(5.8743), lubridate::dhours(7.4324))
#' x
#' #> [1] "4807.85142857144s (~1.34 hours)" # Expected
#' round_time(x)
#' #> [1] "4808s (~1.34 hours)" # Expected
sloss_week <- function(wd, sd_w, sd_f) {

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_identical(wd, sd_w, sd_f, type = "length")

    sd_week <- sd_week(wd, sd_w, sd_f)

    dplyr::case_when(
        sd_week > sd_w ~ (sd_week - sd_w) * wd,
        sd_week <= sd_w ~ (sd_week - sd_f) * fd(wd)
    )

}

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

#' Compute MCTQ average weekly light exposure
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `le_week()` computes the __average weekly light exposure__ for the standard
#' Munich Chronotype Questionnaire (MCTQ).
#'
#' @param le_w A `Duration` object corresponding to the __light exposure on work
#'   days__ of a standard MCTQ questionnaire.
#' @param le_f A `Duration` object corresponding to the __light exposure on
#'   work-free days__ of a standard MCTQ questionnaire.
#'
#' @return A `Duration` object corresponding to the average weekly light
#'   exposure.
#'
#' @inheritParams fd
#' @template mctq_b
#' @template mctq_d
#' @template references_a
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' le_week(5, lubridate::dhours(1.5), lubridate::dhours(3.7))
#' #> [1] "7662.85714285714s (~2.13 hours)" # Expected
#' le_week(6, lubridate::dhours(3), lubridate::dhours(1.5))
#' #> [1] "10028.5714285714s (~2.79 hours)" # Expected
#' le_week(3, lubridate::dhours(5.6), lubridate::as.duration(NA))
#' #> [1] NA # Expected
#'
#' ## __ Vectorized example __
#' wd <- c(4, 5)
#' le_w <- c(lubridate::dhours(3), lubridate::dhours(2.45))
#' le_f <- c(lubridate::dhours(3), lubridate::dhours(3.75))
#' le_week(wd, le_w, le_f)
#' #> [1] "10800s (~3 hours)" # Expected
#' #> [2] "10157.1428571429s (~2.82 hours)" # Expected
#'
#' ## __ Checking second output from vectorized example __
#' i <- 2
#' x <- c(le_w[i], le_f[i])
#' w <- c(wd[i], fd(wd[i]))
#' lubridate::as.duration(stats::weighted.mean(x, w))
#' #> [1] "10157.1428571429s (~2.82 hours)" # Expected
#'
#' ## __ Converting the output to `hms` __
#' x <- le_week(3, lubridate::dhours(1.25), lubridate::dhours(6.23))
#' convert_to(x, "hms")
#' #> 04:05:44.571429 # Expected
#' convert_to(as.integer(x), "hms") # if you want to discard the milliseconds.
#' #> 04:05:44 # Expected
#'
#' ## __ Rounding the output at the seconds level __
#' x <- le_week(2, lubridate::dhours(3.4094), lubridate::dhours(6.2345))
#' x
#' #> [1] "19538.3828571429s (~5.43 hours)" # Expected
#' round_time(x)
#' #> [1] "19538s (~5.43 hours)" # Expected
le_week <- function(wd, le_w, le_f) {

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(le_w)
    assert_duration(le_f)
    assert_identical(wd, le_w, le_f, type = "length")

    ((le_w * wd) + (le_f * fd(wd))) / 7

}
