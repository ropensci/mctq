#' Compute MCTQ free days
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `fd()` computes the __number of work-free days per week__ for the standard
#' Munich Chronotype Questionnaire (MCTQ).
#'
#' @details
#'
#' This function was created according to Roenneberg, Wirz-Justice & Merrow
#' ([2003](https://bit.ly/3rLu195)) and guidelines of The World Wide
#' Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)).
#'
#' @param wd A integerish number corresponding to the __number of work days per
#'   week__ of a standard MCTQ questionnaire.
#'
#' @return A numeric value equivalent to 7 - `wd`, _i.e._ the difference between
#'   the number of days in a week and the number of work days.
#'
#' @family standard MCTQ functions
#' @inherit so references
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
#' `so()` computes the __time of sleep onset__ for the standard Munich
#' Chronotype Questionnaire (MCTQ).
#'
#' @details
#'
#' This function was created according to Roenneberg, Wirz-Justice & Merrow
#' ([2003](https://bit.ly/3rLu195)) and guidelines of The World Wide
#' Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)).
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. This classes can be found on [hms::hms-package] and
#' [lubridate::lubridate-package]. If your data do not conform to the object
#' classes required, you can use [mctq::convert_to()] to convert it.
#'
#' ## Work days and work-free days
#'
#' The computation is the same for work and work-free days. Use the appropriated
#' value entered on the section you are working on.
#'
#' @param sprep A `hms` vector corresponding to the __local time of getting out
#'   of bed__ of a standard MCTQ questionnaire.
#' @param slat A `Duration` vector corresponding to the __sleep latency__ of
#'   standard a MCTQ questionnaire.
#'
#' @return A `hms` vector corresponding to the sum of `sprep` and `slat` rolled
#'   on a 24 hours clock basis.
#'
#' @family standard MCTQ functions
#' @export
#'
#' @references
#'
#' Roenneberg, T., Wirz-Justice, A., & Merrow, M. (2003). Life between clocks:
#' daily temporal patterns of human chronotypes.
#' _Journal of Biological Rhythms_, _18_(1), 80-90. doi:
#' [10.1177/0748730402239679](https://doi.org/10.1177/0748730402239679).
#'
#' The Worldwide Experimental Platform (n.d.). MCTQ. Retrieved from
#' <https://www.thewep.org/documentations/mctq/>.
#'
#' @examples
#' so(hms::parse_hms("22:00:00"), lubridate::dminutes(15))
#' #> 22:15:00 # Expected
#' so(hms::parse_hms("23:30:00"), lubridate::dminutes(45))
#' #> 00:15:00 # Expected
#' so(hms::parse_hms("20:45:00"), lubridate::as.duration(NA))
#' #> NA # Expected
#'
#' sprep <- c(hms::parse_hms("21:30:00"), hms::parse_hms("22:15:00"))
#' slat <- c(lubridate::dminutes(45), lubridate::dminutes(5))
#' so(sprep, slat)
#' #> 22:15:00 # Expected
#' #> 22:20:00 # Expected
so <- function(sprep, slat){

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
#' `gu()` computes the __local time of getting out of bed__ for the standard
#' Munich Chronotype Questionnaire (MCTQ).
#'
#' @param se A `hms` vector corresponding to the __sleep end__ of a standard
#'   MCTQ questionnaire.
#' @param si A `Duration` vector corresponding to the __sleep inertia__ of
#'   standard a MCTQ questionnaire.
#'
#' @return A `hms` vector corresponding to the sum of `se` and `si` rolled on a
#'   24 hours clock basis.
#'
#' @family standard MCTQ functions
#' @inherit so details references
#' @export
#'
#' @examples
#' gu(hms::parse_hms("08:00:00"), lubridate::dminutes(10))
#' #> 08:10:00 # Expected
#' gu(hms::parse_hms("11:45:00"), lubridate::dminutes(90))
#' #> 13:15:00 # Expected
#' gu(hms::as_hms(NA), lubridate::dminutes(90))
#' #> NA # Expected
#'
#' se <- c(hms::parse_hms("12:30:00"), hms::parse_hms("06:40:00"))
#' si <- c(lubridate::dminutes(10), lubridate::dminutes(10))
#' gu(se, si)
#' #> 12:40:00 # Expected
#' #> 06:50:00 # Expected
gu <- function(se, si){

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
#' `sd()` computes the __sleep duration__ for the standard Munich Chronotype
#' Questionnaire (MCTQ).
#'
#' @param so A `hms` vector corresponding to the __sleep onset__ of a standard
#'   MCTQ questionnaire (you can use [mctq::so()] to compute it).
#' @param se A `hms` vector corresponding to the __sleep end__ of a standard
#'   MCTQ questionnaire.
#'
#' @return A `Duration` vector corresponding to the difference between `se` and
#'   `so` rolled on a 24 hours clock basis.
#'
#' @family standard MCTQ functions
#' @inherit so details references
#' @export
#'
#' @examples
#' sd(hms::parse_hms("23:00:00"), hms::parse_hms("08:00:00"))
#' #> [1] "32400s (~9 hours)" # Expected
#' sd(hms::parse_hms("02:00:00"), hms::parse_hms("12:30:00"))
#' #> [1] "37800s (~10.5 hours)" # Expected
#' sd(hms::parse_hms("03:15:00"), hms::as_hms(NA))
#' #> NA # Expected
#'
#' so <- c(hms::parse_hms("04:12:00"), hms::parse_hms("21:20:00"))
#' se <- c(hms::parse_hms("14:30:00"), hms::parse_hms("03:45:00"))
#' sd(so, se)
#' #> [1] "37080s (~10.3 hours)" "23100s (~6.42 hours)" # Expected
sd <- function(so, se){

    checkmate::assert_class(so, "hms")
    checkmate::assert_class(se, "hms")
    assert_identical(so, se, type = "length")

    sum_time(se, - so, class = "Duration", clock = TRUE, vectorize = TRUE)

}

#' Compute MCTQ total time in bed
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tbt()` computes the __total time in bed__ for the standard Munich Chronotype
#' Questionnaire (MCTQ).
#'
#' @param bt A `hms` vector corresponding to the __local time of going to bed__
#'   of a standard MCTQ questionnaire.
#' @param gu A `hms` vector corresponding to the __local time of getting out of
#'   bed__ of a standard MCTQ questionnaire (you can use [mctq::gu()] to compute
#'   it).
#'
#' @return A `hms` vector corresponding to the difference between `gu` and `bt`
#'   rolled on a 24 hours clock basis.
#'
#' @family standard MCTQ functions
#' @inherit so details references
#' @export
#'
#' @examples
#' tbt(hms::parse_hms("22:10:00"), hms::parse_hms("06:15:00"))
#' #> 08:05:00 # Expected
#' tbt(hms::parse_hms("01:20:00"), hms::parse_hms("14:00:00"))
#' #> 12:40:00 # Expected
#' tbt(hms::as_hms(NA), hms::parse_hms("07:20:00"))
#' #> NA # Expected
#'
#' bt <- c(hms::parse_hms("23:50:00"), hms::parse_hms("02:30:00"))
#' gu <- c(hms::parse_hms("09:30:00"), hms::parse_hms("11:25:00"))
#' tbt(bt, gu)
#' #> [1] 09:40:00 # Expected
#' #> [1] 08:55:00 # Expected
tbt <- function(bt, gu){

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
#' `ms()` computes the __mid-sleep__ for the standard Munich Chronotype
#' Questionnaire (MCTQ).
#'
#' @param so A `hms` vector corresponding to the __sleep onset__ of a standard
#'   MCTQ questionnaire (you can use [mctq::so()] to compute it).
#' @param sd A `Duration` vector corresponding to the __sleep duration__ of a
#'   standard MCTQ questionnaire (you can use [mctq::sd()] to compute it).
#'
#' @return A `hms` vector corresponding to the sum between `so` and (`sd` / 2)
#'   rolled on a 24 hours clock basis.
#'
#' @family standard MCTQ functions
#' @inherit so details references
#' @export
#'
#' @examples
#' ms(hms::parse_hms("23:30:00"), lubridate::dhours(8))
#' #> 03:30:00 # Expected
#' ms(hms::parse_hms("01:00:00"), lubridate::dhours(10))
#' #> 06:00:00 # Expected
#' ms(hms::as_hms(NA), lubridate::dhours(7.5))
#' #> NA # Expected
#'
#' so <- c(hms::parse_hms("00:10:00"), hms::parse_hms("01:15:00"))
#' sd <- c(lubridate::dhours(9.25), lubridate::dhours(5.45))
#' ms(so, sd)
#' #> [1] 04:47:30 # Expected
#' #> [1] 03:58:30 # Expected
ms <- function(so, sd){

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
#' Munich Chronotype Questionnaire (MCTQ).
#'
#' @details
#'
#' This function was created according to Roenneberg, Wirz-Justice & Merrow
#' ([2003](https://bit.ly/3rLu195)) and guidelines of The World Wide
#' Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)).
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. This classes can be found on [hms::hms-package] and
#' [lubridate::lubridate-package]. If your data do not conform to the object
#' classes required, you can use [mctq::convert_to()] to convert it.
#'
#' @param sd_w A `Duration` vector corresponding to the __sleep duration on work
#'   days__ of a standard MCTQ questionnaire (you can use [mctq::sd()] to
#'   compute it).
#' @param sd_f A `Duration` vector corresponding to the __sleep duration on
#'   work-free days__ of a standard MCTQ questionnaire (you can use [mctq::sd()]
#'   to compute it).
#'
#' @return A `Duration` vector corresponding to the average weekly sleep
#'   duration.
#'
#' @family standard MCTQ functions
#' @inheritParams fd
#' @inherit so references
#' @export
#'
#' @examples
#' sd_week(5, lubridate::dhours(4), lubridate::dhours(8))
#' #> [1] "18514.2857142857s (~5.14 hours)" # Expected
#' sd_week(4, lubridate::dhours(7), lubridate::dhours(7))
#' #> [1] "25200s (~7 hours)" # Expected
#' sd_week(6, lubridate::as.duration(NA), lubridate::dhours(10))
#' #> [1] NA # Expected
#'
#' wd <- c(3, 7)
#' sd_w <- c(lubridate::dhours(4.5), lubridate::dhours(5.45))
#' sd_f <- c(lubridate::dhours(8), lubridate::dhours(7.3))
#' sd_week(wd, sd_w, sd_f)
#' #> [1] "23400s (~6.5 hours)"  "19620s (~5.45 hours)" # Expected
#'
#' ## ** converting the output to hms **
#' x <- sd_week(5, lubridate::dhours(5.45), lubridate::dhours(9.5))
#' convert_to(x, "hms")
#' #> 06:36:25.714286 # Expected
#' convert_to(as.integer(x), "hms") # if you want to discard the milliseconds.
#' #> 06:36:25 # Expected
sd_week <- function(wd, sd_w, sd_f){

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_identical(wd, sd_w, sd_f, type = "length")

    ((sd_w * wd) + (sd_f * fd(wd))) / 7

}

#' Compute MCTQ chronotype/corrected midsleep on work-free days
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `msf_sc()` computes the __chronotype/corrected midsleep on free days__ for
#' the standard Munich Chronotype Questionnaire (MCTQ).
#'
#' `chronotype()` is just a wrapper for `msf_sc()`.
#'
#' @param msf A `hms` vector corresponding to the __mid-sleep on work-free
#'   days__ of a standard MCTQ questionnaire (you can use [mctq::ms()] to
#'   compute it).
#' @param sd_week A `Duration` vector corresponding to the __average weekly
#'   sleep duration__ of a standard MCTQ questionnaire (you can use
#'   [mctq::sd_week()] to compute it).
#' @param alarm_f A `logical` vector corresponding to the __alarm clock use on
#'   work-free days__ of a standard MCTQ questionnaire.
#'
#' @return A `hms` vector corresponding to the chronotype/corrected midsleep on
#'   free days.
#'
#' @family standard MCTQ functions
#' @inheritParams sd_week
#' @inherit sd_week details references
#' @export
#'
#' @examples
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
#' #> NA # Expected
#'
#' msf <- c(hms::parse_hms("03:45:00"), hms::parse_hm("04:45:00"))
#' sd_w <- c(lubridate::dhours(5), lubridate::dhours(6.45))
#' sd_f <- c(lubridate::dhours(9), lubridate::dhours(10))
#' sd_week <- c(lubridate::dhours(8.5), lubridate::dhours(9.2))
#' alarm_f <- c(TRUE, TRUE)
#' msf_sc(msf, sd_w, sd_f, sd_week, alarm_f)
#' #> 03:30:00 # Expected
#' #> 04:21:00 # Expected
#'
#' ## ** wrapper for msf_sc() **
#' chronotype(hms::parse_hms("07:00:00"), lubridate::dhours(6),
#'            lubridate::dhours(12), lubridate::dhours(9.45), FALSE)
#' #> 05:43:30 # Expected
msf_sc <- function(msf, sd_w, sd_f, sd_week, alarm_f){

    checkmate::assert_class(msf, "hms")
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_duration(sd_week)
    checkmate::assert_logical(alarm_f)
    assert_identical(msf, sd_w, sd_f, sd_week, alarm_f, type = "length")

    dplyr::case_when(
        isTRUE(alarm_f) ~ hms::as_hms(NA),
        sd_f <= sd_w ~ msf,
        TRUE ~ sum_time(msf, - ((sd_f - sd_week) / 2), class = "hms",
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
#' `sloss_week()` computes the __weekly sleep loss__ for the standard Munich
#' Chronotype Questionnaire (MCTQ).
#'
#' @return A `Duration` vector corresponding to the weekly sleep loss.
#'
#' @family standard MCTQ functions
#' @inheritParams sd_week
#' @inheritParams msf_sc
#' @inherit sd_week details references
#' @export
#'
#' @examples
#' sloss_week(4, lubridate::dhours(6.5), lubridate::dhours(7),
#'           lubridate::dhours(6.75))
#' #> [1] "3600s (~1 hours)" # Expected
#' sloss_week(5, lubridate::dhours(7), lubridate::dhours(8),
#'           lubridate::dhours(7.45))
#' #> [1] "8100s (~2.25 hours)" # Expected
#' sloss_week(7, lubridate::dhours(4.5), lubridate::dhours(9.45),
#'            lubridate::as.duration(NA))
#' #> [1] NA # Expected
#'
#' wd <- c(2, 0)
#' sd_w <- c(lubridate::dhours(7), lubridate::dhours(8))
#' sd_f <- c(lubridate::dhours(6.5), lubridate::dhours(8))
#' sd_week <- c(lubridate::dhours(6.75), lubridate::dhours(8))
#' sloss_week(wd, sd_w, sd_f, sd_week)
#' #> [1] "4500s (~1.25 hours)" "0s"  # Expected
sloss_week <- function(wd, sd_w, sd_f, sd_week){

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(sd_w)
    assert_duration(sd_f)
    assert_duration(sd_week)
    assert_identical(wd, sd_w, sd_f, sd_week, type = "length")

    dplyr::case_when(
        sd_week > sd_w ~ (sd_week - sd_w) * wd,
        TRUE ~ (sd_week - sd_f) * fd(wd) # sd_week <= sd_w
    )

}

#' Compute MCTQ Social Jet Lag
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sjl()` computes the __relative social jetlag__ or the __absolute social
#' jetlag__ for the standard Munich Chronotype Questionnaire (MCTQ).
#'
#' @details
#'
#' `sjl()` does a slight different computation than those propose by Roenneberg,
#' Wirz-Justice & Merrow ([2003](https://bit.ly/3rLu195)) and the guidelines of
#' The World Wide Experimental Platform (theWeP, [n.d.](http://bit.ly/3pv8EH1)).
#' See `vignette("social_jet_lag_signal", package = "mctq")` for more details.
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. This classes can be found on [hms::hms-package] and
#' [lubridate::lubridate-package]. If your data do not conform to the object
#' classes required, you can use [mctq::convert_to()] to convert it.
#'
#' @param msw A `hms` vector corresponding to the __mid-sleep on work days__ of
#'   a standard MCTQ questionnaire (you can use [mctq::ms()] to compute it).
#' @param abs A `logical` value indicating if the function must return an
#' absolute SJL (always positive) or a relative SJL (the SJL with no changes).
#'
#' @return
#'
#' * If `abs = TRUE`, a `Duration` vector corresponding to the absolute social
#' jetlag.
#'
#' * If `abs = FALSE`, a `Duration` vector corresponding to the relative
#' social jetlag.
#'
#' @family standard MCTQ functions
#' @inheritParams msf_sc
#' @inherit sd_week references
#' @export
#'
#' @examples
#' sjl(hms::parse_hms("03:30:00"), hms::parse_hms("05:00:00"))
#' #> [1] "5400s (~1.5 hours)" # Expected
#' sjl(hms::parse_hms("23:30:00"), hms::parse_hms("04:30:00"), abs = FALSE)
#' #> [1] "18000s (~5 hours)"
#' sjl(hms::parse_hms("01:15:00"), hms::parse_hms("03:45:00"))
#' #> [1] "9000s (~2.5 hours)" # Expected
#' sjl(hms::as_hms(NA), hms::parse_hms("05:15:00"))
#' #> NA # Expected
#'
#' msw <- c(hms::parse_hms("04:05:00"), hms::parse_hms("04:05:00"))
#' msf <- c(hms::parse_hms("03:05:00"), hms::parse_hms("04:05:00"))
#' sjl(msw, msf, abs = FALSE)
#' #> [1] "-3600s (~-1 hours)" "0s" # Expected
#'
#' sjl_rel(hms::parse_hms("05:10:00"), hms::parse_hms("05:05:00"))
#' #> [1] "-300s (~-5 minutes)" # Expected
#'
#' ## ** converting the output to hms **
#' x <- sjl(hms::parse_hms("01:15:00"), hms::parse_hms("03:25:05"))
#' convert_to(x, "hms")
#' #> 02:10:05 # Expected
sjl <- function(msw, msf, abs = TRUE) {

    checkmate::assert_class(msw, "hms")
    checkmate::assert_class(msf, "hms")
    checkmate::assert_flag(abs)

    shortest_interval <- shortest_interval(msw, msf, class = "Interval")
    int_start <- convert_to(lubridate::int_start(shortest_interval), "hms")
    out <- convert_to(shortest_interval, class = "Duration")

    out <- dplyr::case_when(
        msw == msf ~ out,
        int_start == msw ~ out,
        int_start == msf ~ - out
    )

    if (isTRUE(abs)) {
        abs(out)
    } else {
        out
    }

}

#' @rdname sjl
#' @export
sjl_rel <- function(msw, msf){

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
#' @param le_w A `Duration` vector corresponding to the __light exposure on work
#'   days__ of a standard MCTQ questionnaire.
#' @param le_f A `Duration` vector corresponding to the __light exposure on
#'   work-free days__ of a standard MCTQ questionnaire.
#'
#' @return A `Duration` vector corresponding to the average weekly light
#'   exposure.
#'
#' @family standard MCTQ functions
#' @inheritParams fd
#' @inherit sd_week details references
#' @export
#'
#' @examples
#' le_week(5, lubridate::dhours(1.5), lubridate::dhours(3.7))
#' #> [1] "7662.85714285714s (~2.13 hours)" # Expected
#' le_week(6, lubridate::dhours(3), lubridate::dhours(1.5))
#' #> [1] "10028.5714285714s (~2.79 hours)" # Expected
#' le_week(3, lubridate::dhours(5.6), lubridate::as.duration(NA))
#' #> [1] NA # Expected
#'
#' wd <- c(4, 5)
#' le_w <- c(lubridate::dhours(3), lubridate::dhours(2.45))
#' le_f <- c(lubridate::dhours(3), lubridate::dhours(3.75))
#' le_week(wd, le_w, le_f)
#' #> [1] "10800s (~3 hours)" # Expected
#' #> [2] "10157.1428571429s (~2.82 hours)" # Expected
#'
#' ## ** converting the output to hms **
#' x <- le_week(3, lubridate::dhours(1.25), lubridate::dhours(6.23))
#' convert_to(x, "hms")
#' #> 04:05:44.571429 # Expected
#' convert_to(as.integer(x), "hms") # if you want to discard the milliseconds.
#' #> 04:05:44 # Expected
le_week <- function(wd, le_w, le_f){

    checkmate::assert_numeric(wd, lower = 0, upper = 7)
    assert_duration(le_w)
    assert_duration(le_f)
    assert_identical(wd, le_w, le_f, type = "length")

    ((le_w * wd) + (le_f * fd(wd))) / 7

}
