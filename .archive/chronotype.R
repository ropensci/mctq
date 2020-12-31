#' Process the chronotype variable
#'
#' @param data A tibble with the MCTQ valid data.
#' @param time_zone A string with the base timezone.
#' @param dst A logical value indicating if the Day-Light-Saving
#'   time should be considered.
#'
#' @return A tibble.
#' @importFrom magrittr %>%
#' @importFrom rlang := .data is_true
#' @importFrom utils head
#' @export

chronotype <- function(data) {

    # Check values --------------------

    for (i in c("data")) {

        if (!(is.data.frame(get(i)))) {

            stop(paste(i, "is not a dataframe"))

        }

    }

    # Fix date references  --------------------

    output <- data

    output <- output %>% dplyr::mutate(
        bt_w = lubridate::as_datetime(bt_w),
        s_prep_w = lubridate::as_datetime(s_prep_w),
        se_w = lubridate::as_datetime(se_w),
        bt_f = lubridate::as_datetime(bt_f),
        s_prep_f = lubridate::as_datetime(s_prep_f),
        se_f = lubridate::as_datetime(se_f))

    for (i in 1:nrow(output)) {

        if (is.na(output$timestamp[i])) {

            next()

        }

        if (!(is.na(output$bt_w[i])) &&
            !(is.na(output$s_prep_w[i])) &&
            lubridate::hour(output$bt_w[i]) > 12 &&
            lubridate::hour(output$s_prep_w[i]) < 12) {

            output$s_prep_w[i] <- output$s_prep_w[i] + lubridate::ddays(1)

        }

        if (!(is.na(output$bt_f[i])) &&
            !(is.na(output$s_prep_f[i])) &&
            lubridate::hour(output$bt_f[i]) > 12 &&
            lubridate::hour(output$s_prep_f[i]) < 12) {

            output$s_prep_f[i] <- output$s_prep_f[i] + lubridate::ddays(1)

        }

        if (!(is.na(output$s_prep_w[i])) &&
            !(is.na(output$s_lat_w[i])) &&
            !(is.na(output$se_w[i])) &&
            lubridate::hour(output$s_prep_w[i] + output$s_lat_w[i]) < 12 &&
            lubridate::day(output$s_prep_w[i] + output$s_lat_w[i]) !=
            lubridate::day(output$se_w[i])) {

            output$se_w[i] <- output$se_w[i] + lubridate::ddays(1)

        }

        if (!(is.na(output$s_prep_w[i])) &&
            !(is.na(output$s_lat_w[i])) &&
            !(is.na(output$se_w[i])) &&
            lubridate::hour(output$s_prep_w[i] + output$s_lat_w[i]) > 12 &&
            lubridate::day(output$s_prep_w[i] + output$s_lat_w[i]) ==
            lubridate::day(output$se_w[i])) {

            output$se_w[i] <- output$se_w[i] + lubridate::ddays(1)

        }

        if (!(is.na(output$s_prep_f[i])) &&
            !(is.na(output$s_lat_f[i])) &&
            !(is.na(output$se_f[i])) &&
            lubridate::hour(output$s_prep_f[i] + output$s_lat_f[i]) < 12 &&
            lubridate::day(output$s_prep_f[i] + output$s_lat_f[i]) !=
            lubridate::day(output$se_f[i])) {

            output$se_f[i] <- output$se_f[i] + lubridate::ddays(1)

        }

        if (!(is.na(output$s_prep_f[i])) &&
            !(is.na(output$s_lat_f[i])) &&
            !(is.na(output$se_f[i])) &&
            lubridate::hour(output$s_prep_f[i] + output$s_lat_f[i]) > 12 &&
            lubridate::day(output$s_prep_f[i] + output$s_lat_f[i]) ==
            lubridate::day(output$se_f[i])) {

            output$se_f[i] <- output$se_f[i] + lubridate::ddays(1)

        }

    }

    # Create new MCTQ variables --------------------

    ## Create variables

    output[, "fd"] <- NA

    output[, "so_w"] <- NA
    output[, "sd_w"] <- NA
    output[, "gu_w"] <- NA
    output[, "tbt_w"] <- NA
    output[, "msw"] <- NA

    output[, "so_f"] <- NA
    output[, "sd_f"] <- NA
    output[, "gu_f"] <- NA
    output[, "tbt_f"] <- NA
    output[, "msf"] <- NA

    output[, "sd_week"] <- NA
    output[, "msf_sc"] <- NA
    output[, "msf_sc_bin_1"] <- NA
    output[, "msf_sc_bin_2"] <- NA
    output[, "sloss_week"] <- NA
    output[, "sjl_rel"] <- NA
    output[, "sjl"] <- NA
    output[, "le_week"] <- NA

    ## Assign classes to variables

    output <- output %>%
        dplyr::mutate(fd = as.integer(fd),
                      so_w = as.POSIXct.numeric(output$so_w,
                                                origin = origin),
                      so_f = as.POSIXct.numeric(output$so_f,
                                                origin = origin),
                      sd_w = lubridate::as.duration(sd_w),
                      sd_f = lubridate::as.duration(sd_f),
                      gu_w = as.POSIXct.numeric(output$gu_w,
                                                origin = origin),
                      gu_f = as.POSIXct.numeric(output$gu_f,
                                                origin = origin),
                      tbt_w = lubridate::as.duration(tbt_w),
                      tbt_f = lubridate::as.duration(tbt_f),
                      msw = as.POSIXct.numeric(output$msw,
                                               origin = origin),
                      msf = as.POSIXct.numeric(output$msf,
                                               origin = origin),
                      sd_week = lubridate::as.duration(sd_week),
                      msf_sc = as.POSIXct.numeric(output$msf_sc,
                                                  origin = origin),
                      msf_sc_bin_1 = as.character(msf_sc_bin_1),
                      msf_sc_bin_2 = as.character(msf_sc_bin_2),
                      sloss_week = lubridate::as.duration(sloss_week),
                      sjl_rel = lubridate::as.duration(sjl_rel),
                      sjl = lubridate::as.duration(sjl),
                      le_week = lubridate::as.duration(le_week))

    # Reorder dataset --------------------

    output <- output %>%
        dplyr::select(mctq_id:wd, fd, bt_w:s_lat_w, so_w,
                      se_w:wake_before_alarm_w, sd_w, gu_w, tbt_w,
                      msw, le_w,bt_f:s_lat_f, so_f,
                      se_f:reasons_f, sd_f, gu_f,
                      tbt_f, msf, le_f, sd_week, msf_sc,
                      msf_sc_bin_1, msf_sc_bin_2,
                      sloss_week, sjl_rel, sjl, le_week)

    # Compute new MCTQ variables --------------------

    for (i in 1:nrow(output)) {

        ## Skip blank cases

        if (is.na(output$timestamp[i])) {

            next()

        }

        ## fd

        if (is.na(output$wd[i])) {

            output$fd[i] <- 7

        } else {

            output$fd[i] <- 7 - output$wd[i]

        }

        ## so_w

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$s_prep_w[i])) &&
            !(is.na(output$s_lat_w[i]))) {

            output$so_w[i] <- output$s_prep_w[i] + output$s_lat_w[i]

        }

        ## so_f

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$s_prep_f[i])) &&
            !(is.na(output$s_lat_f[i]))) {

            output$so_f[i] <- output$s_prep_f[i] + output$s_lat_f[i]

        }

        ## sd_w

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$se_w[i])) &&
            !(is.na(output$so_w[i]))) {

            output$sd_w[i] <- lubridate::as.duration(
                output$se_w[i] - output$so_w[i])

        }

        ## sd_f

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$se_f[i])) &&
            !(is.na(output$so_f[i]))) {

            output$sd_f[i] <- lubridate::as.duration(
                output$se_f[i] - output$so_f[i])

        }

        ## gu_w

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$se_w[i])) &&
            !(is.na(output$si_w[i]))) {

            output$gu_w[i] <- output$se_w[i] + output$si_w[i]

        }

        ## gu_f

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$se_f[i])) &&
            !(is.na(output$si_f[i]))) {

            output$gu_f[i] <- output$se_f[i] + output$si_f[i]

        }

        ## tbt_w

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$gu_w[i])) &&
            !(is.na(output$bt_w[i]))) {

            output$tbt_w[i] <- lubridate::as.duration(
                output$gu_w[i] - output$bt_w[i])

        }

        ## tbt_f

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$gu_f[i])) &&
            !(is.na(output$bt_f[i]))) {

            output$tbt_f[i] <- lubridate::as.duration(
                output$gu_f[i] - output$bt_f[i])

        }

        ## msw

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$so_w[i])) &&
            !(is.na(output$sd_w[i]))) {

            output$msw[i] <- output$so_w[i] + (output$sd_w[i] / 2)

        }

        ## msf

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$so_f[i])) &&
            !(is.na(output$sd_f[i]))) {

            output$msf[i] <- output$so_f[i] + (output$sd_f[i] / 2)

        }

        ## sd_week

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$fd[i])) &&
            !(is.na(output$sd_w[i])) &&
            !(is.na(output$sd_f[i]))) {

            output$sd_week[i] <- ((output$sd_w[i] * output$wd[i]) +
                                      (output$sd_f[i] * output$fd[i])) / 7

        }

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$sd_f[i]))) {

            if (output$fd[i] == 7) {

                output$sd_week[i] <- output$sd_f[i]

            }

        }

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$sd_w[i]))) {

            if (output$wd[i] == 7) {

                output$sd_week[i] <- output$sd_w[i]

            }

        }

        ## msf_sc

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$fd[i])) &&
            !(is.na(output$sd_w[i])) &&
            !(is.na(output$sd_f[i]))) {

            if (output$sd_f[i] <= output$sd_w[i]) {

                output$msf_sc[i] <- output$msf[i]
                lubridate::day(output$msf_sc[i]) <- 1

            }

        }

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$fd[i])) &&
            !(is.na(output$sd_w[i])) &&
            !(is.na(output$sd_f[i])) &&
            !(is.na(output$sd_week[i]))) {

            if (output$sd_f[i] > output$sd_w[i]) {

                output$msf_sc[i] <- output$msf[i] -
                    ((output$sd_f[i] - output$sd_week[i]) / 2)
                lubridate::day(output$msf_sc[i]) <- 1

            }

        }

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$sd_w[i]))) {

            if (output$fd[i] == 7) {

                output$msf_sc[i] <- output$msf[i]
                lubridate::day(output$msf_sc[i]) <- 1

            }

        }

        if (!(is.na(output$alarm_f[i]))) {

            if (output$alarm_f[i] == "YES") {

                output$msf_sc[i] <- NA

            }
        }

        ## msf_sc_bin_1

        if (!(is.na(output$msf_sc[i]))) {

            time_ref <- as.numeric(
                output$msf_sc[i] - lubridate::ymd_hms("1970-01-01 00:00:00",
                                                      tz = time_zone,
                                                      quiet = TRUE))

            if (time_ref < 3.2123) {

                output$msf_sc_bin_1[i] <- "DEFINITELY MORNING TYPE"

            } else if (time_ref >= 3.2123 &&
                       time_ref < 4.4351) {

                output$msf_sc_bin_1[i] <- "MODERATELY MORNING TYPE"

            } else if (time_ref >= 4.4351 &&
                       time_ref < 5.0465) {

                output$msf_sc_bin_1[i] <- "SLIGHTLY MORNING TYPE"

            } else if (time_ref >= 5.0465 &&
                       time_ref < 5.6578) {

                output$msf_sc_bin_1[i] <- "INTERMEDIATE TYPE"

            } else if (time_ref >= 5.6578 &&
                       time_ref < 6.8807) {

                output$msf_sc_bin_1[i] <- "SLIGHTLY EVENING TYPE"

            } else if (time_ref >= 6.8807 &&
                       time_ref < 11.7718) {

                output$msf_sc_bin_1[i] <- "MODERATELY EVENING TYPE"

            } else if (time_ref >= 11.7718) {

                output$msf_sc_bin_1[i] <- "DEFINITELY EVENING TYPE"

            }

        }

        ## msf_sc_bin_2

        if (!(is.na(output$msf_sc[i]))) {

            time_ref <- as.numeric(
                output$msf_sc[i] - lubridate::ymd_hms("1970-01-01 00:00:00",
                                                      tz = time_zone,
                                                      quiet = TRUE))

            if (time_ref < 5.0465) {

                output$msf_sc_bin_2[i] <- "MORNING TYPE"

            } else if (time_ref >= 5.0465 &&
                       time_ref < 5.6578) {

                output$msf_sc_bin_2[i] <- "INTERMEDIATE TYPE"

            } else if (time_ref >= 5.6578) {

                output$msf_sc_bin_2[i] <- "EVENING TYPE"

            }

        }

        ## sloss_week

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$sd_w[i])) &&
            !(is.na(output$sd_week[i]))) {

            if (output$sd_week[i] > output$sd_w[i]) {

                output$sloss_week[i] <- (output$sd_week[i] -
                                             output$sd_w[i]) * output$wd[i]

            }

        }

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$fd[i])) &&
            !(is.na(output$sd_w[i])) &&
            !(is.na(output$sd_f[i])) &&
            !(is.na(output$sd_week[i]))) {

            if (output$sd_week[i] <= output$sd_w[i]) {

                output$sloss_week[i] <- (output$sd_week[i] -
                                             output$sd_f[i]) * output$fd[i]

            }

        }

        if (!(is.na(output$fd[i]))) {

            if (output$fd[i] == 7) {

                output$sloss_week[i] <- lubridate::duration(0, "seconds")

            }

        }

        if (!(is.na(output$wd[i]))) {

            if (output$wd[i] == 7) {

                output$sloss_week[i] <- NA

            }

        }

        ## sjl_rel - Value

        if (is.na(output$msf[i])) {

            output$sjl_rel[i] <- NA

        }

        if (!(is.na(output$fd[i]))) {

            if (output$fd[i] == 7) {

                output$sjl_rel[i] <- lubridate::duration(0, "seconds")

            }
        }

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$wd[i])) &&
            !(is.na(output$msw[i])) &&
            !(is.na(output$msf[i]))) {

            msw_ref <- output$msw[i]
            lubridate::day(msw_ref) <- 1
            msf_ref <- output$msf[i]
            lubridate::day(msf_ref) <- 1

            date_time_ref_1 <- abs(lubridate::as.duration(msf_ref - msw_ref))
            lubridate::day(msf_ref) <- 2
            date_time_ref_2 <- abs(lubridate::as.duration(msf_ref - msw_ref))
            lubridate::day(msf_ref) <- 1
            lubridate::day(msw_ref) <- 2
            date_time_ref_3 <- abs(lubridate::as.duration(msf_ref - msw_ref))

            output$sjl_rel[i] <- lubridate::as.duration(min(c(date_time_ref_1,
                                                              date_time_ref_2,
                                                              date_time_ref_3)))

        }

        ## sjl_rel - Signal

        ##' THE PROBLEM: The sjlrel's signal is computed by the difference
        ##' between the mid-sleep hour on work-free days (msf) and the
        ##' mid-sleep hour on workdays (msw) (msf - msw). In practice, the
        ##' signal says whether the individual's mid-sleep hour on workdays
        ##' (msw), relative to his or her mid-sleep hour on work-free days
        ##' (msf), is given sooner or later (it's a difference between two
        ##' hours, not a difference betweem sleep durations). If msw happens
        ##' sooner than msw, than sjlrel is presented as positive, else,
        ##' negative. The tricky part is to know when is sooner and when is
        ##' later. Example: When msw is 01:00 and msf is 13:00, which one is
        ##' sooner and which one is later? The routines below explain one way
        ##' to deal with this problem.
        ##'
        ##' NOTE: This solution disregards cases (rare cases -> shift-work
        ##' cases) in which the difference between msf and msw occurs beyond
        ##' 12 hours. Unfortunately, there is no way to know if this really
        ##' occurs only using the information avaliable.

        ##' CONDITION 1:
        ##'
        ##' 0.5 = 12:00 in day-fraction (12/24), that is the maximum smallest
        ##' difference between two hours, as shown below.
        ##'
        ##' In those cases when the smallest difference between msf and msw is
        ##' exactly 12h, the signal of the sjl_rel variable will be presented as
        ##' positive. This cases are rare. The signal is presented that way
        ##' because there's no way to tell which variable (msf or msw) is
        ##' sooner or later relative to the other.
        ##'
        ##' 	     SMALLEST DIFFERENCE
        ##'    15:00 |------ 9h -----| 0:00 |--------- 15h ----------| 15:00
        ##' -----|----------------------|--------------------------------|---->
        ##'     msw                    msf                               msw
        ##'
        ##' 	                                       SMALLEST DIFFERENCE
        ##'     0:00 |--------- 15h ----------| 15:00 |----- 9h ------| 0:00
        ##' -----|--------------------------------|----------------------|---->
        ##'     msf                              msw                    msf

        if (!(is.na(output$sjl_rel[i]))) {

            if (output$sjl_rel[i] == lubridate::duration(12, "hours")) {

                output$sjl_rel[i] <- output$sjl_rel[i]

                ##' CONDITION 2:
                ##'
                ##' If the smallest difference between msf and msw is 0 (00:00 -> 0/24
                ##' in day-fraction), the signal of the variable sjlrel will be
                ##' presented as positive.

            } else if (output$sjl_rel[i] == lubridate::duration(0, "hours")) {

                output$sjl_rel[i] <- output$sjl_rel[i]

                ##' CONDITION 3:
                ##'
                ##' This condition below refers to the situation below:
                ##'
                ##'     23:00 |------ 3h -------| 02:00
                ##' ------|-------------------------|--------> = + sjl_rel
                ##'      msw                       msf
                ##'  - 0.95833...      +        0.0833...     = - 0.875 (< - 0.5).
                ##'
                ##' CONSIDERING THE SMALLEST DIFFERENCE BETWEEN TWO HOURS, when msw is
                ##' "behind" msf (relative to the arrow of time), sjlrel is presented
                ##' as positive. Example:
                ##'
                ##'          SMALLEST DIFFERENCE
                ##'     13:00 |----- 11h -----| 0:00 |----- 13h -----| 13:00
                ##' -----|----------------------|------------------------|-> = + sjlrel
                ##'     msw                    msf                      msw
                ##' - 0.54166...       +        0   = - 0.54166.. (< - 0.5)
                ##'
                ##' That way, msw is presumed to be sooner than msf.
                ##'
                ##' NOTE: date_time_ref_1 <- abs(as.duration(msf_ref - msw_ref))
                ##'

            } else if (date_time_ref_1 < lubridate::duration(-12, "hours")) {

                output$sjl_rel[i] <- output$sjl_rel[i]

                ##' CONDITION 4:
                ##'
                ##' This condition below refers to the situation below:
                ##'
                ##' 	23:00 |------- 3h ------| 02:00
                ##' ------|-------------------------|--------> = - sjlrel
                ##'      msf                       msw
                ##'    0.95833...       -        0.0833...     = 0.875 (> 0.5)
                ##'
                ##' CONSIDERING THE SMALLEST DIFFERENCE BETWEEN TWO HOURS, when msw is
                ##' in front of msw (relative to the arrow of time), sjlrel is
                ##' presented as negative. Example:
                ##'
                ##'          SMALLEST DIFFERENCE
                ##'    13:00 |----- 11h -----| 0:00 |----- 13h -----| 13:00
                ##' -----|----------------------|-----------------------|-> = - sjlrel
                ##'     msf                    msw                     msf
                ##'  0.54166...       -         0   = 0.54166.. (> 0.5)
                ##'
                ##' That way, msw is presumed to be later than msf
                ##'
                ##' NOTE: date_time_ref_1 <- abs(as.duration(msf_ref - msw_ref))

            } else if (date_time_ref_1 > lubridate::duration(-12, "hours")) {

                output$sjl_rel[i] <- - output$sjl_rel[i]

                ##' CONDITION 5 & 6:
                ##'
                ##' This two conditions below refers to the following situations:
                ##'
                ##'     02:00 |------- 2h ------| 04:00
                ##' ------|-------------------------|--------> = + sjlrel
                ##'      msw                       msf
                ##'  - 0.0833...        +        0.166...  = 0.0833... (> 0 AND < 0.5)
                ##'
                ##'      02:00 |------- 2h ------| 04:00
                ##'  ------|-------------------------|--------> = - sjlrel
                ##' 	  msf                       msw
                ##'     0.0833...        -        0.166... = - 0.0833... (< 0 AND > 0.5)
                ##'
                ##' If none of the conditions above were true, the signal of the
                ##' difference is maintained.
                ##'
                ##' NOTE 1: date_time_ref_1 <- abs(as.duration(msf_ref - msw_ref))
                ##'
                ##' NOTE 2: All the conditions presented since the "DO IF" command are
                ##' tested in order of appeareance, hence there's no need to add the
                ##' "AND < 0.5" or the "AND > 0.5" to the conditions below.

            } else if (date_time_ref_1 > lubridate::duration(0, "hours")) {

                output$sjl_rel[i] <- output$sjl_rel[i]

            } else if (date_time_ref_1 < lubridate::duration(0, "hours")) {

                output$sjl_rel[i] <- - output$sjl_rel[i]

            } else {

                output$sjl_rel[i] <- output$sjl_rel[i]

            }

        }

        #' Durations class may not show the correct value on print. To se the
        #' correct ones, try: output$sjl_rel[i] @.Data

        ## sjl

        if (!(is.na(output$sjl_rel[i]))) {

            output$sjl[i] <- abs(output$sjl_rel[i])

        }

        ## le_week

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$fd[i])) &&
            !(is.na(output$le_w[i])) &&
            !(is.na(output$le_f[i]))) {

            output$le_week[i] <- ((output$le_w[i] * output$wd[i]) +
                                      (output$le_f[i] * output$fd[i])) / 7

        }

        if (!(is.na(output$fd[i])) &&
            !(is.na(output$le_f[i]))) {

            if (output$fd[i] == 7) {

                output$le_week[i] <- output$le_f[i]

            }

        }

        if (!(is.na(output$wd[i])) &&
            !(is.na(output$le_w[i]))) {

            if (output$wd[i] == 7) {

                output$le_week[i] <- output$le_w[i]

            }

        }

    }

    # Manage msf_sc dates --------------------


    # Return output --------------------

    output

}
