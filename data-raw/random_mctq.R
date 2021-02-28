# Source the file before running the functions
# Don't forget to uncomment the `library` functions below

# library(lubridate)
# library(hms)
# library(crayon)
# library(glue)
# library(mctq)

#' Compute and print standard and micro MCTQ distribution parameters
#'
#' @description
#'
#' `std_mctq_par()` computes and prints the Munich Chronotype Questionnaire
#' (MCTQ) reference distribution parameters that is used on [mctq::random_mctq]
#' for __standard__ and __micro__ versions of the questionnaire. See
#' [mctq::random_mctq] to learn more.
#'
#' @details
#'
#' The parameters were based on the distributions shown in Roenneberg,
#' Wirz-Justice, & Merrow (2003) (Table 1, Figure 2, and Figure 7).
#'
#' We assume that all variables have normal distributions and that the minimal
#' and maximum values are, respectively, __\eqn{-3 s}__ and __\eqn{+3 s}__ from
#' the mean (where __\eqn{s}__ is the standard deviation of the sample).
#'
#' Please note that:
#'
#' * This is just a rough approximation, it by no means represents the same
#' distributions from the base article.
#'
#' * The distribution parameters from other variables not shown (like sleep
#' latency) were created based on the experience of the package authors with
#' MCTQ data.
#'
#' * This values are just for reference while building the random cases. If you
#' group several random MCTQ cases, this numbers can have some variation.
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' std_mctq_par()}
std_mctq_par <- function() {
    alert("\nStandard and micro MCTQ distribution parameters\n")

    values <- list( # Extracted from the base article
        w = list(
            suffix = "W",
            title = "Workdays (W)\n",
            sd_mean = hms::parse_hms("07:22:00"),
            sd_sd = hms::parse_hms("01:09:00"),
            ms_mean = hms::parse_hms("03:10:00"),
            ms_sd = hms::parse_hms("00:50:00")),
        f = list(
            suffix = "F",
            title = "\nWork-free days (F)\n",
            sd_mean = hms::parse_hms("08:27:00"),
            sd_sd = hms::parse_hms("01:32:00"),
            ms_mean = hms::parse_hms("05:02:00"),
            ms_sd = hms::parse_hms("01:32:00"))
    )

    for (i in values) {
        alert(i$title, combined_styles = c("black", "bold"))

        ms_mean <- i$ms_mean
        ms_sd <- i$ms_sd
        ms_min <- sum_time(ms_mean, - (3 * ms_sd), clock = TRUE)
        ms_max <- sum_time(ms_mean, + (3 * ms_sd), clock = TRUE)

        sd_mean <- i$sd_mean
        sd_sd <- i$sd_sd
        sd_min <- sum_time(sd_mean, - (3 * sd_sd), clock = TRUE)
        sd_max <- sum_time(sd_mean, + (3 * sd_sd), clock = TRUE)

        message <- paste0("Sleep onset (SO_", i$suffix, ")\n\n")
        cat(message)
        mean <- sum_time(ms_mean, - (sd_mean / 2), clock = TRUE)
        sd <- sum_time((ms_sd + sd_sd) / 2, clock = TRUE)
        min <- sum_time(ms_min, - (sd_mean / 2), clock = TRUE)
        max <- sum_time(ms_max, - (sd_mean / 2), clock = TRUE)
        cat_(min, max, mean, sd)

        message <- paste0("\nSleep end (SE_", i$suffix, ")\n\n")
        cat(message)
        mean <- sum_time(ms_mean, + (sd_mean / 2), clock = TRUE)
        sd <- sum_time((ms_sd + sd_sd) / 2, clock = TRUE)
        min <- sum_time(ms_min, + (sd_mean / 2), clock = TRUE)
        max <- sum_time(ms_max, + (sd_mean / 2), clock = TRUE)
        cat_(min, max, mean, sd)

        message <- paste0("\nSleep duration (SD_", i$suffix, ")\n\n")
        cat(message)
        cat_(sd_min, sd_max, sd_mean, sd_sd)

        message <- paste0("\nMid-sleep (MS", i$suffix, ")\n\n")
        cat(message)
        cat_(ms_min, ms_max, ms_mean, ms_sd)
    }

    invisible(NULL)
}

#' Compute and print MCTQ\eqn{^{Shift}}{ Shift} distribution parameters
#'
#' @description
#'
#' `shift_mctq_par()` computes and prints the Munich Chronotype Questionnaire
#' (MCTQ) reference distribution parameters that is used on [mctq::random_mctq]
#' for the __shift__ version of the questionnaire. See [mctq::random_mctq] to
#' learn more.
#'
#' @details
#'
#' The parameters were based on the distributions shown in Juda, Vetter, &
#' Roenneberg (2013) (Table 2 and Table 3).
#'
#' We assume that all variables have normal distributions and that the minimal
#' and maximum values are, respectively, __\eqn{-3 s}__ and __\eqn{+3 s}__ from
#' the mean (where __\eqn{s}__ is the standard deviation of the sample).
#'
#' Please note that:
#'
#' * This is just a rough approximation, it by no means represents the same
#' distributions from the mentioned article.
#'
#' * This distribution values include those who indicated to be woken up
#' involuntarily (by an alarm clock or other disturbances) on free days
#' following any shift. This can shift these values down.
#'
#' * The distribution parameters from other variables not shown here (like bed
#' time) were created based on the experience of the package authors with
#' MCTQ data.
#'
#' * This values are just for reference while building the random cases. If you
#' group several random MCTQ cases, this numbers can have some variation.
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' shift_mctq_par()}
shift_mctq_par <- function() {
    alert("\nMCTQ Shift distribution parameters\n")

    values <- list( # Extracted from the base article
        w_m = list(
            suffix = "W_M",
            title = "Between two morning shifts (W_M)\n",
            sprep_mean = hms::parse_hms("22:28:00"),
            sprep_sd = hms::parse_hms("01:04:00"),
            slat_mean = hms::as_hms(as.numeric(lubridate::dminutes(20.2))),
            slat_sd = hms::as_hms(as.numeric(lubridate::dminutes(23.7))),
            se_mean = hms::parse_hms("04:34:00"),
            se_sd = hms::parse_hms("00:34:00"),
            tgu_mean = hms::as_hms(as.numeric(lubridate::dminutes(6.4))),
            tgu_sd = hms::as_hms(as.numeric(lubridate::dminutes(11.3))),
            sd_mean = hms::parse_hms("05:46:00"),
            sd_sd = hms::parse_hms("01:14:00"),
            ms_mean = hms::parse_hms("01:43:00"),
            ms_sd = hms::parse_hms("00:40:00")),
        f_m = list(
            suffix = "F_M",
            title = "\nBetween two free days after morning shifts (F_M)\n",
            sprep_mean = hms::parse_hms("23:46:00"),
            sprep_sd = hms::parse_hms("01:19:00"),
            slat_mean = hms::as_hms(as.numeric(lubridate::dminutes(15.4))),
            slat_sd = hms::as_hms(as.numeric(lubridate::dminutes(15.6))),
            se_mean = hms::parse_hms("08:08:00"),
            se_sd = hms::parse_hms("01:46:00"),
            tgu_mean = hms::as_hms(as.numeric(lubridate::dminutes(14.9))),
            tgu_sd = hms::as_hms(as.numeric(lubridate::dminutes(23.4))),
            sd_mean = hms::parse_hms("08:04:00"),
            sd_sd = hms::parse_hms("01:34:00"),
            ms_mean = hms::parse_hms("04:04:00"),
            ms_sd = hms::parse_hms("01:22:00")),
        w_e = list(
            suffix = "W_E",
            title = "\nBetween two evening shifts (W_E)\n",
            sprep_mean = hms::parse_hms("00:38:00"),
            sprep_sd = hms::parse_hms("00:59:00"),
            slat_mean = hms::as_hms(as.numeric(lubridate::dminutes(14.9))),
            slat_sd = hms::as_hms(as.numeric(lubridate::dminutes(17.7))),
            se_mean = hms::parse_hms("08:25:00"),
            se_sd = hms::parse_hms("01:20:00"),
            tgu_mean = hms::as_hms(as.numeric(lubridate::dminutes(11.7))),
            tgu_sd = hms::as_hms(as.numeric(lubridate::dminutes(13))),
            sd_mean = hms::parse_hms("07:31:00"),
            sd_sd = hms::parse_hms("01:17:00"),
            ms_mean = hms::parse_hms("04:38:00"),
            ms_sd = hms::parse_hms("01:01:00")),
        f_e = list(
            suffix = "F_E",
            title = "\nBetween two free days after evening shifts (F_E)\n",
            sprep_mean = hms::parse_hms("00:09:00"),
            sprep_sd = hms::parse_hms("01:26:00"),
            slat_mean = hms::as_hms(as.numeric(lubridate::dminutes(14.5))),
            slat_sd = hms::as_hms(as.numeric(lubridate::dminutes(16.2))),
            se_mean = hms::parse_hms("08:33:00"),
            se_sd = hms::parse_hms("01:31:00"),
            tgu_mean = hms::as_hms(as.numeric(lubridate::dminutes(11.9))),
            tgu_sd = hms::as_hms(as.numeric(lubridate::dminutes(10.9))),
            sd_mean = hms::parse_hms("08:08:00"),
            sd_sd = hms::parse_hms("01:18:00"),
            ms_mean = hms::parse_hms("04:28:00"),
            ms_sd = hms::parse_hms("01:20:00")),
        w_n = list(
            suffix = "W_N",
            title = "\nBetween two night shifts (W_N)\n",
            sprep_mean = hms::parse_hms("07:19:00"),
            sprep_sd = hms::parse_hms("00:57:00"),
            slat_mean = hms::as_hms(as.numeric(lubridate::dminutes(13.9))),
            slat_sd = hms::as_hms(as.numeric(lubridate::dminutes(19.4))),
            se_mean = hms::parse_hms("13:28:00"),
            se_sd = hms::parse_hms("01:32:00"),
            tgu_mean = hms::as_hms(as.numeric(lubridate::dminutes(13.8))),
            tgu_sd = hms::as_hms(as.numeric(lubridate::dminutes(16))),
            sd_mean = hms::parse_hms("05:52:00"),
            sd_sd = hms::parse_hms("01:26:00"),
            ms_mean = hms::parse_hms("10:31:00"),
            ms_sd = hms::parse_hms("01:02:00")),
        f_n = list(
            suffix = "F_N",
            title = "\nBetween two free days after night shifts (F_N)\n",
            sprep_mean = hms::parse_hms("00:40:00"),
            sprep_sd = hms::parse_hms("02:19:00"),
            slat_mean = hms::as_hms(as.numeric(lubridate::dminutes(23.1))),
            slat_sd = hms::as_hms(as.numeric(lubridate::dminutes(36.5))),
            se_mean = hms::parse_hms("09:01:00"),
            se_sd = hms::parse_hms("02:19:00"),
            tgu_mean = hms::as_hms(as.numeric(lubridate::dminutes(13.6))),
            tgu_sd = hms::as_hms(as.numeric(lubridate::dminutes(13.4))),
            sd_mean = hms::parse_hms("07:55:00"),
            sd_sd = hms::parse_hms("01:53:00"),
            ms_mean = hms::parse_hms("05:02:00"),
            ms_sd = hms::parse_hms("02:11:00"))
    )

    for (i in values) {
        alert(i$title, combined_styles = c("black", "bold"))

        message <- paste0("Local time of preparing to sleep (SPrep_",
                          i$suffix, ")\n\n")
        cat(message)
        sprep_mean <- i$sprep_mean
        sprep_sd <- i$sprep_sd
        sprep_min <- sum_time(sprep_mean, - (3 * sprep_sd), clock = TRUE)
        sprep_max <- sum_time(sprep_mean, + (3 * sprep_sd), clock = TRUE)
        cat_(sprep_min, sprep_max, sprep_mean, sprep_sd)

        message <- paste0("\nSleep latency (SLat_", i$suffix, ")\n\n")
        cat(message)
        slat_mean <- i$slat_mean
        slat_sd <- i$slat_sd
        slat_min <- sum_time(slat_mean, - (3 * slat_sd))
        if (slat_min <= 0) slat_min <- hms::as_hms(0)
        slat_max <- sum_time(slat_mean, + (3 * slat_sd))
        cat_(slat_min, slat_max, slat_mean, slat_sd)

        message <- paste0("\nSleep onset (SO_", i$suffix, ")\n\n")
        cat(message)
        mean <- sum_time(sprep_mean, slat_mean, clock = TRUE)
        sd <- sum_time(sprep_sd + slat_sd)
        min <- sum_time(sprep_min, slat_min, clock = TRUE)
        max <- sum_time(sprep_max, slat_max, clock = TRUE)
        cat_(min, max, mean, sd)

        message <- paste0("\nSleep end (SE_", i$suffix, ")\n\n")
        cat(message)
        mean <- i$se_mean
        sd <- i$se_sd
        min_max(mean, sd)

        message <- paste0("\nTime to get up (TGU_", i$suffix, ")\n\n")
        cat(message)
        mean <- i$tgu_mean
        sd <- i$tgu_sd
        min <- sum_time(mean, - (3 * sd))
        if (min <= 0) min <- hms::as_hms(0)
        max <- sum_time(mean, + (3 * sd))
        cat_(min, max, mean, sd)

        message <- paste0("\nSleep duration (SD_", i$suffix, ")\n\n")
        cat(message)
        mean <- i$sd_mean
        sd <- i$sd_sd
        min_max(mean, sd)

        message <- paste0("\nMid-sleep (MS", i$suffix, ")\n\n")
        cat(message)
        mean <- i$ms_mean
        sd <- i$ms_sd
        min_max(mean, sd)
    }

    invisible(NULL)
}

#' Test the creation of [mctq::random_mctq] cases
#'
#' @description
#'
#' `force_random_mctq()` tests [mctq::random_mctq] ability to create cases
#' by forcing it to produce a number of sequential cases.
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"`.
#' @param iterations An [integerish][checkmate::test_integerish()] `numeric`
#'   object or an `integer` object, of length `1`, corresponding to the number
#'   of iterations while running [mctq::random_mctq] (default: `100`).
#' @param seed An [integerish][checkmate::test_integerish()] `numeric` object or
#'   an `integer` object, of length `1`, corresponding to the seed number for
#'   random generation (see [base::set.seed] to learn more) (default: `1`).
#'
#' @return An invisible `tibble` with rows representing each iteration.
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' force_random_mctq("standard")
#' force_random_mctq("micro", iterations = 100)
#' force_random_mctq("shift", iterations = 100)}
force_random_mctq <- function(model, iterations = 100, seed = 1) {
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))
    assert_numeric_(iterations)
    checkmate::assert_numeric(iterations, lower = 0, upper = 500)
    checkmate::assert_integerish(iterations)
    assert_numeric_(seed)
    checkmate::assert_numeric(seed, lower = 0, upper = 500)
    checkmate::assert_integerish(seed)

    iterations <- as.integer(iterations)
    seed <- as.integer(seed)

    set.seed(seed)
    out <- dplyr::as_tibble(random_mctq(model = model, quiet = TRUE))

    for (i in seq_len(iterations - 1)) {
        random_case <- random_mctq(model = model, quiet = TRUE)
        out <- dplyr::bind_rows(out, dplyr::as_tibble(random_case))
    }

    invisible(out)
}

#' Print a [mctq::random_mctq] raw case code
#'
#' @description
#'
#' `random_mctq_raw_code()` prints a [mctq::random_mctq] raw case code to be
#' used with `build_<model>_mctq`. It's purpose is to help programming special
#' mctq cases for [mctq::std_mctq], [mctq::micro_mctq], and [mctq::shift_mctq]
#' datasets.
#'
#' @details
#'
#' The seed used for random generation is the result of `sample(100:200, 1)`
#' (see [base::set.seed]).
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"`.
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' random_mctq_raw_code("standard")
#' random_mctq_raw_code("micro")
#' random_mctq_raw_code("shift")}
random_mctq_raw_code <- function(model) {
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))

    set.seed(sample(100:200, 1))
    data <- random_mctq(model = model, quiet = TRUE)

    if (model == "standard") {
        cat(bt("WORK REGULAR"), " = ",
            dq(format_logical(data$work)),
            ", # logical | Yes/No", "\n",

            bt("WORK DAYS"), " = ",
            dq(format_na(data$wd)),
            ", # integer | [0-7]", "\n\n",


            bt("W BED TIME"), " = ",
            dq(format_hms(data$bt_w)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            bt("W SLEEP PREP"), " = ",
            dq(format_hms(data$sprep_w)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            bt("W SLEEP LAT"), " = ",
            dq(format_duration(data$slat_w)),
            ", # Duration | M", "\n",

            bt("W SLEEP END"), " = ",
            dq(format_hms(data$se_w)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            bt("W SLEEP INERTIA"), " = ",
            dq(format_duration(data$si_w)),
            ", # Duration | M", "\n",

            bt("W ALARM"), " = ",
            dq(format_logical(data$alarm_w)),
            ", # logical | Yes/No", "\n",

            bt("W WAKE BEFORE ALARM"), " = ",
            dq(format_logical(data$wake_before_w)),
            ", # logical | Yes/No", "\n",

            bt("W LIGHT EXPOSURE"), " = ",
            dq(format_hms(data$le_w)),
            ", # Duration | [H]MS, [H]M, [H]", "\n\n",


            bt("F BED TIME"), " = ",
            dq(format_hms(data$bt_f)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            bt("F SLEEP PREP"), " = ",
            dq(format_hms(data$sprep_f)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            bt("F SLEEP LAT"), " = ",
            dq(format_duration(data$slat_f)),
            ", # Duration | M", "\n",

            bt("F SLEEP END"), " = ",
            dq(format_hms(data$se_f)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            bt("F SLEEP INERTIA"), " = ",
            dq(format_duration(data$si_f)),
            ", # Duration | M", "\n",

            bt("F ALARM"), " = ",
            dq(format_logical(data$alarm_f)),
            ", # logical | Yes/No", "\n",

            bt("F REASONS"), " = ",
            dq(format_logical(data$reasons_f)),
            ", # logical | Yes/No", "\n",

            bt("F REASONS WHY"), " = ",
            dq(format_na(data$reasons_why_f)),
            ", # character", "\n",

            bt("F LIGHT EXPOSURE"), " = ",
            dq(format_hms(data$le_f)),
            " # Duration | [H]MS, [H]M, [H]",

            sep = ""
        )
    } else if (model == "micro") {
        cat(bt("SHIFT WORK"), " = ",
            dq(format_logical(data$shift_work)),
            ", # logical | Yes/No", "\n",

            bt("WORK DAYS"), " = ",
            dq(format_na(data$wd)),
            ", # integer | [0-7]", "\n\n",


            bt("W SLEEP ONSET"), " = ",
            dq(format_hms_imp(data$so_w)),
            ", # hms | IMp [0-12h]", "\n",

            bt("W SLEEP END"), " = ",
            dq(format_hms_imp(data$se_w)),
            ", # hms | IMp [0-12h]", "\n\n",

            bt("F SLEEP ONSET"), " = ",
            dq(format_hms_imp(data$so_f)),
            ", # hms | IMp [0-12h]", "\n",

            bt("F SLEEP END"), " = ",
            dq(format_hms_imp(data$se_f)),
            " # hms | IMp [0-12h]",

            sep = ""
        )
    } else if (model == "shift") {
        values <- list(
            w_m = c("W M", "_w_m"),
            f_m = c("F M", "_f_m"),
            w_e = c("W E", "_w_e"),
            f_r = c("F E", "_f_e"),
            w_n = c("W N", "_w_n"),
            f_n = c("F N", "_f_n")
            )

        for (i in values) {
            cat(bt(paste(i[1], "N DAYS")), " = ",
                dq(format_na(data[[paste0("n", i[2])]])),
                ", # integer | [0-7]", "\n",

                bt(paste(i[1], "BED TIME")), " = ",
                dq(format_hms(data[[paste0("bt", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                bt(paste(i[1], "SLEEP PREP")), " = ",
                dq(format_hms(data[[paste0("sprep", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                bt(paste(i[1], "SLEEP LAT")), " = ",
                dq(format_duration(data[[paste0("slat", i[2])]])),
                ", # Duration | M", "\n",

                bt(paste(i[1], "SLEEP END")), " = ",
                dq(format_hms(data[[paste0("se", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                bt(paste(i[1], "TIME GU")), " = ",
                dq(format_duration(data[[paste0("tgu", i[2])]])),
                ", # Duration | M", "\n",

                bt(paste(i[1], "ALARM")), " = ",
                dq(format_logical(data[[paste0("alarm", i[2])]])),
                ", # logical | Yes/No", "\n",

                bt(paste(i[1], "REASONS")), " = ",
                dq(format_logical(data[[paste0("reasons", i[2])]])),
                ", # logical | Yes/No", "\n",

                bt(paste(i[1], "REASONS WHY")), " = ",
                dq(format_na(data[[paste0("reasons_why", i[2])]])),
                ", # character", "\n",

                bt(paste(i[1], "NAP")), " = ",
                dq(format_logical(data[[paste0("nap", i[2])]])),
                ", # logical | Yes/No", "\n",

                bt(paste(i[1], "NAP ONSET")), " = ",
                dq(format_hms(data[[paste0("napo", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                bt(paste(i[1], "NAP END")), " = ",
                dq(format_hms(data[[paste0("nape", i[2])]])),
                ifelse(i[1] == "F N", " ", ", "),
                "# hms | HMS, HM, H [0-24h]", "\n",

                ifelse(i[1] == "F N", "", "\n"), sep = ""
                )
        }
    }

    invisible(NULL)
}

alert <- mctq:::alert

cat_ <- function(min, max, mean, sd) {
    cat("Min:", as.character(min), "\n")
    cat("Max:", as.character(max), "\n")
    cat("Mean:", as.character(mean), "\n")
    cat("SD:", as.character(sd), "\n")
}

min_max <- function(mean, sd) {
    min <- sum_time(mean, - (3 * sd), clock = TRUE)
    max <- sum_time(mean, + (3 * sd), clock = TRUE)

    cat_(min, max, mean, sd)
}

format_logical <- function(x) {
    if(is.na(x)) {
        as.character("")
    } else {
        dplyr::case_when(
            x == TRUE ~ "Yes",
            x == FALSE ~ "No"
        )
    }
}

format_hms <- function(x) {
    if(is.na(x)) {
        as.character("")
    } else {
        format <- c(1, 5)
        x <- convert(x, "hms")
        substr(as.character(x), format[1], format[2])
    }
}

format_hms_imp <- function(x) {
    if(is.na(x)) {
        as.character("")
    } else {
        if (lubridate::hour(x) == 12) {
            am_pm <- "PM"
        } else if (lubridate::hour(x) == 0) {
            x <- hms::parse_hm("12:00")
            am_pm <- "AM"
        } else if (lubridate::hour(x) > 12) {
            x <- hms::as_hms(x - lubridate::dhours(12))
            am_pm <- "PM"
        } else {
            am_pm <- "AM"
        }

        format <- c(1, 5)
        paste(substr(as.character(x), format[1], format[2]), am_pm)
    }
}

format_duration <- function(x) {
    if(is.na(x)) {
        as.character("")
    } else {
        as.character(convert_tu(x, "M"))
    }
}

format_na <- function(x) {
    if(is.na(x)) {
        as.character("")
    } else {
        as.character(x)
    }
}

bt <- function(x) glue::backtick(x)
dq <- function(x) glue::double_quote(x)

# std_mctq_par()
# shift_mctq_par()
# force_random_mctq("standard")
# force_random_mctq("micro")
# force_random_mctq("shift")
# random_mctq_raw_code("standard")
# random_mctq_raw_code("micro")
# random_mctq_raw_code("shift")
