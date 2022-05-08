# # Notes
#
# * Source the file before running the functions.
# * Don't forget to uncomment the 'library' functions below.

# library(checkmate)
# library(cli)
# library(glue)
# library(hms)
# library(lubridate)
# library(mctq)
# library(utils)

#' Compute and print standard and micro MCTQ distribution parameters
#'
#' @description
#'
#' `std_mctq_par()` computes and prints the Munich ChronoType Questionnaire
#' (MCTQ) reference distribution parameters that is used on
#' [`random_mctq()`][mctq::random_mctq] for __standard__ and __micro__ versions
#' of the questionnaire. See [`random_mctq()`][mctq::random_mctq] to learn more.
#'
#' @details
#'
#' The parameters were based on the distributions shown in Roenneberg,
#' Wirz-Justice, & Merrow (2003) (Table 1, Figure 2, and Figure 7).
#'
#' We assume that all variables have normal distributions and that the minimal
#' and maximum values are, respectively, __\eqn{-3 s}__ and __\eqn{+3 s}__ from
#' the mean, where __\eqn{s}__ is the standard deviation of the sample.
#'
#' Please note that:
#'
#' * This is just a rough approximation, it by no means represents the same
#' distributions from the base article.
#' * The distribution parameters from other variables not shown (like sleep
#' latency) were created based on the experience of the package authors with
#' MCTQ data.
#' * These values are just for reference while building the random cases. If you
#' group several random MCTQ cases, these numbers can have some variation.
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' std_mctq_par()}
std_mctq_par <- function() {
    cli::cli_h1("Standard and micro MCTQ distribution parameters")

    values <- list( # Extracted from the base article
        w = list(
            suffix = "W",
            title = "Workdays (W)",
            sd_mean = hms::parse_hms("07:22:00"),
            sd_sd = hms::parse_hms("01:09:00"),
            ms_mean = hms::parse_hms("03:10:00"),
            ms_sd = hms::parse_hms("00:50:00")),
        f = list(
            suffix = "F",
            title = "Work-free days (F)",
            sd_mean = hms::parse_hms("08:27:00"),
            sd_sd = hms::parse_hms("01:32:00"),
            ms_mean = hms::parse_hms("05:02:00"),
            ms_sd = hms::parse_hms("01:32:00"))
    )

    for (i in values) {
        cli::cli_h2(i$title)

        ms_mean <- i$ms_mean
        ms_sd <- i$ms_sd
        ms_min <- sum_time(ms_mean, - (3 * ms_sd),
                           cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()
        ms_max <- sum_time(ms_mean, + (3 * ms_sd),
                           cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        sd_mean <- i$sd_mean
        sd_sd <- i$sd_sd
        sd_min <- sum_time(sd_mean, - (3 * sd_sd),
                           cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()
        sd_max <- sum_time(sd_mean, + (3 * sd_sd),
                           cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        cli::cli_h3(paste0(
            "Local time of sleep onset (SO_", i$suffix, ")"
            ))
        cli::cat_line()

        mean <- sum_time(ms_mean, - (sd_mean / 2),
                         cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()
        sd <- sum_time((ms_sd + sd_sd) / 2,
                       cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()
        min <- sum_time(ms_min, - (sd_mean / 2),
                        cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()
        max <- sum_time(ms_max, - (sd_mean / 2),
                        cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()
        cat_(min, max, mean, sd)

        cli::cli_h3(paste0(
            "\nLocal time of sleep end (SE_", i$suffix, ")"
        ))
        cli::cat_line()

        mean <- sum_time(ms_mean, + (sd_mean / 2),
                         cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        sd <- sum_time((ms_sd + sd_sd) / 2,
                       cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        min <- sum_time(ms_min, + (sd_mean / 2),
                        cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        max <- sum_time(ms_max, + (sd_mean / 2),
                        cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        cat_(min, max, mean, sd)

        cli::cli_h3(paste0(
            "\nSleep duration (SD_", i$suffix, ")"
        ))
        cli::cat_line()

        cat_(sd_min, sd_max, sd_mean, sd_sd)

        cli::cli_h3(paste0(
            "\nLocal time of mid-sleep (MS", i$suffix, ")"
        ))
        cli::cat_line()

        cat_(ms_min, ms_max, ms_mean, ms_sd)
    }

    invisible(NULL)
}

#' Compute and print MCTQ\eqn{^{Shift}}{ Shift} distribution parameters
#'
#' @description
#'
#' `shift_mctq_par()` computes and prints the Munich ChronoType Questionnaire
#' (MCTQ) reference distribution parameters that is used on
#' [`random_mctq()`][mctq::random_mctq] for the __shift__ version of the
#' questionnaire. See [`random_mctq()`][mctq::random_mctq] to learn more.
#'
#' @details
#'
#' The parameters were based on the distributions shown in Juda, Vetter, &
#' Roenneberg (2013) (Table 2 and 3).
#'
#' We assume that all variables have normal distributions and that the minimal
#' and maximum values are, respectively, __\eqn{-3 s}__ and __\eqn{+3 s}__ from
#' the mean (where __\eqn{s}__ is the standard deviation of the sample).
#'
#' Please note that:
#'
#' * This is just a rough approximation, it by no means represents the same
#' distributions from the mentioned article.
#' * This distribution values include those who indicated to be woken up
#' involuntarily (by an alarm clock or other disturbances) on free days
#' following any shift. This can shift these values down.
#' * The distribution parameters from other variables not shown here (like
#' bedtime) were created based on the experience of the package authors with
#' MCTQ data.
#' * These values are just for reference while building the random cases. If you
#' group several random MCTQ cases, these numbers can have some variation.
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' shift_mctq_par()}
shift_mctq_par <- function() {
    cli::cli_h1("MCTQ Shift distribution parameters")

    values <- list( # Extracted from the base article
        w_m = list(
            suffix = "W_M",
            title = "Between two morning shifts (W_M)",
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
            title = "Between two free days after morning shifts (F_M)",
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
            title = "Between two evening shifts (W_E)",
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
            title = "Between two free days after evening shifts (F_E)",
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
            title = "Between two night shifts (W_N)",
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
            title = "Between two free days after night shifts (F_N)",
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
        cli::cli_h2(i$title)

        cli::cli_h3(paste0(
            "Local time of preparing to sleep (SPrep_", i$suffix, ")"
        ))
        cli::cat_line()

        sprep_mean <- i$sprep_mean
        sprep_sd <- i$sprep_sd

        sprep_min <- sum_time(sprep_mean, - (3 * sprep_sd),
                              cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        sprep_max <- sum_time(sprep_mean, + (3 * sprep_sd),
                              cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        cat_(sprep_min, sprep_max, sprep_mean, sprep_sd)

        cli::cli_h3(paste0(
            "Sleep latency (SLat_", i$suffix, ")"
        ))
        cli::cat_line()

        slat_mean <- i$slat_mean
        slat_sd <- i$slat_sd

        slat_min <- sum_time(slat_mean, - (3 * slat_sd)) %>%
            as.numeric() %>%
            hms::hms()

        if (slat_min <= 0) slat_min <- hms::as_hms(0)

        slat_max <- sum_time(slat_mean, + (3 * slat_sd)) %>%
            as.numeric() %>%
            hms::hms()

        cat_(slat_min, slat_max, slat_mean, slat_sd)

        cli::cli_h3(paste0(
            "Local time of sleep onset (SO_", i$suffix, ")"
        ))
        cli::cat_line()

        mean <- sum_time(sprep_mean, slat_mean, cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        sd <- sum_time(sprep_sd + slat_sd) %>%
            as.numeric() %>%
            hms::hms()

        min <- sum_time(sprep_min, slat_min, cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        max <- sum_time(sprep_max, slat_max, cycle = lubridate::ddays()) %>%
            as.numeric() %>%
            hms::hms()

        cat_(min, max, mean, sd)

        cli::cli_h3(paste0(
            "Local time of sleep end (SE_", i$suffix, ")"
        ))
        cli::cat_line()

        mean <- i$se_mean
        sd <- i$se_sd
        min_max(mean, sd)

        cli::cli_h3(paste0(
            "Time to get up (TGU_", i$suffix, ")"
        ))
        cli::cat_line()

        mean <- i$tgu_mean
        sd <- i$tgu_sd

        min <- sum_time(mean, - (3 * sd)) %>%
            as.numeric() %>%
            hms::hms()

        if (min <= 0) min <- hms::as_hms(0)

        max <- sum_time(mean, + (3 * sd)) %>%
            as.numeric() %>%
            hms::hms()

        cat_(min, max, mean, sd)

        cli::cli_h3(paste0(
            "Sleep duration (SD_", i$suffix, ")"
        ))
        cli::cat_line()

        mean <- i$sd_mean
        sd <- i$sd_sd
        min_max(mean, sd)

        cli::cli_h3(paste0(
            "Local time of mid-sleep (MS", i$suffix, ")"
        ))
        cli::cat_line()

        mean <- i$ms_mean
        sd <- i$ms_sd
        min_max(mean, sd)
    }

    invisible(NULL)
}

#' Test the creation of `random_mctq()` cases
#'
#' @description
#'
#' `force_random_mctq()` tests [`random_mctq()`][mctq::random_mctq] ability to
#' create cases by forcing it to produce a number of sequential cases.
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"`.
#' @param iterations An integer number corresponding to the number of iterations
#'   while running [`random_mctq()`][mctq::random_mctq] (default: `100`).
#' @param seed An integer number corresponding to the seed number for
#'   random generation (see [base::set.seed] to learn more) (default: `1`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with rows representing each
#'   iteration.
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
    checkmate::assert_integerish(iterations, lower = 0, upper = 500)
    checkmate::assert_integerish(seed, lower = 0, upper = 500)

    set.seed(as.integer(seed))
    out <- dplyr::as_tibble(random_mctq(model = model))

    for (i in seq_len(as.integer(iterations) - 1)) {
        random_case <- random_mctq(model = model)
        out <- dplyr::bind_rows(out, dplyr::as_tibble(random_case))
    }

    invisible(out)
}

#' Print a `random_mctq()` raw case code
#'
#' @description
#'
#' `random_mctq_raw_code()` prints a [`random_mctq()`][mctq::random_mctq] raw
#' case code to be used with a `build_<model>_mctq()`function. Its purpose is to
#' help programming special MCTQ cases for [`std_mctq`][mctq::std_mctq],
#' [`micro_mctq`][mctq::micro_mctq], and [`shift_mctq`][mctq::shift_mctq]
#' datasets.
#'
#' @details
#'
#' The seed used for random generation is the result of `sample(100:200, 1)`
#' (see [`?set.seed`][base::set.seed] to learn more).
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"`.
#' @param clipboard A [`logical`][base::logical()] value indicating if the case
#'   code must be transfered to the Windows clipboard. See
#'   [`?writeClipboard`][utils::writeClipboard] to learn more (default: `TRUE`).
#'
#' @family random_mctq functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' random_mctq_raw_code("standard")
#' random_mctq_raw_code("micro")
#' random_mctq_raw_code("shift")
#' }
random_mctq_raw_code <- function(model, clipboard = TRUE) {
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))

    set.seed(sample(100:200, 1))
    data <- random_mctq(model = model)

    if (model %in% c("std", "standard")) {
        out <- paste0(
            glue::backtick("WORK REGULAR"), " = ",
            glue::double_quote(format_logical(data$work)),
            ", # logical | Yes/No", "\n",

            glue::backtick("WORK DAYS"), " = ",
            glue::double_quote(format_na(data$wd)),
            ", # integer | [0-7]", "\n\n",


            glue::backtick("W BEDTIME"), " = ",
            glue::double_quote(format_hms(data$bt_w)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            glue::backtick("W SLEEP PREP"), " = ",
            glue::double_quote(format_hms(data$sprep_w)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            glue::backtick("W SLEEP LAT"), " = ",
            glue::double_quote(format_duration(data$slat_w)),
            ", # Duration | M", "\n",

            glue::backtick("W SLEEP END"), " = ",
            glue::double_quote(format_hms(data$se_w)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            glue::backtick("W SLEEP INERTIA"), " = ",
            glue::double_quote(format_duration(data$si_w)),
            ", # Duration | M", "\n",

            glue::backtick("W ALARM"), " = ",
            glue::double_quote(format_logical(data$alarm_w)),
            ", # logical | Yes/No", "\n",

            glue::backtick("W WAKE BEFORE ALARM"), " = ",
            glue::double_quote(format_logical(data$wake_before_w)),
            ", # logical | Yes/No", "\n",

            glue::backtick("W LIGHT EXPOSURE"), " = ",
            glue::double_quote(format_hms(data$le_w)),
            ", # Duration | [H]MS, [H]M, [H]", "\n\n",


            glue::backtick("F BEDTIME"), " = ",
            glue::double_quote(format_hms(data$bt_f)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            glue::backtick("F SLEEP PREP"), " = ",
            glue::double_quote(format_hms(data$sprep_f)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            glue::backtick("F SLEEP LAT"), " = ",
            glue::double_quote(format_duration(data$slat_f)),
            ", # Duration | M", "\n",

            glue::backtick("F SLEEP END"), " = ",
            glue::double_quote(format_hms(data$se_f)),
            ", # hms | HMS, HM, H [0-24h]", "\n",

            glue::backtick("F SLEEP INERTIA"), " = ",
            glue::double_quote(format_duration(data$si_f)),
            ", # Duration | M", "\n",

            glue::backtick("F ALARM"), " = ",
            glue::double_quote(format_logical(data$alarm_f)),
            ", # logical | Yes/No", "\n",

            glue::backtick("F REASONS"), " = ",
            glue::double_quote(format_logical(data$reasons_f)),
            ", # logical | Yes/No", "\n",

            glue::backtick("F REASONS WHY"), " = ",
            glue::double_quote(format_na(data$reasons_why_f)),
            ", # character", "\n",

            glue::backtick("F LIGHT EXPOSURE"), " = ",
            glue::double_quote(format_hms(data$le_f)),
            " # Duration | [H]MS, [H]M, [H]"
        )

        cat(out, sep = "")

        if (isTRUE(clipboard)) {
            cli::cat_line()
            clipboard(out)
        }
    } else if (model == "micro") {
        out <- paste0(
            glue::backtick("SHIFT WORK"), " = ",
            glue::double_quote(format_logical(data$shift_work)),
            ", # logical | Yes/No", "\n",

            glue::backtick("WORK DAYS"), " = ",
            glue::double_quote(format_na(data$wd)),
            ", # integer | [0-7]", "\n\n",


            glue::backtick("W SLEEP ONSET"), " = ",
            glue::double_quote(format_hms_imp(data$so_w)),
            ", # hms | IMp [0-12h]", "\n",

            glue::backtick("W SLEEP END"), " = ",
            glue::double_quote(format_hms_imp(data$se_w)),
            ", # hms | IMp [0-12h]", "\n\n",

            glue::backtick("F SLEEP ONSET"), " = ",
            glue::double_quote(format_hms_imp(data$so_f)),
            ", # hms | IMp [0-12h]", "\n",

            glue::backtick("F SLEEP END"), " = ",
            glue::double_quote(format_hms_imp(data$se_f)),
            " # hms | IMp [0-12h]"
        )

        cat(out, sep = "")

        if (isTRUE(clipboard)) {
            cli::cat_line()
            clipboard(out)
        }
    } else if (model == "shift") {
        values <- list(
            w_m = c("W M", "_w_m"),
            f_m = c("F M", "_f_m"),
            w_e = c("W E", "_w_e"),
            f_r = c("F E", "_f_e"),
            w_n = c("W N", "_w_n"),
            f_n = c("F N", "_f_n")
            )

        out <- character()

        for (i in values) {
            out <- out %>% append(paste0(
                glue::backtick(paste(i[1], "N DAYS")), " = ",
                glue::double_quote(format_na(data[[paste0("n", i[2])]])),
                ", # integer | [0-7]", "\n",

                glue::backtick(paste(i[1], "BEDTIME")), " = ",
                glue::double_quote(format_hms(data[[paste0("bt", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                glue::backtick(paste(i[1], "SLEEP PREP")), " = ",
                glue::double_quote(format_hms(data[[paste0("sprep", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                glue::backtick(paste(i[1], "SLEEP LAT")), " = ",
                glue::double_quote(format_duration(
                    data[[paste0("slat", i[2])]])),
                ", # Duration | M", "\n",

                glue::backtick(paste(i[1], "SLEEP END")), " = ",
                glue::double_quote(format_hms(data[[paste0("se", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                glue::backtick(paste(i[1], "TIME GU")), " = ",
                glue::double_quote(format_duration(
                    data[[paste0("tgu", i[2])]])),
                ", # Duration | M", "\n",

                glue::backtick(paste(i[1], "ALARM")), " = ",
                glue::double_quote(format_logical(
                    data[[paste0("alarm", i[2])]])),
                ", # logical | Yes/No", "\n",

                glue::backtick(paste(i[1], "REASONS")), " = ",
                glue::double_quote(format_logical(
                    data[[paste0("reasons", i[2])]])),
                ", # logical | Yes/No", "\n",

                glue::backtick(paste(i[1], "REASONS WHY")), " = ",
                glue::double_quote(format_na(
                    data[[paste0("reasons_why", i[2])]])),
                ", # character", "\n",

                glue::backtick(paste(i[1], "NAP")), " = ",
                glue::double_quote(format_logical(data[[paste0("nap", i[2])]])),
                ", # logical | Yes/No", "\n",

                glue::backtick(paste(i[1], "NAP ONSET")), " = ",
                glue::double_quote(format_hms(data[[paste0("napo", i[2])]])),
                ", # hms | HMS, HM, H [0-24h]", "\n",

                glue::backtick(paste(i[1], "NAP END")), " = ",
                glue::double_quote(format_hms(data[[paste0("nape", i[2])]])),
                ifelse(i[1] == "F N", " ", ", "),
                "# hms | HMS, HM, H [0-24h]",

                ifelse(i[1] == "F N", "", "\n\n")
                ))
        }

        out <- paste(out, collapse = "")
        cat(out, sep = "")

        if (isTRUE(clipboard)) {
            cli::cat_line()
            clipboard(out)
        }
    }

    invisible(NULL)
}

cat_ <- function(min, max, mean, sd) {
    cli::cli_text("{.emph Min:} ", as.character(min), "\n")
    cli::cli_text("{.emph Max:} ", as.character(max), "\n")
    cli::cli_text("{.emph Mean:} ", as.character(mean), "\n")
    cli::cli_text("{.emph SD:} ", as.character(sd), "\n")
}

min_max <- function(mean, sd) {
    min <- sum_time(mean, - (3 * sd), cycle = lubridate::ddays()) %>%
        as.numeric() %>%
        hms::hms()

    max <- sum_time(mean, + (3 * sd), cycle = lubridate::ddays()) %>%
        as.numeric() %>%
        hms::hms()

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
        x <- hms::hms(mctq:::extract_seconds(x))
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
        as.character(as.numeric(x) / 60)
    }
}

format_na <- function(x) {
    if(is.na(x)) {
        as.character("")
    } else {
        as.character(x)
    }
}

clipboard <- function(..., space_above = TRUE, n_minus = TRUE, quiet = FALSE) {
    mctq:::assert_has_length(list(...))
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(quiet)

    utils::writeClipboard(as.character(unlist(list(...), use.names = FALSE)))

    if (isFALSE(quiet)) {
        if(isTRUE(space_above)) cli::cat_line()
        cli::cli_inform("{cli::col_silver('[Copied to clipboard]')}")
    }
}

# std_mctq_par()
# shift_mctq_par()
# force_random_mctq("standard")
# force_random_mctq("micro")
# force_random_mctq("shift")
# random_mctq_raw_code("standard")
# random_mctq_raw_code("micro")
# random_mctq_raw_code("shift")
