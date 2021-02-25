# Source the file before running the functions
# Don't forget to uncomment the `library` functions

# library(lubridate)
# library(hms)
# library(crayon)
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
#' ## Requirements
#'
#' This function works only in interactive mode.
#'
#' ## Guidelines
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
    # Check requirements -----

    if (!is_interactive()) {
        stop("This function can only be used in interactive mode.",
             call. = FALSE)
    }

    # Title message -----

    alert("\nStandard and micro MCTQ distribution parameters\n")

    # Set values -----

    values <- list( # extracted from the base article
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

    # Compute and print variables -----

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
        cat_(mean, sd, min, max)

        message <- paste0("\nSleep end (SE_", i$suffix, ")\n\n")
        cat(message)
        mean <- sum_time(ms_mean, + (sd_mean / 2), clock = TRUE)
        sd <- sum_time((ms_sd + sd_sd) / 2, clock = TRUE)
        min <- sum_time(ms_min, + (sd_mean / 2), clock = TRUE)
        max <- sum_time(ms_max, + (sd_mean / 2), clock = TRUE)
        cat_(mean, sd, min, max)

        message <- paste0("\nSleep duration (SD_", i$suffix, ")\n\n")
        cat(message)
        cat_(sd_mean, sd_sd, sd_min, sd_max)

        message <- paste0("\nMid-sleep (MS", i$suffix, ")\n\n")
        cat(message)
        cat_(ms_mean, ms_sd, ms_min, ms_max)
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
#' ## Requirements
#'
#' This function works only in interactive mode.
#'
#' ## Guidelines
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
    # Check requirements -----

    if (!is_interactive()) {
        stop("This function can only be used in interactive mode.",
             call. = FALSE)
    }

    # Title message -----

    alert("\nMCTQ Shift distribution parameters\n")

    # Set values -----

    values <- list( # extracted from the base article
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

    # Compute and print variables -----

    for (i in values) {
        alert(i$title, combined_styles = c("black", "bold"))

        message <- paste0("Local time of preparing to sleep (SPrep_",
                          i$suffix, ")\n\n")
        cat(message)
        sprep_mean <- i$sprep_mean
        sprep_sd <- i$sprep_sd
        sprep_min <- sum_time(sprep_mean, - (3 * sprep_sd), clock = TRUE)
        sprep_max <- sum_time(sprep_mean, + (3 * sprep_sd), clock = TRUE)
        cat_(sprep_mean, sprep_sd, sprep_min, sprep_max)

        message <- paste0("\nSleep latency (SLat_", i$suffix, ")\n\n")
        cat(message)
        slat_mean <- i$slat_mean
        slat_sd <- i$slat_sd
        slat_min <- sum_time(slat_mean, - (3 * slat_sd))
        if (slat_min <= 0) slat_min <- hms::as_hms(0)
        slat_max <- sum_time(slat_mean, + (3 * slat_sd))
        cat_(slat_mean, slat_sd, slat_min, slat_max)

        message <- paste0("\nSleep onset (SO_", i$suffix, ")\n\n")
        cat(message)
        mean <- sum_time(sprep_mean, slat_mean, clock = TRUE)
        sd <- sum_time(sprep_sd + slat_sd)
        min <- sum_time(sprep_min, slat_min, clock = TRUE)
        max <- sum_time(sprep_max, slat_max, clock = TRUE)
        cat_(mean, sd, min, max)

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
        cat_(mean, sd, min, max)


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

alert <- mctq:::alert

cat_ <- function(mean, sd, min, max) {
    cat("Mean:", as.character(mean), "\n")
    cat("SD:", as.character(sd), "\n")
    cat("Min:", as.character(min), "\n")
    cat("Max:", as.character(max), "\n")
}

min_max <- function(mean, sd) {
    min <- sum_time(mean, - (3 * sd), clock = TRUE)
    max <- sum_time(mean, + (3 * sd), clock = TRUE)

    cat_(mean, sd, min, max)
}

# std_mctq_par()
# shift_mctq_par()
