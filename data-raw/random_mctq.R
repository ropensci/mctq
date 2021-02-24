# Source the file before running the functions
# Don't forget to uncomment the `library` functions

# library(lubridate)
# library(hms)
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
#' The distributions were based on the general sample distributions shown in
#' Roenneberg, Wirz-Justice, & Merrow (2003) (Table 1, Figure 2, and Figure 7).
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
    ## Check requirements -----
    if (!is_interactive()) {
        stop("This function can only be used in interactive mode.",
             call. = FALSE)
    }

    ## Title message -----

    alert("\nStandard and micro MCTQ distribution parameters\n")

    ## Set values -----

    so_ <- function(mean_ms, mean_sd, sd_ms, sd_sd, min_ms, max_ms) {
        mean <- sum_time(mean_ms, - (mean_sd / 2), clock = TRUE)
        sd <- sum_time((sd_ms + sd_sd) / 2, clock = TRUE)
        min <- sum_time(min_ms, - (mean_sd / 2), clock = TRUE)
        max <- sum_time(max_ms, - (mean_sd / 2), clock = TRUE)

        cat_(mean, sd, min, max)
    }

    se_ <- function(mean_ms, mean_sd, sd_ms, sd_sd, min_ms, max_ms) {
        mean <- sum_time(mean_ms, + (mean_sd / 2), clock = TRUE)
        sd <- sum_time((sd_ms + sd_sd) / 2, clock = TRUE)
        min <- sum_time(min_ms, + (mean_sd / 2), clock = TRUE)
        max <- sum_time(max_ms, + (mean_sd / 2), clock = TRUE)

        cat_(mean, sd, min, max)
    }

    ## Compute workdays (W) variables -----

    alert("Workdays (W)\n", combined_styles = c("black", "bold"))

    ### Mid-sleep (MSW)

    cat("Mid-sleep (MSW)\n\n")

    mean_ms <- hms::as_hms("03:10:00") # extracted from the base article
    sd_ms <- hms::as_hms("00:50:00") # extracted from the base article
    min_ms <- sum_time(mean_ms, - (3 * sd_ms), clock = TRUE)
    max_ms <- sum_time(mean_ms, + (3 * sd_ms), clock = TRUE)

    cat_(mean_ms, sd_ms, min_ms, max_ms)

    ### Sleep duration (SD_W)

    cat("\nSleep duration (SD_W)\n\n")

    mean_sd <- hms::as_hms("07:22:00") # extracted from the base article
    sd_sd <- hms::as_hms("01:09:00") # extracted from the base article
    min_sd <- sum_time(mean_sd, - (3 * sd_sd), clock = TRUE)
    max_sd <- sum_time(mean_sd, + (3 * sd_sd), clock = TRUE)

    cat_(mean_sd, sd_sd, min_sd, max_sd)

    ### Sleep onset (SO_W)

    cat("\nSleep onset (SO_W)\n\n")

    so_(mean_ms, mean_sd, sd_ms, sd_sd, min_ms, max_ms)

    ### Sleep end (SE_W)

    cat("\nSleep end (SE_W)\n\n")

    se_(mean_ms, mean_sd, sd_ms, sd_sd, min_ms, max_ms)

    ## Compute work-free days (F) variables -----

    alert("\nWork-free days (F)\n", combined_styles = c("black", "bold"))

    ### Mid-sleep (MSF)

    cat("Mid-sleep (MSF)\n\n")

    mean_ms <- hms::as_hms("05:02:00") # extracted from the base article
    sd_ms <- hms::as_hms("01:32:00") # extracted from the base article
    min_ms <- sum_time(mean_ms, - (3 * sd_ms), clock = TRUE)
    max_ms <- sum_time(mean_ms, + (3 * sd_ms), clock = TRUE)

    cat_(mean_ms, sd_ms, min_ms, max_ms)

    ### Sleep duration (SD_F)

    cat("\nSleep duration (SD_F)\n\n")

    mean_sd <- hms::as_hms("08:27:00") # extracted from the base article
    sd_sd <- hms::as_hms("01:32:00") # extracted from the base article
    min_sd <- sum_time(mean_sd, - (3 * sd_sd), clock = TRUE)
    max_sd <- sum_time(mean_sd, + (3 * sd_sd), clock = TRUE)

    cat_(mean_sd, sd_sd, min_sd, max_sd)

    ### Sleep onset (SO_F)

    cat("\nSleep onset (SO_F)\n\n")

    so_(mean_ms, mean_sd, sd_ms, sd_sd, min_ms, max_ms)

    ### Sleep end (SE_F)

    cat("\nSleep end (SE_F)\n\n")

    se_(mean_ms, mean_sd, sd_ms, sd_sd, min_ms, max_ms)
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
#' The distributions were based on the distributions shown in Juda, Vetter, &
#' Roenneberg (2013) (Table 2 and Table 3) - including those who indicated to be
#' woken up involuntarily (by an alarm clock or other disturbances).
#'
#' We assume that all variables have normal distributions and that the minimal
#' and maximum values are, respectively, __\eqn{-3 s}__ and __\eqn{+3 s}__ from
#' the mean (where __\eqn{s}__ is the standard deviation of the sample).
#'
#' Please note that:
#'
#' * This is just a rough approximation, it by no means represents the same
#' distributions from the mentioned articles.
#'
#' * This distribution values include those who indicated to be woken up
#' involuntarily (by an alarm clock or other disturbances). This can shift these
#' values down.
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
    return(print("Under development"))

    ## Title message -----

    alert("\n\nMCTQ Shift distribution parameters\n")

    ## Compute "Between two morning shifts" (W_M) variables -----

    alert("Between two morning shifts\n", combined_styles = c("black", "bold"))

    ### Local time of preparing to sleep (SPrep_W_M)

    cat("Local time of preparing to sleep (SPrep_W_M)\n\n")

    mean <- hms::as_hms("22:28:00") # extracted from the base article
    sd <- hms::as_hms("01:04:00") # extracted from the base article
    min <- sum_time(mean, - (3 * sd), clock = TRUE)
    max <- sum_time(mean, + (3 * sd), clock = TRUE)

    cat_(mean, sd, min, max)

    ### Sleep latency (SLat_W_M)

    cat("\nSleep latency (SLat_W_M)\n\n")

    mean <- lubridate::dminutes(20.2) # extracted from the base article
    mea <- hms::as_hms(as.numeric(mean))
    sd <- lubridate::dminutes(23.7) # extracted from the base article
    sd <- hms::as_hms(as.numeric(sd))
    min <- sum_time(mean, - (3 * sd))
    if (min <= 0 ) min = hms::as_hms(0)
    max <- sum_time(mean, + (3 * sd))

    cat_(mean, sd, min, max)

    ### Sleep end (SE_W_M)

    cat("Sleep end (SE_W_M)\n\n")

    mean <- hms::as_hms("04:34:00") # extracted from the base article
    sd <- hms::as_hms("00:34:00") # extracted from the base article
    min <- sum_time(mean, - (3 * sd), clock = TRUE)
    max <- sum_time(mean, + (3 * sd), clock = TRUE)

    cat_(mean, sd, min, max)
}

alert <- mctq:::alert

cat_ <- function(mean, sd, min, max) {
    cat("Mean:", as.character(mean), "\n")
    cat("SD:", as.character(sd), "\n")
    cat("Min:", as.character(min), "\n")
    cat("Max:", as.character(max), "\n")
}

# std_mctq_par()
# shift_mctq_par()
