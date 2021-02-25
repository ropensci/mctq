#' Build a random MCTQ case
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `random_mctq` builds a fictional Munich Chronotype Questionnaire (MCTQ) case
#' composed by MCTQ basic/measurable variables.
#'
#' @details
#'
#' The case structure (variable names and classes) are the same as the datasets
#' provided by the `mctq` package. See [mctq::std_mctq], [mctq::micro_mctq] and
#' `mctq::shift_mctq` to learn more.
#'
#' ## Requirements
#'
#' This function requires the [`stats`][stats::stats-package] package. This wont
#' be a issue for most people, since the package comes with a standard R
#' installation.
#'
#' If you don't have the [`stats`][stats::stats-package] package, you can
#' install it with `install.packages("stats")`.
#'
#' ## Cases
#'
#' Random _standard_ and _micro_ MCTQ cases were created with the general
#' population in mind. The data was set to resemble the distributions shown in
#' Roenneberg, Wirz-Justice, & Merrow (2003).
#'
#' _MCTQ\eqn{^{Shift}}{ Shift}_ random cases were created based on the shift
#' configuration from "Study Site 1" shown in Vetter, Juda, & Roenneberg (2012).
#' The data was set to resemble the distribution parameters shown in Juda,
#' Vetter, & Roenneberg (2013).
#'
#' You can see more about the distribution parameters used at
#' <https://github.com/gipsousp/mctq/blob/master/data-raw/random_mctq.R>.
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"` (default: `"standard"`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A named `list` with elements representing each MCTQ basic/measurable
#'   variable of the model indicated in `model`.
#'
#' @template references_f
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' random_mctq("standard")
#' random_mctq("micro")
#' random_mctq("shift")}
random_mctq <- function(model = "standard", quiet = FALSE) {
    if (!is_namespace_loaded("stats")) {
        stop("This function requires the 'stats' package to run. ",
             'You can install it by running: \n \n',
             'install.packages("stats") \n', call. = FALSE)
    }

    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))
    checkmate::assert_flag(quiet)

    if (model %in% c("std", "standard")) {
        shush(alert("\nModel: Standard MCTQ\n",
                    combined_styles = c("bold", "red")), quiet = quiet)
        random_std_mctq()
    } else if (model == "micro") {
        shush(alert("\nModel: Micro MCTQ\n",
                    combined_styles = c("bold", "red")), quiet = quiet)
        random_micro_mctq()
    } else if (model == "shift") {
        shush(alert("\nModel: MCTQ Shift\n",
                    combined_styles = c("bold", "red")), quiet = quiet)
        random_shift_mctq()
    }
}

random_std_mctq <- function() {
    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))

    # Create `work` and `wd` -----

    work <- sample(c(TRUE, FALSE), 1, prob = c(10, 1))
    wd <- sample(0:7, 1, prob = c(1, rep(2, 4), 10, 2, 1))
    if (work == FALSE) wd <- as.integer(0)

    # Create `bt_w` and `bt_f` -----

    ## [stats::dnorm()] if the `mean` or `max` value represents the day after,
    ## add 1 day (86400s) to it

    ## min = ~ so_w_min - 01:00; max = ~ so_w_max - 01:00;
    ## mean = ~ so_w_mean - 01:00; sd = ~ so_w_sd
    min <- as.numeric(hms::parse_hm("20:00"))
    max <- as.numeric(hms::parse_hm("01:00") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("22:30")),
                         sd = as.numeric(hms::parse_hm("01:00")))
    bt_w <- clock_roll(sample_time(min = min, max = max, by = by, prob = prob))

    ## min = ~ so_f_min - 01:00; max = ~ so_f_max - 02:00;
    ## mean = ~ so_f_mean - 02:00; sd = ~ so_f_sd
    min <- as.numeric(hms::parse_hm("19:15"))
    max <- as.numeric(hms::parse_hm("03:25") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("22:50")),
                         sd = as.numeric(hms::parse_hm("01:30")))

    for (i in seq(3)) { # Bias
        bt_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check <- shortest_interval(bt_w, bt_f, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == bt_f) break
    }

    # Create `sprep_w` and `sprep_f` -----

    min <- as.numeric(bt_w)
    max <- as.numeric(bt_w + lubridate::dhours(1))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(bt_w + lubridate::dhours(0.5)),
                         sd = as.numeric(lubridate::dhours(0.5)))

    sprep_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                      prob = prob))

    min <- as.numeric(bt_f)
    max <- as.numeric(bt_f + lubridate::dhours(2))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(bt_f + lubridate::dhours(1)),
                         sd = as.numeric(lubridate::dhours(1)))

    for (i in seq(3)) { # Bias
        sprep_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                          prob = prob))
        check_w <- shortest_interval(bt_w, sprep_w)
        check_f <- shortest_interval(bt_f, sprep_f)
        if (check_f >= check_w) break
    }

    # Create `slat_w`, `slat_f`, `si_w`, and `si_f` -----

    min <- as.numeric(lubridate::dminutes(0))
    max <- as.numeric(lubridate::dminutes(120))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(lubridate::dminutes(10)),
                         sd = as.numeric(lubridate::dminutes(15)))

    slat_w <- sample_time("Duration", min = min, max = max, by = by,
                          prob = prob)

    si_w <- sample_time("Duration", min = min, max = max, by = by,
                        prob = prob)

    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(lubridate::dminutes(20)),
                         sd = as.numeric(lubridate::dminutes(15)))

    for (i in seq(3)) { # Bias
        slat_f <- sample_time("Duration", min = min, max = max, by = by,
                              prob = prob)
        if (slat_f >= slat_w) break
    }

    for (i in seq(3)) { # Bias
        si_f <- sample_time("Duration", min = min, max = max, by = by,
                            prob = prob)
        if (si_f >= si_w) break
    }

    # Create `se_w` and `se_f` -----

    ## min = sprep_w + slat_w + ~ sd_w_min; max = sprep_w + slat_w + ~ sd_w_max;
    ## mean = sprep_w + slat_w + ~ sd_w_mean; sd = ~ sd_w_sd
    min <- as.numeric(sprep_w + slat_w + hms::parse_hm("03:55"))
    max <- as.numeric(sprep_w + slat_w + hms::parse_hm("10:50"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_w + slat_w + hms::parse_hm("07:20")),
                         sd = hms::parse_hm("01:10"))
    se_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                   prob = prob))

    ## min = sprep_f + slat_f + ~ sd_f_min; max = sprep_f + slat_f + ~ sd_f_max;
    ## mean = sprep_f + slat_f + ~ sd_f_mean; sd = ~ sd_f_sd
    min <- as.numeric(sprep_f + slat_f + hms::parse_hm("03:50"))
    max <- as.numeric(sprep_f + slat_f + hms::parse_hm("13:05"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_f + slat_f + hms::parse_hm("08:25")),
                         sd = as.numeric(hms::parse_hm("01:30")))

    for (i in seq(3)) { # Bias
        se_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check_w <- shortest_interval(sprep_w, se_w)
        check_f <- shortest_interval(sprep_f, se_f)
        if (check_f >= check_w) break
    }

    # Create `le_w` and `le_f` -----

    min <-as.numeric(lubridate::dhours(0))
    max <- as.numeric(lubridate::dhours(4))
    prob <- stats::dnorm(seq(min, max, by), mean = lubridate::dhours(1.5),
                         sd = lubridate::dhours(1))
    le_w <- sample_time("Duration", min = min, max = max, by = by, prob = prob)

    min <-as.numeric(lubridate::dhours(0))
    max <- as.numeric(lubridate::dhours(6))
    prob <- stats::dnorm(seq(min, max, by), mean = lubridate::dhours(2),
                         sd = lubridate::dhours(1))

    for (i in seq(3)) { # Bias
        le_f <- sample_time("duration", min = min, max = max, by = by,
                            prob = prob)
        if (le_f >= le_w) break
    }

    # Create `alarm_w` and `wake_before_f` -----

    alarm_w <- sample(c(TRUE, FALSE), 1, prob = c(10, 1))
    wake_before_w <- sample(c(TRUE, FALSE), 1, prob = c(1, 10))
    if (isFALSE(alarm_w)) wake_before_w <- as.logical(NA)

    # Create `alarm_f`, `reasons_f`, and `reasons_why_f` -----

    alarm_f <- sample(c(TRUE, FALSE), 1, prob = c(1, 10))
    reasons_f <- sample(c(TRUE, FALSE), 1, prob = c(1, 10))
    reasons_why_f <- sample(c("Child(ren)/pet(s)", "Hobbies"), 1)
    if (isFALSE(reasons_f)) reasons_why_f <- as.character(NA)

    # Create and return output -----

    list(
        work = work,
        wd = as.integer(wd),

        bt_w = bt_w,
        sprep_w = sprep_w,
        slat_w = slat_w,
        se_w = se_w,
        si_w = si_w,
        alarm_w = alarm_w,
        wake_before_w = wake_before_w,
        le_w = le_w,

        bt_f = bt_f,
        sprep_f = sprep_f,
        slat_f = slat_f,
        se_f = se_f,
        si_f = si_f,
        alarm_f = alarm_f,
        reasons_f = reasons_f,
        reasons_why_f = reasons_why_f,
        le_f = le_f
    )
}

random_micro_mctq <- function() {
    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))

    # Create `shift_work` and `wd` -----

    shift_work <- sample(c(TRUE, FALSE), 1, prob = c(1, 15))
    wd <- sample(0:7, 1, prob = c(1, rep(2, 4), 10, 2, 1))

    # Create `so_w` and so_f` -----

    ## [stats::dnorm()] if the `mean` or `max` value represents the day after,
    ## add 1 day (86400s) to it

    ## min = ~ so_w_min; max = ~ so_w_max; mean = ~ so_w_mean; sd = ~ so_w_sd
    min <- as.numeric(hms::parse_hm("21:00"))
    max <- as.numeric(hms::parse_hm("02:00") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("23:30")),
                         sd = as.numeric(hms::parse_hm("01:00")))
    so_w <- clock_roll(sample_time(min = min, max = max, by = by, prob = prob))

    ## min = ~ so_f_min; max = ~ so_f_max; mean = ~ so_f_mean; sd = ~ so_f_sd
    min <- as.numeric(hms::parse_hm("20:15"))
    max <- as.numeric(hms::parse_hm("05:25") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:50") +
                                               lubridate::ddays()),
                         sd = as.numeric(hms::parse_hm("01:30")))

    for (i in seq(3)) { # Bias
        so_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check <- shortest_interval(so_w, so_f, "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == so_f) break
    }

    # Create `se_w` and `se_f` -----

    ## min = so_w + ~ sd_w_min; max = so_w + ~ sd_w_max;
    ## mean = so_w + ~ sd_w_mean; sd = ~ sd_w_sd
    min <- as.numeric(so_w + hms::parse_hm("03:55"))
    max <- as.numeric(so_w + hms::parse_hm("10:50"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(so_w + hms::parse_hm("07:20")),
                         sd = as.numeric(hms::parse_hm("01:10")))
    se_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                   prob = prob))

    ## min = so_f + ~ sd_f_min; max = so_f + ~ sd_f_max;
    ## mean = so_f + ~ sd_f_mean; sd = ~ sd_f_sd
    min <- as.numeric(so_f + hms::parse_hm("03:50"))
    max <- as.numeric(so_f + hms::parse_hm("13:05"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(so_f + hms::parse_hm("08:25")),
                         sd = as.numeric(hms::parse_hm("01:30")))

    for (i in seq(3)) { # Bias
        se_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check_w <- shortest_interval(so_w, se_w)
        check_f <- shortest_interval(so_f, se_f)
        if (check_f >= check_w) break
    }

    # Create and return output -----

    list(
        shift_work = shift_work,
        wd = as.integer(wd),

        so_w = so_w,
        se_w = se_w,

        so_f = so_f,
        se_f = se_f
    )
}

random_shift_mctq <- function(n_w = c(n_w_m = 6, n_w_e = 4, n_w_n = 6),
                              n_f = c(n_f_m = 2, n_f_e = 2, n_f_n = 8)) {
    # Check arguments -----

    checkmate::assert_integerish(n_w, lower = 0, any.missing = FALSE, len = 3)
    checkmate::assert_integerish(n_f, lower = 0, any.missing = FALSE, len = 3)

    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))
    envir <- environment()

    # Create `sprep_w_*` and `sprep_f_*` -----

    ## [stats::dnorm()] if the `mean` or `max` value represents the day after,
    ## add 1 day (86400s) to it

    min <- as.numeric(hms::parse_hm("19:15"))
    max <- as.numeric(hms::parse_hm("01:40") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("22:30")),
                         sd = as.numeric(hms::parse_hm("01:05")))
    sprep_w_m <- clock_roll(sample_time(min = min, max = max, by = by,
                                     prob = prob))

    min <- as.numeric(hms::parse_hm("21:40"))
    max <- as.numeric(hms::parse_hm("03:35") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:40") +
                                               lubridate::ddays()),
                         sd = as.numeric(hms::parse_hm("01:00")))
    sprep_w_e <- clock_roll(sample_time(min = min, max = max, by = by,
                                     prob = prob))

    min <- as.numeric(hms::parse_hm("04:30"))
    max <- as.numeric(hms::parse_hm("10:10"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("07:20")),
                         sd = as.numeric(hms::parse_hm("01:00")))
    sprep_w_n <- clock_roll(sample_time(min = min, max = max, by = by,
                                     prob = prob))

    min <- as.numeric(hms::parse_hm("19:50"))
    max <- as.numeric(hms::parse_hm("03:45") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("23:45")),
                         sd = as.numeric(hms::parse_hm("01:20")))

    for (i in seq(3)) { # Bias
        sprep_f_m <- clock_roll(sample_time(min = min, max = max, by = by,
                                            prob = prob))
        check <- shortest_interval(sprep_w_m, sprep_f_m, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == sprep_f_m) break
    }

    min <- as.numeric(hms::parse_hm("19:50"))
    max <- as.numeric(hms::parse_hm("04:30") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:10") +
                                               lubridate::ddays()),
                         sd = as.numeric(hms::parse_hm("01:25")))

    for (i in seq(3)) { # Bias
        sprep_f_e <- clock_roll(sample_time(min = min, max = max, by = by,
                                            prob = prob))
        check <- shortest_interval(sprep_w_e, sprep_f_e, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == sprep_f_e) break
    }

    min <- as.numeric(hms::parse_hm("17:45"))
    max <- as.numeric(hms::parse_hm("07:35") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:40")),
                         sd = as.numeric(hms::parse_hm("02:20")))

    for (i in seq(3)) { # Bias
        sprep_f_n <- clock_roll(sample_time(min = min, max = max, by = by,
                                            prob = prob))
        check <- shortest_interval(sprep_w_n, sprep_f_n, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == sprep_f_n) break
    }

    # Create `bt_w_*` and `bt_f_*` -----

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("02:00"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:30")),
                         sd = as.numeric(hms::parse_hm("00:30")))

    for (i in c("bt_w_m", "bt_w_e", "bt_w_n", "bt_f_m", "bt_f_e", "bt_f_n")) {
        sample <- sample_time(min = min, max = max, by = by, prob = prob)
        x <- paste0("sprep_", str_extract_(i, "._.$"))
        assign(i, sum_time(get(x), - sample, clock = TRUE))
    }

    # Create `slat_w_*` and `slat_f_*` -----

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("01:30"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:20")),
                         sd = as.numeric(hms::parse_hm("00:25")))

    slat_w_m <- sample_time("Duration", min = min, max = max, by = by,
                            prob = prob)

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("01:10"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:20")))

    slat_w_e <- sample_time("Duration", min = min, max = max, by = by,
                            prob = prob)

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("01:10"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:20")))

    slat_w_n <- sample_time("Duration", min = min, max = max, by = by,
                            prob = prob)

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("01:00"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:15")))

    for (i in seq(3)) { # Bias
        slat_f_m <- sample_time("Duration", min = min, max = max, by = by,
                                prob = prob)
        if (slat_f_m >= slat_w_m) break
    }

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("01:05"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:15")))

    for (i in seq(3)) { # Bias
        slat_f_e <- sample_time("Duration", min = min, max = max, by = by,
                                prob = prob)
        if (slat_f_e >= slat_w_e) break
    }

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("02:15"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:25")),
                         sd = as.numeric(hms::parse_hm("00:35")))

    for (i in seq(3)) { # Bias
        slat_f_n <- sample_time("Duration", min = min, max = max, by = by,
                                prob = prob)
        if (slat_f_n >= slat_w_n) break
    }

    # Create `se_w_*` and `se_f_*` -----

    ## [stats::dnorm()] if the `mean` or `max` value represents the day after,
    ## add 1 day (86400s) to it

    ## Transition times: 06:00, 14:00, and 22:00

    ## se must be greater than sprep + slat; se + tgu cannot be greater than
    ## the transition time - 00:30

    ## min = sprep_w + slat_w + ~ sd_w_min; max = ~ se_w; mean = ~ se_w;
    ## sd = ~ se_sd

    min <- sum_time(sprep_w_m, slat_w_m, hms::parse_hm("01:45"), clock = TRUE)
    min <- as.numeric(min)
    max <- as.numeric(hms::parse_hm("05:00"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("04:35")),
                         sd = as.numeric(hms::parse_hm("00:35")))
    se_w_m <- clock_roll(sample_time(min = min, max = max, by = by,
                                        prob = prob))

    min <- sum_time(sprep_w_e, slat_w_e, hms::parse_hm("03:40"), clock = TRUE)
    min <- as.numeric(min)
    max <- as.numeric(hms::parse_hm("12:25"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("08:25")),
                         sd = as.numeric(hms::parse_hm("01:20")))
    se_w_e <- clock_roll(sample_time(min = min, max = max, by = by,
                                        prob = prob))

    min <- sum_time(sprep_w_n, slat_w_n, hms::parse_hm("01:35"), clock = TRUE)
    min <- as.numeric(min)
    max <- as.numeric(hms::parse_hm("18:05"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("13:30")),
                         sd = as.numeric(hms::parse_hm("01:30")))
    se_w_n <- clock_roll(sample_time(min = min, max = max, by = by,
                                        prob = prob))

    ## min = sprep_f + slat_f + ~ sd_f_min; max = sprep_f + slat_f + ~ sd_f_max
    ## mean = sprep_f + slat_f + ~ sd_f_mean; sd = ~ sd_sd

    min <- as.numeric(sprep_f_m + slat_f_m + hms::parse_hm("03:20"))
    max <- as.numeric(sprep_f_m + slat_f_m + hms::parse_hm("12:45"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_f_m + slat_f_m + hms::parse_hm("08:05")),
                         sd = as.numeric(hms::parse_hm("01:35")))

    for (i in seq(3)) { # Bias
        se_f_m <- clock_roll(sample_time(min = min, max = max, by = by,
                                            prob = prob))
        check <- shortest_interval(se_w_m, se_f_m, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == se_f_m) break
    }

    min <- as.numeric(sprep_f_e + slat_f_e + hms::parse_hm("04:15"))
    max <- as.numeric(sprep_f_e + slat_f_e + hms::parse_hm("12:00"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_f_e + slat_f_e + hms::parse_hm("08:10")),
                         sd = as.numeric(hms::parse_hm("01:20")))

    for (i in seq(3)) { # Bias
        se_f_e <- clock_roll(sample_time(min = min, max = max, by = by,
                                            prob = prob))
        check <- shortest_interval(se_w_e, se_f_e, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == se_f_e) break
    }

    min <- as.numeric(sprep_f_n + slat_f_n + hms::parse_hm("02:15"))
    max <- as.numeric(sprep_f_n + slat_f_n + hms::parse_hm("13:35"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_f_n + slat_f_n + hms::parse_hm("07:55")),
                         sd = as.numeric(hms::parse_hm("01:55")))

    for (i in seq(3)) { # Bias
        se_f_n <- clock_roll(sample_time(min = min, max = max, by = by,
                                            prob = prob))
        check <- shortest_interval(se_w_n, se_f_n, class = "Interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == se_f_n) break
    }

    # Create `tgu_w_*` and `tgu_f_*` -----

    values <- list(
        tgu_w_m = list(
            name = "tgu_w_m",
            min = as.numeric(hms::parse_hm("00:00")),
            max = as.numeric(hms::parse_hm("00:30")),
            mean = as.numeric(hms::parse_hm("00:05")),
            sd = as.numeric(hms::parse_hm("00:10"))),
        tgu_w_e = list(
            name = "tgu_w_e",
            min = as.numeric(hms::parse_hm("00:00")),
            max = as.numeric(hms::parse_hm("00:50")),
            mean = as.numeric(hms::parse_hm("00:10")),
            sd = as.numeric(hms::parse_hm("00:15"))),
        tgu_w_n = list(
            name = "tgu_w_n",
            min = as.numeric(hms::parse_hm("00:00")),
            max = as.numeric(hms::parse_hm("01:00")),
            mean = as.numeric(hms::parse_hm("00:15")),
            sd = as.numeric(hms::parse_hm("00:15"))),
        tgu_f_m = list(
            name = "tgu_f_m",
            min = as.numeric(hms::parse_hm("00:00")),
            max = as.numeric(hms::parse_hm("01:25")),
            mean = as.numeric(hms::parse_hm("00:15")),
            sd = as.numeric(hms::parse_hm("00:25"))),
        tgu_f_e = list(
            name = "tgu_f_e",
            min = as.numeric(hms::parse_hm("00:00")),
            max = as.numeric(hms::parse_hm("00:45")),
            mean = as.numeric(hms::parse_hm("00:10")),
            sd = as.numeric(hms::parse_hm("00:10"))),
        tgu_f_n = list(
            name = "tgu_f_n",
            min = as.numeric(hms::parse_hm("00:00")),
            max = as.numeric(hms::parse_hm("00:55")),
            mean = as.numeric(hms::parse_hm("00:15")),
            sd = as.numeric(hms::parse_hm("00:15")))
    )

    lapply(values, sampler_2, by = by, envir = envir)

    # Create `napo_w_*` and `napo_f_*` -----

    ## Transition times: 06:00, 14:00, and 22:00

    ## min = transition time + 03:00

    min <- as.numeric(hms::parse_hm("09:00"))
    max <- sum_time(bt_w_m, - hms::parse_hm("05:00"), clock = TRUE)
    max <- as.numeric(max)
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:10")))

    napo_w_m <- sample_time(min = min, max = max, by = by, prob = prob)

    min <- as.numeric(hms::parse_hm("15:00"))
    max <- sum_time(bt_w_e, - hms::parse_hm("05:00"), clock = TRUE)
    max <- as.numeric(max)
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:10")),
                         sd = as.numeric(hms::parse_hm("00:15")))

    napo_w_e <- sample_time(min = min, max = max, by = by, prob = prob)

    min <- as.numeric(hms::parse_hm("01:00"))
    max <- sum_time(bt_w_n, - hms::parse_hm("05:00"), clock = TRUE)
    max <- as.numeric(max)
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:15")))

    napo_w_n <- sample_time(min = min, max = max, by = by, prob = prob)

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- sum_time(bt_f_m, - hms::parse_hm("05:00"), clock = TRUE)
    max <- as.numeric(max)
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:25")))

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- sum_time(bt_f_e, - hms::parse_hm("05:00"), clock = TRUE)
    max <- as.numeric(max)
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:10")),
                         sd = as.numeric(hms::parse_hm("00:10")))

    min <- as.numeric(se + hms::parse_hm("00:00"))
    max <- sum_time(bt_f_n, - hms::parse_hm("05:00"), clock = TRUE)
    max <- as.numeric(max)
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:15")),
                         sd = as.numeric(hms::parse_hm("00:15")))

    # Create `nap_w_*` and `nap_f_*` -----

    for (i in c("nap_w_m", "nap_w_e", "nap_w_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 1)))
    }

    for (i in c("nap_f_m", "nap_f_e", "nap_f_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 1)))
    }

    # Create `alarm_w_*` and `alarm_f_*` -----

    for (i in c("alarm_w_m", "alarm_w_e", "alarm_w_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(10, 1)))
    }

    for (i in c("alarm_f_m", "alarm_f_e", "alarm_f_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 10)))
    }

    # Create `reasons_w_*`, `reasons_f_*`, `reasons_why_w`, and
    # `reasons_why_f` -----

    for (i in c("reasons_w_m", "reasons_w_e", "reasons_w_n",
                "reasons_f_m", "reasons_f_e", "reasons_f_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 10)))
    }

    for (i in c("reasons_why_w_m", "reasons_why_w_e", "reasons_why_w_n",
                "reasons_why_f_m", "reasons_why_f_e", "reasons_why_f_n")) {
        assign(i, sample(c("Child(ren)/pet(s)", "Hobbies"), 1))
    }

    for (i in c("reasons_w_m", "reasons_w_e", "reasons_w_n",
                "reasons_f_m", "reasons_f_e", "reasons_f_n")) {
        if (isFALSE(get(i))) {
            x <- paste0("reasons_why_", str_extract_("reasons_w_m", "._.$"))
            assign(x, as.character(NA))
        }
    }

    # Check inconsistencies

    if (sum_time(sprep_w_m, slat_w_m, clock = TRUE) > se_w_m) {
        stop("Critical error.", call. = FALSE)
    }

    # Create and return output -----

    list(
        n_w_m = as.integer(n_w[1]),
        bt_w_m = bt_w_m,
        sprep_w_m = sprep_w_m,
        slat_w_m = slat_w_m,
        se_w_m = se_w_m,
        tgu_w_m = tgu_w_m,
        alarm_w_m = alarm_w_m,
        nap_w_m = nap_w_m,
        napo_w_m = napo_w_m,
        nape_w_m = nape_w_m,
        reasons_w_m = reasons_w_m,
        reasons_why_w_m = reasons_why_w_m,

        n_f_m = as.integer(n_f[1]),
        bt_f_m = bt_f_m,
        sprep_f_m = sprep_f_m,
        slat_f_m = slat_f_m,
        se_f_m = se_f_m,
        tgu_f_m = tgu_f_m,
        alarm_f_m = alarm_f_m,
        nap_f_m = nap_f_m,
        napo_f_m = napo_f_m,
        nape_f_m = nape_f_m,
        reasons_f_m = reasons_f_m,
        reasons_why_f_m = reasons_why_f_m,

        n_w_e = as.integer(n_w[2]),
        bt_w_e = bt_w_e,
        sprep_w_e = sprep_w_e,
        slat_w_e = slat_w_e,
        se_w_e = se_w_e,
        tgu_w_e = tgu_w_e,
        alarm_w_e = alarm_w_e,
        nap_w_e = nap_w_e,
        napo_w_e = napo_w_e,
        nape_w_e = nape_w_e,
        reasons_w_e = reasons_w_e,
        reasons_why_w_e = reasons_why_w_e,

        n_f_e = as.integer(n_f[2]),
        bt_f_e = bt_f_e,
        sprep_f_e = sprep_f_e,
        slat_f_e = slat_f_e,
        se_f_e = se_f_e,
        tgu_f_e = tgu_f_e,
        alarm_f_e = alarm_f_e,
        nap_f_e = nap_f_e,
        napo_f_e = napo_f_e,
        nape_f_e = nape_f_e,
        reasons_f_e = reasons_f_e,
        reasons_why_f_e = reasons_why_f_e,

        n_w_n = as.integer(n_w[3]),
        bt_w_n = bt_w_n,
        sprep_w_n = sprep_w_n,
        slat_w_n = slat_w_n,
        se_w_n = se_w_n,
        tgu_w_n = tgu_w_n,
        alarm_w_n = alarm_w_n,
        nap_w_n = nap_w_n,
        napo_w_n = napo_w_n,
        nape_w_n = nape_w_n,
        reasons_w_n = reasons_w_n,
        reasons_why_w_n = reasons_why_w_n,

        n_f_n = as.integer(n_f[3]),
        bt_f_n = bt_f_n,
        sprep_f_n = sprep_f_n,
        slat_f_n = slat_f_n,
        se_f_n = se_f_n,
        tgu_f_n = tgu_f_n,
        alarm_f_n = alarm_f_n,
        nap_f_n = nap_f_n,
        napo_f_n = napo_f_n,
        nape_f_n = nape_f_n,
        reasons_f_n = reasons_f_n,
        reasons_why_f_n = reasons_why_f_n,
    )
}

sample_time <- function(class = "hms", min = hms::parse_hms("00:00:00"),
                        max = hms::parse_hms("23:59:59"),
                        by = lubridate::dminutes(5), size = 1,
                        replace = FALSE, prob = NULL) {
    classes <- c("Duration", "Period", "hms", "integer", "numeric")

    checkmate::assert_choice(tolower(class), tolower(classes))
    checkmate::assert_multi_class(min, classes)
    checkmate::assert_multi_class(max, classes)
    checkmate::assert_multi_class(by, classes)
    assert_length_one(min)
    assert_length_one(max)
    assert_length_one(by)
    checkmate::assert_flag(replace)
    checkmate::assert_number(size, lower = 0)
    checkmate::assert_numeric(prob, null.ok = TRUE)

    min <- as.numeric(min)
    max <- as.numeric(max)
    by <- as.numeric(by)

    if (size > length(seq(min, max, by)) && isFALSE(replace)) {
        stop("You cannot take a sample larger than the population ",
             "when 'replace = FALSE'", call. = FALSE)
    }

    sample <- sample(seq(min, max, by), size = size, replace = replace,
                     prob = prob)

    convert(sample, class, quiet = TRUE)
}

sampler_1 <- function(x, by, envir) {
    min <- x$min
    max <- x$max
    prob <- stats::dnorm(seq(min, max, by), mean = x$mean, sd = x$sd)
    sample <- sample_time("Duration", min = min, max = max, by = by,
                          prob = prob)

    assign(x$name, sample, envir = envir)

    if (grepl("_f", x$name)) {
        work <- get(sub("_f", "_w", x$name), envir = envir)

        for (i in seq(3)) { # Bias
            free <- get(x$name, envir = envir)

            check <- shush(shortest_interval(free, work, class = "Interval"))
            check <- lubridate::int_end(check)
            if (hms::as_hms(check) == free) break

            sample <- sample_time("Duration", min = min, max = max, by = by,
                                  prob = prob)

            assign(x$name, sample, envir = envir)
        }
    }
}

sampler_2 <- function(x, by, envir) {
    min <- x$min
    max <- x$max
    prob <- stats::dnorm(seq(min, max, by), mean = x$mean, sd = x$sd)
    sample <- sample_time("Duration", min = min, max = max, by = by, prob = prob)

    assign(x$name, sample, envir = envir)

    if (grepl("_f", x$name)) {
        work <- get(sub("_f", "_w", x$name), envir = envir)

        for (j in seq(3)) { # Bias
            free <- get(x$name, envir = envir)
            if (free >= work) break

            sample <- sample_time("Duration", min = min, max = max, by = by,
                             prob = prob)
            assign(x$name, sample, envir = envir)
        }
    }
}
