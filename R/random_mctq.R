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
#' population in mind. The data was set to fit the distributions shown in
#' Roenneberg, Wirz-Justice, & Merrow (2003).
#'
#' _MCTQ\eqn{^{Shift}}{ Shift}_ random cases were created based on the shift
#' configuration from "Study Site 1" shown in Vetter, Juda, & Roenneberg (2012)
#' (it's also present in Juda, Vetter, & Roenneberg (2013) supplemental
#' materials).
#'
#' _MCTQ\eqn{^{Shift}}{ Shift}_ can be applied to the general population,
#' considering a survey with conditional branching made for shift-workers
#' respondents, but it would be difficult to analyze or even to interpret the
#' data in this case, due to peculiarities of this version of the questionnaire.
#' Given that a fictitious dataset would not be of much use if it did not come
#' close to a real world application, we decided to stick with a configuration
#' that could represent the full potential of the scale.
#'
#' @section Standard and micro MCTQ distribution parameters:
#'
#' Based on the general sample distributions shown in Roenneberg, Wirz-Justice,
#' & Merrow (2003) (Table 1, Figure 2, and Figure 7) and assuming normal
#' distributions where the minimal and maximum values are, respectively,
#' __\eqn{-3 s}__ and __\eqn{+3 s}__ from the mean (where __\eqn{s}__ is the
#' standard deviation of the sample), the distribution values for workdays and
#' work-free days for `random_mctq(model = "standard")` and `random_mctq(model =
#' "micro")`are as follow.
#'
#' ## Notes
#'
#' * Please note that this is just a rough approximation, it by no means
#' represents the same distributions from the base article.
#'
#' * The distribution parameters from other variables not shown here (like sleep
#' latency) were created based on the experience of the package authors with
#' MCTQ data.
#'
#' * This values are just for reference while building the random cases. If you
#' group several random MCTQ cases, this numbers can have some variation.
#'
#' * If you are visualizing this documentation in plain text (`ASCII`), you may
#' have some trouble to understand the equations. If you want a better viewer,
#' you can see this documentation on the package
#' [website](https://gipsousp.github.io/mctq/reference/).
#'
#' ## Workdays
#'
#' * Midsleep (__\eqn{MSW}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \textrm{03:10:00}}{= 03:10:00}
#'   (extracted from the base article)
#'   * __\eqn{s}__ \eqn{= \textrm{00:50:00}}{= 00:50:00} (extracted from the
#'   base article)
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \overline{X}_{MSW} - (3 \times
#'   s_{MSW}) = \textrm{00:40:00}}{= Mean_MSW - (3 * s_MSW) = 00:40:00}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \overline{X}_{MSW} + (3 \times
#'   s_{MSW}) = \textrm{05:40:00}}{= Mean_MSW + (3 * s_MSW) = 05:40:00}
#'
#' * Sleep duration (__\eqn{SD}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \textrm{07:22:00}}{= 07:22:00}
#'   (extracted from the base article)
#'   * __\eqn{s}__ \eqn{= \textrm{01:09:00}}{= 01:09:00} (extracted from the
#'   base article)
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \overline{X}_{SD} - (3 \times
#'   s_{SD}) = \textrm{03:53:00}}{= Mean_SD - (3 * s_SD) = 03:53:00}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \overline{X}_{SD} + (3 \times
#'   s_{SD}) = \textrm{10:47:00}}{= Mean_SD + (3 * s_SD) = 10:47:00}
#'
#' * Sleep onset (__\eqn{SO}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \overline{X}_{MSW} -
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{23:29:00}}{= Mean_MSW - (Mean_SD /
#'   2) = 23:29:00}
#'   * __\eqn{s}__ \eqn{= \dfrac{s_{MSW} + s_{SD}}{2} = \textrm{00:59:30}}{=
#'   (s_MSW + s_SD) / 2 = 00:59:30}
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \textrm{Min}_{MSW} -
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{20:59:00}}{= Min_MSW - (Mean_SD / 2)
#'   = 20:59:00}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \textrm{Max}_{MSW} -
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{01:59:00}}{= Max_MSW - (Mean_SD / 2)
#'   = 01:59:00}
#'
#' * Sleep end (__\eqn{SE}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \overline{X}_{MSW} +
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{06:51:00}}{= Mean_MSW + (Mean_SD /
#'   2) = 06:51:00}
#'   * __\eqn{s}__ \eqn{= \dfrac{s_{MSW} + s_{SD}}{2} = \textrm{00:59:30}}{=
#'   (s_MSW + s_SD) / 2 = 00:59:30}
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \textrm{Min}_{MSW} +
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{04:21:00}}{= Min_MSW + (Mean_SD / 2)
#'   = 04:21:00}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \textrm{Max}_{MSW} +
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{09:21:00}}{= Max_MSW + (Mean_SD / 2)
#'   = 09:21:00}
#'
#' ## Work-free days
#'
#' * Midsleep (__\eqn{MSW}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \textrm{05:02:00}}{= 05:02:00}
#'   (extracted from the base article)
#'   * __\eqn{s}__ \eqn{= \textrm{01:32:00}}{= 01:32:00} (extracted from the
#'   base article)
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \overline{X}_{MSW} - (3 \times
#'   s_{MSW}) = \textrm{00:26:00}}{= Mean_MSW - (3 * s_MSW) = 00:26:00}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \overline{X}_{MSW} + (3 \times
#'   s_{MSW}) = \textrm{09:38:00}}{= Mean_MSW + (3 * s_MSW) = 09:38:00}
#'
#' * Sleep duration (__\eqn{SD}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \textrm{08:27:00}}{= 08:27:00}
#'   (extracted from the base article)
#'   * __\eqn{s}__ \eqn{= \textrm{01:32:00}}{= 01:32:00} (extracted from the
#'   base article)
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \overline{X}_{SD} - (3 \times
#'   s_{SD}) = \textrm{03:51:00}}{= Mean_SD - (3 * s_SD) = 03:51:00}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \overline{X}_{SD} + (3 \times
#'   s_{SD}) = \textrm{13:03:00}}{= Mean_SD + (3 * s_SD) = 13:03:00}
#'
#' * Sleep onset (__\eqn{SO}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \overline{X}_{MSW} -
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{00:48:30}}{= Mean_MSW - (Mean_SD /
#'   2) = 00:48:30}
#'   * __\eqn{s}__ \eqn{= \dfrac{s_{MSW} + s_{SD}}{2} = \textrm{01:32:00}}{=
#'   (s_MSW + s_SD) / 2 = 01:32:00}
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \textrm{Min}_{MSW} -
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{20:12:30}}{= Min_MSW - (Mean_SD / 2)
#'   = 20:12:30}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \textrm{Max}_{MSW} -
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{05:24:30}}{= Max_MSW - (Mean_SD / 2)
#'   = 05:24:30}
#'
#' * Sleep end (__\eqn{SE}__)
#'   * __\eqn{\overline{X}}{Mean}__ \eqn{= \overline{X}_{MSW} +
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{09:15:30}}{= Mean_MSW + (Mean_SD /
#'   2) = 09:15:30}
#'   * __\eqn{s}__ \eqn{= \dfrac{s_{MSW} + s_{SD}}{2} = \textrm{01:32:00}}{=
#'   (s_MSW + s_SD) / 2 = 01:32:00}
#'   * __\eqn{\textrm{Min}}{Min}__ \eqn{= \textrm{Min}_{MSW} +
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{04:39:30}}{= Min_MSW + (Mean_SD / 2)
#'   = 04:39:30}
#'   * __\eqn{\textrm{Max}}{Max}__ \eqn{= \textrm{Max}_{MSW} +
#'   \dfrac{\overline{X}_{SD}}{2} = \textrm{13:51:30}}{= Max_MSW + (Mean_SD / 2)
#'   = 13:51:30}
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"` (default: `"standard"`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A named list with elements representing each MCTQ basic/measurable
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
        shush(crayon_message("\nModel: Standard MCTQ\n", c("bold", "red")),
              quiet = quiet)
        random_std_mctq()
    } else if (model == "micro") {
        shush(crayon_message("\nModel: Micro MCTQ\n", c("bold", "red")),
              quiet = quiet)
        random_micro_mctq()
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

    # min = ~ min_so_w - 01:00; max = ~ max_so_w - 01:00;
    # mean = ~ mean_so_w - 01:00; sd = ~ s_so_w
    min <- as.numeric(hms::parse_hm("20:00"))
    max <- as.numeric(hms::parse_hm("01:00") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("22:30")),
                         sd = as.numeric(hms::parse_hm("01:00")))
    bt_w <- clock_roll(sample_time(min = min, max = max, by = by, prob = prob))

    # min = ~ min_so_f - 01:00; max = ~ max_so_f - 02:00;
    # mean = ~ mean_so_f - 02:00; sd = ~ s_so_f
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

    # min = sprep_w + slat_w + ~ min_sd_w; max = sprep_w + slat_w + ~ max_sd_w;
    # mean = sprep_w + slat_w + ~ mean_sd_w; sd = ~ s_sd_w
    min <- as.numeric(sprep_w + slat_w + hms::parse_hm("03:55"))
    max <- as.numeric(sprep_w + slat_w + hms::parse_hm("10:45"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_w + slat_w + hms::parse_hm("07:20")),
                         sd = hms::parse_hm("01:10"))
    se_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                   prob = prob))

    # min = sprep_f + slat_f + ~ min_sd_f; max = sprep_f + slat_f + ~ max_sd_f;
    # mean = sprep_f + slat_f + ~ mean_sd_f; sd = ~ s_sd_f
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
    reasons_f <- sample(c(TRUE, FALSE), 1)
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

    # min = ~ min_so_w; max = ~ max_so_w; mean = ~ mean_so_w; sd = ~ s_so_w
    min <- as.numeric(hms::parse_hm("21:00"))
    max <- as.numeric(hms::parse_hm("02:00") + lubridate::ddays())
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("23:30")),
                         sd = as.numeric(hms::parse_hm("01:00")))
    so_w <- clock_roll(sample_time(min = min, max = max, by = by, prob = prob))

    # min = ~ min_so_f; max = ~ max_so_f; mean = ~ mean_so_f; sd = ~ s_so_f
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

    # min = so_w + ~ min_sd_w; max = so_w + ~ max_sd_w;
    # mean = so_w + ~ mean_sd_w; sd = ~ s_sd_w
    min <- as.numeric(so_w + hms::parse_hm("03:55"))
    max <- as.numeric(so_w + hms::parse_hm("10:45"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(so_w + hms::parse_hm("07:20")),
                         sd = as.numeric(hms::parse_hm("01:10")))
    se_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                   prob = prob))

    # min = so_f + ~ min_sd_f; max = so_f + ~ max_sd_f;
    # mean = so_f + ~ mean_sd_f; sd = ~ s_sd_f
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

    # Create `bt_w_*` and `bt_f_*` -----

    ## [stats::dnorm()] if the `mean` or `max` value represents the day after,
    ## add 1 day (86400s) to it

    min <- as.numeric(hms::parse_hm("20:00"))
    max <- as.numeric(hms::hms(min) + lubridate::dhours(5)) # 06:00
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("23:30")),
                         sd = as.numeric(lubridate::dhours(2)))
    bt_w_m <- clock_roll(sample_time(min = min, max = max, by = by, prob = prob))

    for (i in seq(3)) { # Bias
        bt_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check <- shortest_interval(bt_w, bt_f, "interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == bt_f) break
    }

    # Create `sprep_w` and `sprep_f` -----

    min <- as.numeric(bt_w)
    max <- as.numeric(bt_w + lubridate::dhours(2))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(bt_w + lubridate::dhours(1)),
                         sd = as.numeric(lubridate::dhours(0.5)))
    sprep_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                      prob = prob))

    min <- as.numeric(bt_f)
    max <- as.numeric(bt_f + lubridate::dhours(2))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(bt_f + lubridate::dhours(1)),
                         sd = as.numeric(lubridate::dhours(0.5)))

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
                         mean = as.numeric(lubridate::dminutes(20)),
                         sd = as.numeric(lubridate::dminutes(15)))
    slat_w <- sample_time("duration", min = min, max = max, by = by,
                          prob = prob)

    for (i in seq(3)) { # Bias
        slat_f <- sample_time("duration", min = min, max = max, by = by,
                              prob = prob)
        if (slat_f >= slat_w) break
    }

    si_w <- sample_time("duration", min = min, max = max, by = by,
                        prob = prob)

    for (i in seq(3)) { # Bias
        si_f <- sample_time("duration", min = min, max = max, by = by,
                            prob = prob)
        if (si_f >= si_w) break
    }

    # Create `se_w` and `se_f` -----

    min <- as.numeric(sprep_w + slat_w + lubridate::dhours(4))
    max <- as.numeric(sprep_w + slat_w + lubridate::dhours(14))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_w + + slat_w + lubridate::dhours(6)),
                         sd = as.numeric(lubridate::dhours(1.5)))
    se_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                   prob = prob))

    min <- as.numeric(sprep_f + slat_f + lubridate::dhours(4))
    max <- as.numeric(sprep_f + slat_f + lubridate::dhours(14))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(
                             sprep_f + slat_f + lubridate::dhours(9)),
                         sd = as.numeric(lubridate::dhours(1.5)))

    for (i in seq(3)) { # Bias
        se_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check_w <- shortest_interval(sprep_w, se_w)
        check_f <- shortest_interval(sprep_f, se_f)
        if (check_f >= check_w) break
    }

    # Create `le_w` and `le_f` -----

    min <-as.numeric(lubridate::dhours(0))
    max <- as.numeric(lubridate::dhours(12))
    prob <- stats::dnorm(seq(min, max, by), mean = lubridate::dhours(3),
                         sd = lubridate::dhours(1))
    le_w <- sample_time("duration", min = min, max = max, by = by, prob = prob)

    for (i in seq(3)) { # Bias
        le_f <- sample_time("duration", min = min, max = max, by = by,
                            prob = prob)
        if (le_f >= le_w) break
    }

    # Create `alarm_w` and `wake_before_f` -----

    alarm_w <- sample(c(TRUE, FALSE), 1, prob = c(5, 1))
    wake_before_w <- sample(c(TRUE, FALSE), 1, prob = c(1, 5))
    if (isFALSE(alarm_w)) wake_before_w <- as.logical(NA)

    # Create `alarm_f`, `reasons_f`, and `reasons_why_f` -----

    alarm_f <- sample(c(TRUE, FALSE), 1, prob = c(1, 5))
    reasons_f <- sample(c(TRUE, FALSE), 1)
    reasons_why_f <- sample(c("Child(ren)/pet(s)", "Hobbies"), 1)
    if (isFALSE(reasons_f)) reasons_why_f <- as.character(NA)

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
