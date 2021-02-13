#' Build a random MCTQ case
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `random_mctq` builds a fictional Munich Chronotype Questionnaire (MCTQ) case
#' composed by MCTQ basic/measurable variables.
#'
#' At the moment, __only the standard MCTQ is available__.
#'
#' @details
#'
#' The case structure (variable names and classes) are the same as the datasets
#' provided by the `mctq` package. See [mctq::std_mctq] to learn more.
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
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"` (default: `"standard"`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A named list with elements representing each MCTQ basic/measurable
#'   variable of the model indicated in `model`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' random_mctq("standard")
#' random_mctq("shift")
#' random_mctq("micro")
#' }
random_mctq <- function(model = "standard", quiet = FALSE) {
    if (!is_namespace_loaded("stats")) {
        stop("This function requires the 'stats' package to run. ",
             'You can install it by running: \n \n',
             'install.packages("stats") \n', call. = FALSE)
    }

    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))
    checkmate::assert_flag(quiet)

    if (model %in% c("std", "standard")) {
        shush(message("\nModel: Standard MCTQ\n"), quiet = quiet)
        random_std_mctq()
    }
}

random_std_mctq <- function() {
    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))

    # Create `work` and `wd` -----

    work <- sample(c(TRUE, FALSE), 1, prob = c(5, 1))
    wd <- sample(0:7, 1, prob = c(1, rep(2, 4), 10, 2, 1))
    if (work == FALSE) wd <- as.integer(NA)

    # Create `bt_w` and `bt_f` -----

    min <- as.numeric(hms::parse_hms("21:00:00"))
    max <- as.numeric(sum_time(hms::as_hms(min),
                               lubridate::dhours(10))) # 06:00:00
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hms("23:30:00")),
                         sd = as.numeric(lubridate::dhours(2)))
    bt_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                   prob = prob))

    for (i in seq(3)) { # Bias
        bt_f <- clock_roll(sample_time(min = min, max = max, by = by,
                                       prob = prob))
        check <- shortest_interval(bt_w, bt_f, "interval")
        check <- lubridate::int_end(check)
        if (hms::as_hms(check) == bt_f) break
    }

    # Create `sprep_w` and `sprep_f` -----

    min <- as.numeric(bt_w)
    max <- as.numeric(bt_w + lubridate::dhours(4))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(bt_w + lubridate::dhours(1)),
                         sd = as.numeric(lubridate::dhours(0.5)))
    sprep_w <- clock_roll(sample_time(min = min, max = max, by = by,
                                      prob = prob))

    min <- as.numeric(bt_f)
    max <- as.numeric(bt_f + lubridate::dhours(4))
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

    convert(sample, class, input_unit = "S")
}
