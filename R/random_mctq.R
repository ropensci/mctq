#' Build a random MCTQ case
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `random_mctq` builds a fictional Munich Chronotype Questionnaire (MCTQ) case
#' composed of MCTQ basic/measurable variables.
#'
#' This function is __for testing and learning purposes only__. Please don't
#' misuse it.
#'
#' @details
#'
#' The case structure (variable names and classes) are the same as the datasets
#' provided by the `mctq` package. See [mctq::std_mctq], [mctq::micro_mctq] and
#' [mctq::shift_mctq] to learn more.
#'
#' ## Requirements
#'
#' This function requires the [`stats`][stats::stats-package] package. This
#' won't be an issue for most people since the package comes with a standard R
#' installation.
#'
#' If you don't have the [`stats`][stats::stats-package] package, you can
#' install it with `install.packages("stats")`.
#'
#' ## Cases
#'
#' Random standard and micro MCTQ cases were created with the general
#' population in mind. The data was set to resemble the distribution parameters
#' shown in Roenneberg, Wirz-Justice, & Merrow (2003).
#'
#' MCTQ\eqn{^{Shift}}{ Shift} random cases were created based on the shift
#' configuration from "Study Site 1" shown in Vetter, Juda, & Roenneberg (2012).
#' The data was set to resemble the distribution parameters shown in Juda,
#' Vetter, & Roenneberg (2013).
#'
#' You can see more about the distribution parameters used
#' [here](https://github.com/gipso/mctq/blob/master/data-raw/random_mctq.R).
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, and `"micro"` (default: `"standard"`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A named `list` with elements representing each MCTQ basic/measurable
#'   variable of the model indicated in the `model` argument.
#'
#' @template references_f
#' @family utility functions
#' @importFrom lubridate %within%
#' @export
#'
#' @examples
#' \dontrun{
#' random_mctq("standard")
#' random_mctq("micro")
#' random_mctq("shift")}
random_mctq <- function(model = "standard", quiet = FALSE) {
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))
    checkmate::assert_flag(quiet)
    require_pkg("stats")

    if (model %in% c("std", "standard")) {
        random_std_mctq()
    } else if (model == "micro") {
        random_micro_mctq()
    } else if (model == "shift") {
        random_shift_mctq()
    }
}

random_std_mctq <- function() {
    ## R CMD Check variable bindings fix (see: <http://bit.ly/3bliuam>)
    work <- wd <- NULL
    bt_w <- sprep_w <- slat_w <- se_w <- si_w <- alarm_w <- NULL
    wake_before_w <- le_w <- NULL
    bt_f <- sprep_f <- slat_f <- se_f <- si_f <- alarm_f <- NULL
    reasons_f <- reasons_why_f <- le_f <- NULL

    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))
    envir <- environment()

    # Create `work` and `wd` -----

    work <- sample(c(TRUE, FALSE), 1, prob = c(10, 1))
    wd <- sample(0:7, 1, prob = c(1, rep(2, 4), 10, 2, 1))
    if (work == FALSE) wd <- as.integer(0)

    # Create `bt_w` and `bt_f` -----

    ## `bt_w` for workdays:
    ## min = ~ `so_w_min` - 01:00; max = ~ `so_w_max` - 01:00;
    ## mean = ~ `so_w_mean` - 01:00; sd = ~ `so_w_sd`

    ## `bt_f` for work-free days:
    ## min = ~ `so_f_min` - 01:00; max = ~ `so_f_max` - 02:00;
    ## mean = ~ `so_f_mean` - 02:00; sd = ~ `so_f_sd`

    values <- list(
        bt_w = list(
            name = "bt_w",
            min = hms::parse_hm("20:00"),
            max = hms::parse_hm("01:00"),
            mean = hms::parse_hm("22:30"),
            sd = hms::parse_hm("01:00")),
        bt_f = list(
            name = "bt_f",
            min = hms::parse_hm("19:15"),
            max = hms::parse_hm("03:25"),
            mean = hms::parse_hm("22:50"),
            sd = hms::parse_hm("01:30"))
    )

    lapply(values, sampler_2, by = by, envir = envir)

    # Create `sprep_w` and `sprep_f` -----

    values <- list(
        sprep_w = list(
            name = "sprep_w",
            min = bt_w,
            max = bt_w + hms::parse_hm("01:00"),
            mean = bt_w + hms::parse_hm("00:30"),
            sd = hms::parse_hm("00:30")),
        sprep_f = list(
            name = "sprep_f",
            min = bt_f,
            max = bt_f + hms::parse_hm("02:00"),
            mean = bt_f + hms::parse_hm("01:00"),
            sd = hms::parse_hm("01:00"))
    )

    lapply(values, sampler_3, y = "bt", by = by, envir = envir)

    # Create `slat_w` and `slat_f` -----

    values <- list(
        slat_w = list(
            name = "slat_w",
            min = hms::parse_hm("00:05"),
            max = hms::parse_hm("01:00"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("00:15")),
        slat_f = list(
            name = "slat_f",
            min = hms::parse_hm("00:05"),
            max = hms::parse_hm("02:00"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("00:15"))
    )

    lapply(values, sampler_4, by = by, envir = envir)

    # Create `se_w` and `se_f` -----

    ## `se_w` for workdays:
    ## min = `sprep_w` + `slat_w` + ~ `sd_w_min`;
    ## max = `sprep_w` + `slat_w` + ~ `sd_w_max`;
    ## mean = `sprep_w` + `slat_w` + ~ `sd_w_mean`; sd = ~ `sd_w_sd`

    ## `se_f` for work-free days:
    ## min = `sprep_f` + `slat_f` + ~ `sd_f_min`;
    ## max = `sprep_f` + `slat_f` + ~ `sd_f_max`;
    ## mean = `sprep_f` + `slat_f` + ~ `sd_f_mean`; sd = ~ `sd_f_sd`

    values <- list(
        se_w = list(
            name = "se_w",
            min = sprep_w + slat_w + hms::parse_hm("03:55"),
            max = sprep_w + slat_w + hms::parse_hm("10:50"),
            mean = sprep_w + slat_w + hms::parse_hm("07:20"),
            sd = hms::parse_hm("01:10")),
        se_f = list(
            name = "se_f",
            min = sprep_f + slat_f + hms::parse_hm("03:50"),
            max = sprep_f + slat_f + hms::parse_hm("13:05"),
            mean = sprep_f + slat_f + hms::parse_hm("08:25"),
            sd = hms::parse_hm("01:30"))
    )

    lapply(values, sampler_3, y = "bt", by = by, envir = envir)

    # Create si_w` and `si_f` -----

    values <- list(
        si_w = list(
            name = "si_w",
            min = hms::parse_hm("00:05"),
            max = hms::parse_hm("02:00"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("00:15")),
        si_f = list(
            name = "si_f",
            min = hms::parse_hm("00:05"),
            max = hms::parse_hm("02:00"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("00:15"))
    )

    lapply(values, sampler_4, by = by, envir = envir)

    # Create `le_w` and `le_f` -----

    values <- list(
        le_w = list(
            name = "le_w",
            min = hms::parse_hm("00:00"),
            max = hms::parse_hm("04:00"),
            mean = hms::parse_hm("01:30"),
            sd = hms::parse_hm("01:00")),
        le_f = list(
            name = "le_f",
            min = hms::parse_hm("00:00"),
            max = hms::parse_hm("06:00"),
            mean = hms::parse_hm("02:00"),
            sd = hms::parse_hm("01:00"))
    )

    lapply(values, sampler_4, by = by, envir = envir)

    # Create `alarm_w` and `wake_before_f` -----

    alarm_w <- sample(c(TRUE, FALSE), 1, prob = c(10, 1))
    wake_before_w <- sample(c(TRUE, FALSE), 1, prob = c(1, 10))
    if (isFALSE(alarm_w)) wake_before_w <- as.logical(NA)

    # Create `alarm_f`, `reasons_f`, and `reasons_why_f` -----

    alarm_f <- sample(c(TRUE, FALSE), 1, prob = c(1, 10))
    reasons_f <- sample(c(TRUE, FALSE), 1, prob = c(1, 5))
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
    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    shift_work <- wd <- so_w <- se_w <- so_f <- se_f <- NULL

    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))
    envir <- environment()

    # Create `shift_work` and `wd` -----

    shift_work <- sample(c(TRUE, FALSE), 1, prob = c(1, 15))
    wd <- sample(0:7, 1, prob = c(1, rep(2, 4), 10, 2, 1))

    # Create `so_w` and so_f` -----

    ## `so` values for workdays:
    ## min = ~ `so_w_min`; max = ~ `so_w_max`; mean = ~ `so_w_mean`;
    ## sd = ~ `so_w_sd`

    ## `so` values for work-free days:
    ## min = ~ `so_f_min`; max = ~ `se_f_min`; mean = ~ `so_f_mean`;
    ## sd = ~ `so_f_sd`

    values <- list(
        so_w = list(
            name = "so_w",
            min = hms::parse_hm("21:00"),
            max = hms::parse_hm("02:00"),
            mean = hms::parse_hm("23:30"),
            sd = hms::parse_hm("01:00")),
        so_f = list(
            name = "so_f",
            min = hms::parse_hm("20:15"),
            max = hms::parse_hm("05:25"),
            mean = hms::parse_hm("00:50"),
            sd = hms::parse_hm("01:30"))
    )

    lapply(values, sampler_2, by = by, envir = envir)

    # Create `se_w` and `se_f` -----

    ## `se` values for workdays:
    ## min = `so_w` + ~ `sd_w_min`; max = `so_w` + ~ `sd_w_max`;
    ## mean = `so_w` + ~ `sd_w_mean`; sd = ~ `sd_w_sd`

    ## `se` for work-free days:
    ## min = `so_f` + ~ `sd_f_min`; max = `so_f` + ~ `sd_f_max`;
    ## mean = `so_f` + ~ `sd_f_mean`; sd = ~ `sd_f_sd`

    values <- list(
        se_w = list(
            name = "se_w",
            min = so_w + hms::parse_hm("03:55"),
            max = so_w + hms::parse_hm("10:50"),
            mean = so_w + hms::parse_hm("07:20"),
            sd = hms::parse_hm("01:10")),
        se_f = list(
            name = "se_f",
            min = so_f + hms::parse_hm("03:50"),
            max = so_f + hms::parse_hm("13:05"),
            mean = so_f + hms::parse_hm("08:25"),
            sd = hms::parse_hm("01:30"))
    )

    lapply(values, sampler_3, y = "so", by = by, envir = envir)

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

    # R CMD Check variable bindings fix -----

    ## See: <http://bit.ly/3bliuam>

    n_w_m <- bt_w_m <- sprep_w_m <- slat_w_m <- se_w_m <- alarm_w_m <- NULL
    tgu_w_m <- nap_w_m <- napo_w_m <- nape_w_m <- reasons_w_m <- NULL
    reasons_why_w_m <- NULL
    n_f_m <- bt_f_m <- sprep_f_m <- slat_f_m <- se_f_m <- alarm_f_m <- NULL
    tgu_f_m <- nap_f_m <- napo_f_m <- nape_f_m <- reasons_f_m <- NULL
    reasons_why_f_m <- NULL
    n_w_e <- bt_w_e <- sprep_w_e <- slat_w_e <- se_w_e <- alarm_w_e <- NULL
    tgu_w_e <- nap_w_e <- napo_w_e <- nape_w_e <- reasons_w_e <- NULL
    reasons_why_w_e <- NULL
    n_f_e <- bt_f_e <- sprep_f_e <- slat_f_e <- se_f_e <- alarm_f_e <- NULL
    tgu_f_e <- nap_f_e <- napo_f_e <- nape_f_e <- reasons_f_e <- NULL
    reasons_why_f_e <- NULL
    n_w_n <- bt_w_n <- sprep_w_n <- slat_w_n <- se_w_n <- alarm_w_n <- NULL
    tgu_w_n <- nap_w_n <- napo_w_n <- nape_w_n <- reasons_w_n <- NULL
    reasons_why_w_n <- NULL
    n_f_n <- bt_f_n <- sprep_f_n <- slat_f_n <- se_f_n <- alarm_f_n <- NULL
    tgu_f_n <- nap_f_n <- napo_f_n <- nape_f_n <- reasons_f_n <- NULL
    reasons_why_f_n <- NULL

    # Set values -----

    by <- as.numeric(lubridate::dminutes(5))
    envir <- environment()

    # Create `sprep_w_*` and `sprep_f_*` -----

    values <- list(
        sprep_w_m = list(
            name = "sprep_w_m",
            min = hms::parse_hm("19:15"),
            max = hms::parse_hm("00:40"), # Changed
            mean = hms::parse_hm("22:30"),
            sd = hms::parse_hm("01:05")),
        sprep_w_e = list(
            name = "sprep_w_e",
            min = hms::parse_hm("21:40"),
            max = hms::parse_hm("02:35"), # Changed
            mean = hms::parse_hm("00:40"),
            sd = hms::parse_hm("01:00")),
        sprep_w_n = list(
            name = "sprep_w_n",
            min = hms::parse_hm("06:30"), # Changed
            max = hms::parse_hm("10:10"),
            mean = hms::parse_hm("07:20"),
            sd = hms::parse_hm("01:00")),
        sprep_f_m = list(
            name = "sprep_f_m",
            min = hms::parse_hm("19:50"),
            max = hms::parse_hm("03:45"),
            mean = hms::parse_hm("23:45"),
            sd = hms::parse_hm("01:20")),
        sprep_f_e = list(
            name = "sprep_f_e",
            min = hms::parse_hm("19:50"),
            max = hms::parse_hm("04:30"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("01:25")),
        sprep_f_n = list(
            name = "sprep_f_n",
            min = hms::parse_hm("17:45"),
            max = hms::parse_hm("07:35"),
            mean = hms::parse_hm("00:40"),
            sd = hms::parse_hm("02:20"))
    )

    lapply(values, sampler_2, by = by, envir = envir)

    # Create `bt_w_*` and `bt_f_*` -----

    min <- as.numeric(hms::parse_hm("00:00"))
    max <- as.numeric(hms::parse_hm("02:00"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:30")),
                         sd = as.numeric(hms::parse_hm("00:30")))

    for (i in c("bt_w_m", "bt_w_e", "bt_w_n", "bt_f_m", "bt_f_e", "bt_f_n")) {
        sample <- sample_time(min = min, max = max, by = by, prob = prob)
        name <- paste0("sprep_", str_extract_(i, "._.$"))
        assign(i, sum_time(get(name), - sample, circular = TRUE))
    }

    # Create `slat_w_*` and `slat_f_*` -----

    values <- list(
        slat_w_m = list(
            name = "slat_w_m",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("00:30"), # Changed
            mean = hms::parse_hm("00:20"),
            sd = hms::parse_hm("00:25")),
        slat_w_e = list(
            name = "slat_w_e",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("00:40"), # Changed
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:20")),
        slat_w_n = list(
            name = "slat_w_n",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("01:10"),
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:20")),
        slat_f_m = list(
            name = "slat_f_m",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("01:00"),
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:15")),
        slat_f_e = list(
            name = "slat_f_e",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("01:05"),
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:15")),
        slat_f_n = list(
            name = "slat_f_n",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("02:15"),
            mean = hms::parse_hm("00:25"),
            sd = hms::parse_hm("00:35"))
    )

    lapply(values, sampler_4, by = by, envir = envir)

    # Create `se_w_*` and `se_f_*` -----

    ## Transition times: 06:00, 14:00, and 22:00

    ## Bear in mind:
    ## `se_w_min` must be greater or equal than `sprep_max + slat_max` +
    ##  `sd_w_min`
    ## `se_w_max` + `tgu_w_max` must be less or equal than the
    ## `transition time` - `00:30`
    ## `sd` must be equal or greater than `max` - `min`

    ## `se` for workdays (if the assumptions above are true):
    ## min = `sprep_w` + `slat_w` + ~ `sd_w_min`;
    ## max = ~ `se_w`; mean = ~ `se_w`; sd = ~ `se_sd`

    ## `se` for work-free days:
    ## min = `sprep_f` + `slat_f` + ~ `sd_f_min`;
    ## max = `sprep_f` + `slat_f` + ~ `sd_f_max`
    ## mean = `sprep_f` + `slat_f` + ~ `sd_f_mean`; sd = ~ `sd_sd`

    values <- list(
        # se_w_m_min >= sprep_w_m_max + slat_w_m_max + sd_w_m_min
        # sum_time(hms::parse_hm("00:40"), hms::parse_hm("00:30"), `sd_w_m_min`)
        # se_w_m_max + tgu_w_m_max <= 06:00 - 00:30
        # sum_time(hms::parse_hm("05:00"), hms::parse_hm("00:30"))
        se_w_m = list(
            name = "se_w_m",
            min = sum_time(sprep_w_m, slat_w_m, hms::parse_hm("02:05"),
                           circular = TRUE),
            max = hms::parse_hm("05:00"), # Changed
            mean = hms::parse_hm("04:35"),
            sd = hms::parse_hm("00:35")),
        # se_w_e_min >= sprep_w_e_max + slat_w_e_max + sd_w_e_min
        # sum_time(hms::parse_hm("02:35"), hms::parse_hm("00:40"), `sd_w_e_min`)
        # se_w_e_max + tgu_w_e_max) <= 14:00 - 00:30
        # sum_time(hms::parse_hm("12:25"), hms::parse_hm("00:50"))
        se_w_e = list(
            name = "se_w_e",
            min = sum_time(sprep_w_e, slat_w_e, hms::parse_hm("03:40"),
                           circular = TRUE),
            max = hms::parse_hm("12:25"),
            mean = hms::parse_hm("08:25"),
            sd = hms::parse_hm("01:20")),
        # se_w_n_min >= sprep_w_n_max + slat_w_n_max + sd_w_n_min
        # sum_time(hms::parse_hm("10:10"), hms::parse_hm("01:10"), `sd_w_n_min`)
        # se_w_n_max + tgu_w_n_max <= 22:00 - 00:30
        # sum_time(hms::parse_hm("18:05"), hms::parse_hm("01:00"))
        se_w_n = list(
            name = "se_w_n",
            min = sum_time(sprep_w_n, slat_w_n, hms::parse_hm("02:00"), # Chan.
                           circular = TRUE),
            max = hms::parse_hm("18:05"),
            mean = hms::parse_hm("13:30"),
            sd = hms::parse_hm("01:30")),
        se_f_m = list(
            name = "se_f_m",
            min = sprep_f_m + slat_f_m + hms::parse_hm("03:20"),
            max = sprep_f_m + slat_f_m + hms::parse_hm("12:45"),
            mean = sprep_f_m + slat_f_m + hms::parse_hm("08:05"),
            sd = hms::parse_hm("01:35")),
        se_f_e = list(
            name = "se_f_e",
            min = sprep_f_e + slat_f_e + hms::parse_hm("04:15"),
            max = sprep_f_e + slat_f_e + hms::parse_hm("12:00"),
            mean = sprep_f_e + slat_f_e + hms::parse_hm("08:10"),
            sd = hms::parse_hm("01:20")),
        se_f_n = list(
            name = "se_f_n",
            min = sprep_f_n + slat_f_n + hms::parse_hm("02:15"),
            max = sprep_f_n + slat_f_n + hms::parse_hm("13:35"),
            mean = sprep_f_n + slat_f_n + hms::parse_hm("07:55"),
            sd = hms::parse_hm("01:55"))
    )

    lapply(values, sampler_2, by = by, envir = envir)

    # Create `tgu_w_*` and `tgu_f_*` -----

    values <- list(
        tgu_w_m = list(
            name = "tgu_w_m",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("00:30"), # Changed
            mean = hms::parse_hm("00:05"),
            sd = hms::parse_hm("00:10")),
        tgu_w_e = list(
            name = "tgu_w_e",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("00:50"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("00:15")),
        tgu_w_n = list(
            name = "tgu_w_n",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("01:00"),
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:15")),
        tgu_f_m = list(
            name = "tgu_f_m",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("01:25"),
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:25")),
        tgu_f_e = list(
            name = "tgu_f_e",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("00:45"),
            mean = hms::parse_hm("00:10"),
            sd = hms::parse_hm("00:10")),
        tgu_f_n = list(
            name = "tgu_f_n",
            min = hms::parse_hm("00:05"), # Changed
            max = hms::parse_hm("00:55"),
            mean = hms::parse_hm("00:15"),
            sd = hms::parse_hm("00:15"))
    )

    lapply(values, sampler_4, by = by, envir = envir)

    # Create `napo_w_*` and `napo_f_*` -----

    ## Transition times: 06:00, 14:00, and 22:00

    ## Bear in mind:
    ## `napo_w_min` must be greater or equal than the `transition time` +
    ## `01:30`
    ## `napo_f_min` must be greater or equal than `se_f_max + tgu_f_max` +
    ## `01:30` or `sprep_f_max` + `slat_f_max` + `sd_f_max` + tgu_f_max` + 01:30
    ## `napo_max` must be less or equal than `bt_min` - `01:30`
    ## `sd` must be equal or greater than `max` - `min`

    values <- list(
        # napo_w_m_min >= 06:00 + 01:30
        # napo_w_m_max <= bt_w_m_min - 01:30
        # sum_time(hms::parse_hm("19:15"), - hms::parse_hm("01:30"))
        napo_w_m = list(
            name = "napo_w_m",
            min = hms::parse_hm("07:30"),
            max = hms::parse_hm("17:45"),
            mean = interval_mean(hms::parse_hm("09:00"),
                                 hms::parse_hm("17:45")),
            sd = hms::parse_hm("01:30")),
        ## napo_w_e_min >= 14:00 + 01:30
        ## napo_w_e_max <= bt_w_e_min - 01:30
        ## sum_time(hms::parse_hm("21:40"), - hms::parse_hm("01:30"))
        napo_w_e = list(
            name = "napo_w_e",
            min = hms::parse_hm("15:30"),
            max = hms::parse_hm("20:10"),
            mean = interval_mean(hms::parse_hm("15:30"),
                                 hms::parse_hm("20:10")),
            sd = hms::parse_hm("01:30")),
        # napo_w_n_min >= 22:00 + 01:30
        # napo_w_n_max <= bt_w_n_min - 01:30
        # sum_time(hms::parse_hm("06:30"), - hms::parse_hm("01:30"))
        napo_w_n = list(
            name = "napo_w_n",
            min = hms::parse_hm("23:30"),
            max = hms::parse_hm("05:00"),
            mean = interval_mean(hms::parse_hm("23:30"),
                                 hms::parse_hm("05:00")),
            sd = hms::parse_hm("01:30")),
        napo_f_m = list(
            name = "napo_f_m",
            min = sum_time(se_f_m, tgu_f_m, hms::parse_hm("01:30")),
            max = sum_time(bt_f_m, - hms::parse_hm("01:30")),
            mean = interval_mean(hms::as_hms(se_f_m + tgu_f_m), bt_f_m),
            sd = hms::parse_hm("01:30")),
        napo_f_e = list(
            name = "napo_f_e",
            min = sum_time(se_f_e, tgu_f_e, hms::parse_hm("01:30")),
            max = sum_time(bt_f_e, - hms::parse_hm("01:30")),
            mean = interval_mean(hms::as_hms(se_f_e + tgu_f_e), bt_f_e),
            sd = hms::parse_hm("01:30")),
        napo_f_n = list(
            name = "napo_f_n",
            min = sum_time(se_f_n, tgu_f_n, hms::parse_hm("01:30")),
            max = sum_time(bt_f_n, - hms::parse_hm("01:30")),
            mean = interval_mean(hms::as_hms(se_f_n + tgu_f_n), bt_f_n),
            sd = hms::parse_hm("01:30"))
    )

    lapply(values, sampler_1, by = by, envir = envir)

    # Create `nape_w_*` and `nape_f_*` -----

    min <- as.numeric(hms::parse_hm("00:10"))
    max <- as.numeric(hms::parse_hm("01:00"))
    prob <- stats::dnorm(seq(min, max, by),
                         mean = as.numeric(hms::parse_hm("00:30")),
                         sd = as.numeric(hms::parse_hm("00:15")))

    for (i in c("nape_w_m", "nape_w_e", "nape_w_n", "nape_f_m", "nape_f_e",
                "nape_f_n")) {
        name <- get(paste0("napo_", str_extract_(i, "._.$")))
        sample <- sample_time(min = min, max = max, by = by, prob = prob)
        assign(i, sum_time(name, sample, circular = TRUE))
    }

    # Create `nap_w_*` and `nap_f_*` -----

    for (i in c("nap_w_m", "nap_w_e", "nap_w_n", "nap_f_m", "nap_f_e",
                "nap_f_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 1)))

        if (isFALSE(get(i))) {
            name <- paste0("napo_", str_extract_(i, "._.$"))
            assign(name, hms::as_hms(NA))
            name <- paste0("nape_", str_extract_(i, "._.$"))
            assign(name, hms::as_hms(NA))
        }
    }

    # Create `alarm_w_*` and `alarm_f_*` -----

    alarm_w_m <- sample(c(TRUE, FALSE), 1, prob = c(10, 1))
    alarm_w_e <- sample(c(TRUE, FALSE), 1, prob = c(5, 1))
    alarm_w_n <- sample(c(TRUE, FALSE), 1, prob = c(1, 1))

    for (i in c("alarm_f_m", "alarm_f_e", "alarm_f_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 10)))
    }

    # Create `reasons_w_*`, `reasons_f_*`, `reasons_why_w`, and
    # `reasons_why_f` -----

    for (i in c("reasons_why_w_m", "reasons_why_w_e", "reasons_why_w_n",
                "reasons_why_f_m", "reasons_why_f_e", "reasons_why_f_n")) {
        assign(i, sample(c("Child(ren)/pet(s)", "Hobbies"), 1))
    }

    for (i in c("reasons_w_m", "reasons_w_e", "reasons_w_n",
                "reasons_f_m", "reasons_f_e", "reasons_f_n")) {
        assign(i, sample(c(TRUE, FALSE), 1, prob = c(1, 5)))

        if (isFALSE(get(i))) {
            name <- paste0("reasons_why_", str_extract_(i, "._.$"))
            assign(name, as.character(NA))
        }
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
        reasons_w_m = reasons_w_m,
        reasons_why_w_m = reasons_why_w_m,
        nap_w_m = nap_w_m,
        napo_w_m = napo_w_m,
        nape_w_m = nape_w_m,

        n_f_m = as.integer(n_f[1]),
        bt_f_m = bt_f_m,
        sprep_f_m = sprep_f_m,
        slat_f_m = slat_f_m,
        se_f_m = se_f_m,
        tgu_f_m = tgu_f_m,
        alarm_f_m = alarm_f_m,
        reasons_f_m = reasons_f_m,
        reasons_why_f_m = reasons_why_f_m,
        nap_f_m = nap_f_m,
        napo_f_m = napo_f_m,
        nape_f_m = nape_f_m,

        n_w_e = as.integer(n_w[2]),
        bt_w_e = bt_w_e,
        sprep_w_e = sprep_w_e,
        slat_w_e = slat_w_e,
        se_w_e = se_w_e,
        tgu_w_e = tgu_w_e,
        alarm_w_e = alarm_w_e,
        reasons_w_e = reasons_w_e,
        reasons_why_w_e = reasons_why_w_e,
        nap_w_e = nap_w_e,
        napo_w_e = napo_w_e,
        nape_w_e = nape_w_e,

        n_f_e = as.integer(n_f[2]),
        bt_f_e = bt_f_e,
        sprep_f_e = sprep_f_e,
        slat_f_e = slat_f_e,
        se_f_e = se_f_e,
        tgu_f_e = tgu_f_e,
        alarm_f_e = alarm_f_e,
        reasons_f_e = reasons_f_e,
        reasons_why_f_e = reasons_why_f_e,
        nap_f_e = nap_f_e,
        napo_f_e = napo_f_e,
        nape_f_e = nape_f_e,

        n_w_n = as.integer(n_w[3]),
        bt_w_n = bt_w_n,
        sprep_w_n = sprep_w_n,
        slat_w_n = slat_w_n,
        se_w_n = se_w_n,
        tgu_w_n = tgu_w_n,
        alarm_w_n = alarm_w_n,
        reasons_w_n = reasons_w_n,
        reasons_why_w_n = reasons_why_w_n,
        nap_w_n = nap_w_n,
        napo_w_n = napo_w_n,
        nape_w_n = nape_w_n,

        n_f_n = as.integer(n_f[3]),
        bt_f_n = bt_f_n,
        sprep_f_n = sprep_f_n,
        slat_f_n = slat_f_n,
        se_f_n = se_f_n,
        tgu_f_n = tgu_f_n,
        alarm_f_n = alarm_f_n,
        reasons_f_n = reasons_f_n,
        reasons_why_f_n = reasons_why_f_n,
        nap_f_n = nap_f_n,
        napo_f_n = napo_f_n,
        nape_f_n = nape_f_n
    )
}

normalize <- function(min, max, mean, ambiguity = 24) {
    classes <- c("Duration", "difftime", "hms")

    checkmate::assert_multi_class(min, classes)
    checkmate::assert_multi_class(max, classes)
    checkmate::assert_multi_class(mean, classes)
    assert_length_one(min)
    assert_length_one(max)
    assert_length_one(mean)
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))

    min <- clock_roll(hms::as_hms(as.numeric(min)))
    max <- clock_roll(hms::as_hms(as.numeric(max)))
    mean <- clock_roll(hms::as_hms(as.numeric(mean)))

    interval <- assign_date(min, max, ambiguity = ambiguity)
    min <- hms::as_hms(lubridate::int_start(interval))
    max <- hms::as_hms(as.numeric(min) + as.numeric(interval))

    check_1 <- lubridate::as_datetime(mean) %within% interval
    check_2 <- lubridate::as_datetime(as.numeric(mean + lubridate::ddays()))
    check_2 <- check_2 %within% interval

    if (check_1) {
        list(min = min, max = max, mean = hms::hms(as.numeric(mean)))
    } else if (check_2) {
        mean <- hms::hms(as.numeric(mean + lubridate::ddays()))
        list(min = min, max = max, mean = mean)
    } else {
        stop("'mean' can't be found within the interval between 'min' ",
             "and 'max'", call. = FALSE)
    }

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
    classes <- c("character", "numeric", "Duration", "difftime", "hms")
    names <- c("name", "min", "max", "mean", "sd")

    checkmate::assert_list(x)
    checkmate::assert_names(names(x), identical.to = names)
    lapply(x, checkmate::assert_multi_class, classes = classes)

    classes <- c("numeric", "Duration", "difftime", "hms")
    checkmate::assert_multi_class(by, classes)
    checkmate::assert_environment(envir)

    normalize <- normalize(x$min, x$max, x$mean)
    list2env(lapply(normalize, as.numeric), envir = environment())

    sd <- as.numeric(x$sd)
    by <- as.numeric(by)
    prob <- stats::dnorm(seq(min, max, by), mean = mean, sd = sd)
    sample <- clock_roll(sample_time(min = min, max = max, by = by,
                                     prob = prob))
    assign(x$name, sample, envir = envir)
}

sampler_2 <- function(x, by, envir) {
    classes <- c("character", "numeric", "Duration", "difftime", "hms")
    names <- c("name", "min", "max", "mean", "sd")

    checkmate::assert_list(x)
    checkmate::assert_names(names(x), identical.to = names)
    lapply(x, checkmate::assert_multi_class, classes = classes)

    classes <- c("numeric", "Duration", "difftime", "hms")
    checkmate::assert_multi_class(by, classes)
    checkmate::assert_environment(envir)

    normalize <- normalize(x$min, x$max, x$mean)
    list2env(lapply(normalize, as.numeric), envir = environment())

    sd <- as.numeric(x$sd)
    by <- as.numeric(by)
    prob <- stats::dnorm(seq(min, max, by), mean = mean, sd = sd)
    sample <- clock_roll(sample_time(min = min, max = max, by = by,
                                     prob = prob))
    assign(x$name, sample, envir = envir)

    if (grepl("_f$|_f_", x$name, perl = TRUE)) {
        work <- get(sub("_f", "_w", x$name), envir = envir)

        for (i in seq(3)) { # Bias
            free <- get(x$name, envir = envir)

            check <- shush(shorter_interval(free, work, class = "Interval"))
            check <- lubridate::int_end(check)
            if (hms::as_hms(check) == free) break

            sample <- clock_roll(sample_time(min = min, max = max, by = by,
                                             prob = prob))
            assign(x$name, sample, envir = envir)
        }
    }
}

sampler_3 <- function(x, y, by, envir) {
    classes <- c("character", "numeric", "Duration", "difftime", "hms")
    names <- c("name", "min", "max", "mean", "sd")

    checkmate::assert_list(x)
    checkmate::assert_names(names(x), identical.to = names)
    lapply(x, checkmate::assert_multi_class, classes = classes)
    checkmate::assert_string(y, min.chars = 1)

    classes <- c("numeric", "Duration", "difftime", "hms")
    checkmate::assert_multi_class(by, classes)
    checkmate::assert_environment(envir)

    normalize <- normalize(x$min, x$max, x$mean)
    list2env(lapply(normalize, as.numeric), envir = environment())

    sd <- as.numeric(x$sd)
    by <- as.numeric(by)
    prob <- stats::dnorm(seq(min, max, by), mean = mean, sd = sd)
    sample <- clock_roll(sample_time(min = min, max = max, by = by,
                                     prob = prob))
    assign(x$name, sample, envir = envir)

    if (grepl("_f$|_f_", x$name, perl = TRUE)) {
        x_work <- get(sub("_f", "_w", x$name), envir = envir)
        y_work <- get(paste0(y, "_w"), envir = envir)
        y_free <- get(paste0(y, "_f"), envir = envir)

        for (i in seq(3)) { # Bias
            x_free <- get(x$name, envir = envir)

            check_w <- shush(shorter_interval(x_work, y_work))
            check_f <- shush(shorter_interval(x_free, y_free))
            if (check_f >= check_w) break

            sample <- clock_roll(sample_time(min = min, max = max, by = by,
                                             prob = prob))
            assign(x$name, sample, envir = envir)
        }
    }
}

sampler_4 <- function(x, by, envir) {
    classes <- c("character", "numeric", "Duration", "difftime", "hms")
    names <- c("name", "min", "max", "mean", "sd")

    checkmate::assert_list(x)
    checkmate::assert_names(names(x), identical.to = names)
    lapply(x, checkmate::assert_multi_class, classes = classes)

    classes <- c("numeric", "Duration", "difftime", "hms")
    checkmate::assert_multi_class(by, classes)
    checkmate::assert_environment(envir)

    min <- as.numeric(x$min)
    max <- as.numeric(x$max)
    mean <- as.numeric(x$mean)
    sd <- as.numeric(x$sd)
    by <- as.numeric(by)
    prob <- stats::dnorm(seq(min, max, by), mean = mean, sd = sd)
    sample <- sample_time("Duration", min = min, max = max, by = by,
                          prob = prob)
    assign(x$name, sample, envir = envir)

    if (grepl("_f$|_f_", x$name, perl = TRUE)) {
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
