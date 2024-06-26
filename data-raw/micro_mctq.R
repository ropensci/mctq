# # Notes
#
# * Source the file before running the functions.
# * Don't forget to uncomment the 'library' functions below.

# library(checkmate)
# library(dplyr)
# library(rutils) # https://github.com/danielvartan/rutils
# library(hms)
# library(lubridate)
# library(mctq)
# library(rlang)
# library(usethis)
# library(utils)
# library(validate)

#' Build a fictional micro MCTQ raw dataset
#'
#' @description
#'
#' `build_micro_mctq()` builds a fictional raw dataset, __for testing and
#' learning purposes__, composed of basic/measurable variables of the Munich
#' ChronoType Questionnaire (MCTQ) standard version. See
#' [`?micro_mctq`][mctq::micro_mctq] to learn more.
#'
#' @param write (optional) a [`logical`][base::logical()] value indicating if
#'   the function must write a `micro_mctq.csv` file to `"./inst/extdata/"`
#'   (default: `FALSE`).
#' @param random_cases (optional) a [`logical`][base::logical()] value
#'   indicating if the function must add random MCTQ cases besides the core
#'   ones.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a raw micro MCTQ
#'   dataset.
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(build_micro_mctq())
#' }
#' }
build_micro_mctq <- function(write = FALSE, random_cases = TRUE) {
  # Check arguments -----

  checkmate::assert_flag(write)
  checkmate::assert_flag(random_cases)

  # Set IDs -----

  set.seed(1)
  reserved_id <- sample(50, 12)
  id <- seq(50)[!(seq(50) %in% reserved_id)]

  # Create cases -----

  ## Base respondent: sleeps less than the recommended for an adult on
  ##                  workdays and stretches during work-free days

  micro_mctq <- dplyr::tibble(
    `ID` = as.character(reserved_id[1]), # integer | [auto-increment]

    `SHIFT WORK` = "No", # logical | Yes/no
    `WORK DAYS` = "5", # integer | [0-7]

    `W SLEEP ONSET` = "01:45 AM", # hms | IMp [0-12h]
    `W SLEEP END` = "06:30 AM", # hms | IMp [0-12h]

    `F SLEEP ONSET` = "02:45 AM", # hms | IMp [0-12h]
    `F SLEEP END` = "12:00 PM" # hms | IMp [0-12h]
  )

  ## Add random cases

  format_logical <- function(x) {
    dplyr::case_when(
      x == TRUE ~ "Yes",
      x == FALSE ~ "No"
    )
  }

  format_hms <- function(x) {
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

  if (isTRUE(random_cases)) {
    for (i in id) {
      random_case <-
        random_mctq(model = "micro") |>
        dplyr::as_tibble() |>
        dplyr::transmute(
          `ID` = as.character(i),

          `SHIFT WORK` =  format_logical(.data$shift_work),
          `WORK DAYS` = as.character(.data$wd),

          `W SLEEP ONSET` = format_hms(.data$so_w),
          `W SLEEP END` = format_hms(.data$se_w),

          `F SLEEP ONSET` = format_hms(.data$so_f),
          `F SLEEP END` = format_hms(.data$se_f)
        )

      micro_mctq <- dplyr::bind_rows(micro_mctq, random_case)
    }
  }

  ## Presence of invalid values

  micro_mctq <-
    micro_mctq |>
    dplyr::add_row(
      `ID` = as.character(reserved_id[2]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "10", # integer | [0-7] # INVALID

      `W SLEEP ONSET` = "27:00 PM", # hms | IMp [0-12h] # INVALID
      `W SLEEP END` = "12:15 PM", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "34:00 AM", # hms | IMp [0-12h] # INVALID
      `F SLEEP END` = "14:12 PM" # hms | IMp [0-12h]
    ) |>

    ## Sleeps more on workdays than on work-free days

    dplyr::add_row(
      `ID` = as.character(reserved_id[3]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "2", # integer | [0-7]

      `W SLEEP ONSET` = "09:45 PM", # hms | IMp [0-12h]
      `W SLEEP END` = "09:15 AM", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "00:35 AM", # hms | IMp [0-12h]
      `F SLEEP END` = "06:00 AM" # hms | IMp [0-12h]
    ) |>

    ## Null MCTQ

    dplyr::add_row(
      `ID` = as.character(reserved_id[4]), # integer | [auto-increment]

      `SHIFT WORK` = "", # logical | Yes/No
      `WORK DAYS` = "", # integer | [0-7]

      `W SLEEP ONSET` = "", # hms | IMp [0-12h]
      `W SLEEP END` = "", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "", # hms | IMp [0-12h]
      `F SLEEP END` = "" # hms | IMp [0-12h]
    ) |>

    ## Did not answer workdays questions

    dplyr::add_row(
      `ID` = as.character(reserved_id[5]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "", # integer | [0-7]

      `W SLEEP ONSET` = "", # hms | IMp [0-12h]
      `W SLEEP END` = "", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "05:30 AM", # hms | IMp [0-12h]
      `F SLEEP END` = "15:00 PM" # hms | IMp [0-12h]
    ) |>

    ## All, or almost all, basic variables have the same values

    dplyr::add_row(
      `ID` = as.character(reserved_id[6]), # integer | [auto-increment]

      `SHIFT WORK` = "0", # logical | Yes/No
      `WORK DAYS` = "0", # integer | [0-7]

      `W SLEEP ONSET` = "0", # hms | IMp [0-12h]
      `W SLEEP END` = "0", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "0", # hms | IMp [0-12h]
      `F SLEEP END` = "0" # hms | IMp [0-12h]
    ) |>

    ## Works 7 days a week and didn't answer the work-free days section

    dplyr::add_row(
      `ID` = as.character(reserved_id[7]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "7", # integer | [0-7]

      `W SLEEP ONSET` = "11:40 PM", # hms | IMp [0-12h]
      `W SLEEP END` = "06:30 AM", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "", # hms | IMp [0-12h]
      `F SLEEP END` = "" # hms | IMp [0-12h]
    ) |>

    ## Suspicious values

    dplyr::add_row(
      `ID` = as.character(reserved_id[8]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "6", # integer | [0-7]

      `W SLEEP ONSET` = "02:30 AM", # hms | IMp [0-12h]
      `W SLEEP END` = "04:00 AM", # hms | IMp [0-12h] # SUSPICIOUS

      `F SLEEP ONSET` = "01:00 AM", # hms | IMp [0-12h]
      `F SLEEP END` = "03:00 AM" # hms | IMp [0-12h] # SUSPICIOUS
    ) |>

    ## Different formats

    dplyr::add_row(
      `ID` = as.character(reserved_id[9]), # integer | [auto-increment]

      `SHIFT WORK` = "false", # logical | Yes/No # AMBIGUOUS
      `WORK DAYS` = "5", # integer | [0-7]

      `W SLEEP ONSET` = "2315", # hms | IMp [0-12h] # AMBIGUOUS
      `W SLEEP END` = "7:15", # hms | IMp [0-12h] # AMBIGUOUS

      `F SLEEP ONSET` = "02:30 AM", # hms | IMp [0-12h]
      `F SLEEP END` = "10:00 AM" # hms | IMp [0-12h]
    ) |>

    ## Possible filling error

    dplyr::add_row(
      `ID` = as.character(reserved_id[10]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "6", # integer | [0-7]

      `W SLEEP ONSET` = "", # hms | IMp [0-12h]
      `W SLEEP END` = "", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "10:45 PM", # hms | IMp [0-12h]
      `F SLEEP END` = "06:00 AM" # hms | IMp [0-12h]
    ) |>

    ## Repeated workdays and work-free days values (possible carryover
    ## effect)

    dplyr::add_row(
      `ID` = as.character(reserved_id[11]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "5", # integer | [0-7]

      `W SLEEP ONSET` = "11:10 PM", # hms | IMp [0-12h]
      `W SLEEP END` = "07:00 AM", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "11:10 PM", # hms | IMp [0-12h]
      `F SLEEP END` = "07:00 AM" # hms | IMp [0-12h]
    ) |>

    ## Sleep onset is equal or greater than sleep end

    dplyr::add_row(
      `ID` = as.character(reserved_id[12]), # integer | [auto-increment]

      `SHIFT WORK` = "No", # logical | Yes/No
      `WORK DAYS` = "2", # integer | [0-7]

      `W SLEEP ONSET` = "01:00 AM", # hms | IMp [0-12h]
      `W SLEEP END` = "08:00 AM", # hms | IMp [0-12h]

      `F SLEEP ONSET` = "04:00 AM", # hms | IMp [0-12h]
      `F SLEEP END` = "04:00 AM", # hms | IMp [0-12h]
    )

  micro_mctq <-
    micro_mctq %>% # Don't change the pipe.
    dplyr::arrange(as.integer(.data$`ID`))

  # Write and return output -----

  if (isTRUE(write)) {
    if (!(dir.exists("./inst/extdata/"))) dir.create("./inst/extdata/")

    micro_mctq |>
      utils::write.csv(paste0("./inst/extdata/", "micro_mctq", ".csv"),
                       row.names = FALSE,
                       quote = FALSE)
  }

  invisible(micro_mctq)
}

#' Tidy `build_micro_mctq()` output
#'
#' @description
#'
#' `tidy_micro_mctq` tidy the output of `build_micro_mctq()`. See
#' [`?micro_mctq`][mctq::micro_mctq] to learn more.
#'
#' @details
#'
#' Here the process of _tiding_ a dataset is understood as transforming it in
#' input data, like described in Loo and Jonge (2018). It's a very similar
#' process of tiding data described in the workflow proposed by Wickham and
#' Grolemund (n.d.).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' `micro_mctq` data, run `validate_micro_mctq()`.
#'
#' To learn more about the concept of tidy data, see Wickham (2014) and
#' Wickham and Grolemund (n.d.).
#'
#' @param write (optional) a [`logical`][base::logical()] value indicating if
#'   the function must write a `micro_mctq.rda` file to `"./data/"` (default:
#'   `FALSE`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a tidied, but not
#'   validated, micro MCTQ dataset.
#'
#' @template references_e
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(tidy_micro_mctq())
#' }
#' }
tidy_micro_mctq <- function(write = FALSE) {
  # Check arguments -----

  checkmate::assert_flag(write)

  # Set values -----

  micro_mctq <- build_micro_mctq()

  # Clean NULL cases -----

  fix_character <- function(x) {
    x <- trimws(x)

    for (i in c("", "NA")) {
      x <- dplyr::na_if(x, i)
    }

    x
  }

  micro_mctq <-
    micro_mctq |>
    dplyr::mutate(dplyr::across(.fns = fix_character)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      length = dplyr::n_distinct(dplyr::c_across(-.data$ID))
    ) |>
    dplyr::mutate(dplyr::across(
      -.data$ID, #nolint
      .fns = ~ ifelse(length <= 2, NA, .x))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-length)

  # Convert columns -----

  orders <- c("IMp", "IMSp", "HM", "HMS")

  micro_mctq <-
    micro_mctq |>
    dplyr::transmute(
      id = as.integer(.data$`ID`),

      shift_work = dplyr::case_when(
        tolower(.data$`SHIFT WORK`) == "yes" ~ TRUE, # nolint
        tolower(.data$`SHIFT WORK`) == "false" ~ FALSE,
        tolower(.data$`SHIFT WORK`) == "no" ~ FALSE),
      wd = as.integer(.data$`WORK DAYS`),

      so_w = mctq:::shush(hms::as_hms(
        lubridate::parse_date_time(`W SLEEP ONSET`, orders)
      )),
      se_w = mctq:::shush(hms::as_hms(
        lubridate::parse_date_time(`W SLEEP END`, orders)
      )),

      so_f = mctq:::shush(hms::as_hms(
        lubridate::parse_date_time(`F SLEEP ONSET`, orders)
      )),
      se_f = mctq:::shush(hms::as_hms(
        lubridate::parse_date_time(`F SLEEP END`, orders)
      ))
    )

  # Write and output dataset -----

  if (isTRUE(write)) usethis::use_data(micro_mctq, overwrite = TRUE)

  invisible(micro_mctq)
}

#' Validate [mctq::tidy_micro_mctq()] output
#'
#' @description
#'
#' `validate_micro_mctq()` validates the output of `tidy_micro_mctq()`.
#' See [`?micro_mctq`][mctq::micro_mctq] to learn more.
#'
#' @details
#'
#' Here, the process of _validating_ a dataset is understood as detecting
#' invalid data, by checking whether data satisfies certain assumptions from
#' domain knowledge, to then,  removing or, if possible, fixing them. You can
#' find more about data validation and error location in Loo and Jonge (2018).
#'
#' This process can be considered as part of the process of transforming data,
#' described in the workflow proposed by Wickham and Grolemund (n.d.).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated micro MCTQ
#'   dataset.
#'
#' @inheritParams tidy_micro_mctq
#' @template references_d
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(validate_micro_mctq())
#' }
#' }
validate_micro_mctq <- function(write = FALSE) {
  # To do -----
  #
  # * Adapt this process by using `errorlocate` package with `validate`.

  # Check arguments -----

  checkmate::assert_flag(write)

  # Set values -----

  set.seed(1)
  reserved_id <- sample(50, 12)
  micro_mctq <- tidy_micro_mctq()

  # Do univariate validation -----

  validate_hms <- function(x) {
    dplyr::case_when(
      x == hms::parse_hm("24:00") ~ hms::parse_hm("00:00"),
      x >= hms::parse_hm("00:00") & x < hms::parse_hm("24:00") ~ x
    )
  }

  micro_mctq <-
    micro_mctq |>
    dplyr::mutate(
      wd = dplyr::case_when(
        validate::in_range(wd, min = 0, max = 7) ~ wd
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("so_w", "se_w", "so_f", "se_f")),
        validate_hms
      )
    )

  # Do multivariate validation -----

  for (i in c("_w", "_f")) {
    so_i <- paste0("so", i)
    se_i <- paste0("se", i)

    micro_mctq <-
      micro_mctq |>
      dplyr::mutate(
        sd_i = mctq::sdu(!!as.symbol(so_i), !!as.symbol(se_i)),
        dummy = dplyr::case_when(
          sd_i < lubridate::dhours(2) |
            sd_i > lubridate::dhours(18) ~ TRUE,
          TRUE ~ FALSE
        ),
        !!as.symbol(so_i) := dplyr::if_else(
          dummy, hms::as_hms(NA), !!as.symbol(so_i)
        ),
        !!as.symbol(se_i) := dplyr::if_else(
          dummy, hms::as_hms(NA), !!as.symbol(se_i)
        )
      ) |>
      dplyr::select(-dummy, -sd_i)
  }


  # Clean invalid cases -----

  ## Cases: "Suspicious values"

  invalid <- c(reserved_id[8])

  micro_mctq <-
    micro_mctq |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        -.data$id,
        .fns = ~ dplyr::if_else(
          .data$id %in% invalid, rutils:::na_as(.x), .x
        )
      ),
      dplyr::across(
        -c(id:shift_work),
        .fns = ~ dplyr::if_else(
          shift_work, rutils:::na_as(.x), .x
        )
      )
    ) |>
    dplyr::ungroup()


  # Write and output dataset -----

  if (isTRUE(write)) usethis::use_data(micro_mctq, overwrite = TRUE)

  invisible(micro_mctq)
}

#' Analyze `validate_micro_mctq()` output
#'
#' @description
#'
#' `analyse_micro_mctq()` computes and creates the non-measured MCTQ variables
#' based on the output of `validate_micro_mctq()`. See
#' [`?micro_mctq`][mctq::micro_mctq] to learn more.
#'
#' @details
#'
#' Computing and creating new variables is part of the process of producing
#' statistics, like described in Loo and Jonge (2018). It's also a part of the
#' process of transforming data, described in the workflow proposed by Wickham
#' and Grolemund (n.d.).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with all the variables
#'   proposed for a micro MCTQ dataset.
#'
#' @inheritParams tidy_micro_mctq
#' @inheritParams pretty_mctq
#' @template references_d
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(analyze_micro_mctq())
#' }
#' }
analyze_micro_mctq <- function(write = FALSE, round = TRUE, hms = FALSE) {
  # Check arguments -----

  checkmate::assert_flag(write)
  checkmate::assert_flag(round)
  checkmate::assert_flag(hms)

  # Set values -----

  micro_mctq <- validate_micro_mctq()

  # Create computed variables -----

  micro_mctq <-
    micro_mctq |>
    dplyr::mutate(
      fd = mctq::fd(wd),

      sd_w = mctq::sdu(so_w, se_w),
      msw = mctq::msl(so_w, sd_w),

      sd_f = mctq::sdu(so_f, se_f),
      msf = mctq::msl(so_f, sd_f),

      sd_week = mctq::sd_week(sd_w, sd_f, wd),
      msf_sc = mctq::msf_sc(
        msf, sd_w, sd_f, sd_week, rep(FALSE, length(id))
      ),
      sloss_week = mctq::sloss_week(sd_w, sd_f, wd),
      sjl_rel = mctq::sjl_rel(msw, msf),
      sjl = abs(sjl_rel),
      sjl_sc_rel = mctq::sjl_sc_rel(so_w, se_w, so_f, se_f),
      sjl_sc = abs(sjl_sc_rel)
    ) |>
    dplyr::relocate(
      id, shift_work, wd, fd,

      so_w, se_w, sd_w, msw,

      so_f, se_f, sd_f, msf,

      sd_week, sloss_week, msf_sc, sjl_rel, sjl, sjl_sc_rel, sjl_sc
    )

  # Fix missing sections -----

  ## See `vignette("missing_sections", "mctq")` to learn more.

  count_w <- length(names(micro_mctq)[grepl("_w$", names(micro_mctq))])
  count_f <- length(names(micro_mctq)[grepl("_f$", names(micro_mctq))])
  count_w <- count_w * (2 / 3)
  count_f <- count_f * (2 / 3)

  count_na <- function(x) {
    checkmate::assert_atomic(x)

    length(which(is.na(x)))
  }

  test <-
    micro_mctq |>
    dplyr::mutate(dplyr::across(.fns = as.character)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dummy_0_a = as.integer(wd) == 0,
      dummy_0_b = count_na(dplyr::c_across(dplyr::ends_with("_w"))) >= count_w,
      dummy_7_a = as.integer(wd) == 7,
      dummy_7_b = count_na(dplyr::c_across(dplyr::ends_with("_f"))) >= count_f,
      dummy_0 = dummy_0_a & dummy_0_b & dummy_7_b == FALSE,
      dummy_7 = dummy_7_a & dummy_7_b & dummy_0_b == FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dummy_0, dummy_7)

  micro_mctq <-
    dplyr::bind_cols(micro_mctq, test) |>
    dplyr::mutate(
      sd_week = dplyr::case_when(
        dummy_0 == TRUE ~ sd_f,
        dummy_7 == TRUE ~ sd_w,
        TRUE ~ sd_week
      ),
      msf_sc = dplyr::if_else(dummy_0, msf, msf_sc),
      sloss_week = dplyr::if_else(
        dummy_0, lubridate::dhours(0), sloss_week
      ),
      sjl_rel = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_rel),
      sjl = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl),
      sjl_sc_rel = dplyr::if_else(
        dummy_0, lubridate::dhours(0), sjl_sc_rel
      ),
      sjl_sc = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_sc)
    ) |>
    dplyr::select(-dummy_0, -dummy_7)

  # Make MCTQ pretty -----

  micro_mctq <-
    micro_mctq |>
    pretty_mctq(round = round, hms = hms)

  # Write and output dataset -----

  if (isTRUE(write)) usethis::use_data(micro_mctq, overwrite = TRUE)

  invisible(micro_mctq)
}

# raw <- build_micro_mctq()
# tidy <- tidy_micro_mctq()
# valid <- validate_micro_mctq()
# analysis <- analyze_micro_mctq()
