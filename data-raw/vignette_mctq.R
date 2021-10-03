# Source the file before running the function
# Don't forget to uncomment the `library` functions below

library(magrittr)

# library(checkmate)
# library(dplyr)
# library(mctq)
# library(readr)
# library(rlang)
# library(utils)

#' Build a fictional standard MCTQ raw dataset for vignette use
#'
#' @description
#'
#' `build_vignette_std_mctq()` builds a fictional raw dataset composed of
#' basic/measurable variables of the Munich Chronotype Questionnaire (MCTQ)
#' standard version to be used on vignette examples.
#'
#' The basis for this dataset is [mctq::std_mctq].
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `vignette_std_mctq` file to `"./inst/extdata/"` (default: `FALSE`).
#'
#' @return An invisible `tibble` with a raw standard MCTQ dataset.
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! :=
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(mctq::build_vignette_mctq())
#' }}
build_vignette_std_mctq <- function(write = FALSE) {
    checkmate::assert_flag(write)

    vignette_mctq <- suppressMessages(
        readr::read_csv(mctq::raw_data("std_mctq.csv"))) %>%
        dplyr::slice(2, 3, 8, 11, 38)

    vignette_mctq$ID <- seq(nrow(vignette_mctq))

    names(vignette_mctq) <- c("id", "work", "wd", "bt_w", "sprep_w", "slat_w",
                              "se_w", "si_w", "alarm_w", "wake_before_w",
                              "le_w", "bt_f", "sprep_f", "slat_f", "se_f",
                              "si_f", "alarm_f", "reasons_f", "reasons_why_f",
                              "le_f")

    for (i in c("work", "alarm_w", "wake_before_w", "reasons_f", "alarm_f")) {
        vignette_mctq <- vignette_mctq %>%
            dplyr::mutate(
                !!as.symbol(i) := dplyr::case_when(
                    tolower(!!as.symbol(i)) == "yes" ~ TRUE,
                    tolower(!!as.symbol(i)) == "no" ~ FALSE)
            )
    }

    if (isTRUE(write)) {
        if (!(dir.exists("./inst/extdata/"))) dir.create("./inst/extdata/")

        vignette_mctq %>%
            utils::write.csv(paste0("./inst/extdata/", "vignette_mctq", ".csv"),
                             row.names = FALSE,
                             quote = FALSE)
    }

    invisible(vignette_mctq)
}

# raw <- build_vignette_mctq()
