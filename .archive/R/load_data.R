#' Load a delimited or a `.RData` file to R
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `load_data()` is a wrapper function for [readr::read_delim()] to help simple
#' data loading. It can read any kind of delimited data in addition to `.RData`
#' files.
#'
#' If this function doesn't work for your file, we recommend using the
#' [readr::readr] package to load it. If you're using
#' [RStudio IDE](https://rstudio.com/), you can also go to `Import Dataset`,
#' on the environment tab, to load your data.
#'
#' @details
#'
#' ## `.csv` and `.tsv` files
#'
#' Common delimiters for `.csv`
#' ([Comma-Separated Values](http://bit.ly/3qby909)) are `,` and `;`. For
#' `.tsv` ([Tab-Separated Values](http://bit.ly/3oGCXu8)) use `"\t"`.
#'
#' ## `.RData` files
#'
#' `load_data()` can also read `.RData` files. In this case, only the `file`
#' argument is needed, all other arguments are ignored.
#'
#' If you need to read `.rda` files, use the [base::load()] function.
#'
#' @param file A string indicating the file address to load. If unassigned, a
#'   dialog window will open allowing browsing (may not work in some OSs).
#' @param delim (optional) a string containing the field separator in `file`
#'   (default: `","`).
#' @param na (optional) a character vector indicating values that must be
#'   interpreted as `NA` (default: `c("", " ", "NA")`).
#' @param col_types (optional) a `NULL` value or a [readr::cols()] specification
#'   to set how `file` columns must be treated. Check [readr::read_delim()] to
#'   learn more (default: `readr::cols(.default = "c")`, which imports all
#'   columns as `character`).
#' @param trim_ws (optional) a logical value indicating if leading and/or
#'   trailing whitespaces must be trimmed from each field (default: `TRUE`).
#' @param skip (optional) an integerish value, equal or greater than `0`,
#'   indicating the number of rows to skip when reading `file` (default: `0`).
#' @param skip_empty_rows (optional) a logical value indicating if blank rows
#'   must be ignored altogether. If this option is `TRUE`, then blank rows will
#'   not be represented at all. If it is `FALSE` then they will be represented
#'   by `NA` values in all the columns (default: `TRUE`).
#'
#' @return An invisible tibble with data imported from `file`.
#'
#' @family utility functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' data <- load_data(raw_data("std_mctq.csv"))
#' }
load_data <- function(file = as.character(tcltk::tkgetOpenFile()),
                      delim = ",",
                      na = c("", " ", "NA"),
                      col_types = readr::cols(.default = "c"),
                      trim_ws = TRUE,
                      skip = 0,
                      skip_empty_rows = TRUE) {
    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    checkmate::assert_string(delim)
    checkmate::assert_character(na, any.missing = FALSE)
    checkmate::assert_class(col_types, "col_spec", null.ok = TRUE)
    checkmate::assert_flag(trim_ws)
    checkmate::assert_count(skip)
    checkmate::assert_flag(skip_empty_rows)

    if (stringr::str_detect(file, "\\.RData$")) {
        invisible(readRDS(file))
    } else {
        out <- file %>%
            readr::read_delim(delim = delim,
                              na = na,
                              col_types = col_types,
                              trim_ws = trim_ws,
                              skip = skip,
                              skip_empty_rows = skip_empty_rows) %>%
            dplyr::as_tibble()

        invisible(out)
    }
}

#' Write a delimited or a `.RData` file
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `write_data()` is a wrapper function for [readr::write_delim()] to help with
#' simple data exporting. It can write any kind of delimited data in addition to
#' `.RData` files.
#'
#' `write_data()` also deals with some common annoyances, like the need to
#' mutate objects of class `Duration` or `Period` to a more convenient format
#' (`hms`) when exporting data to text.
#'
#' If this function doesn't work for your data, we recommend using the
#' [readr::readr] package to export it.
#'
#' @details
#'
#' ## `type` argument
#'
#' The `type` argument is only needed if a `.csv`, `.tsv` or `.RData` file
#' extension is not found in `file`. If one of this extensions is present,
#' `write_data()` will change the type value for the appropriated format.
#'
#' ## `.csv` and `.tsv` files
#'
#' Common delimiters for `.csv`
#' ([Comma-Separated Values](http://bit.ly/3qby909)) are `,` and `;`. For
#' `.tsv` ([Tab-Separated Values](http://bit.ly/3oGCXu8)) use `"\t"`.
#'
#' Note that if `type == "tsv"`, `write_data()` will invariably change the
#' delimiter for `"\t"`.
#'
#' ## `.RData` files
#'
#' `write_data()` can also write `.RData` files. In this case, the file will
#' contain the data in `data` with no changes.
#'
#' If you need to write `.rda` files, use the [base::save()] function.
#'
#' @param data A data frame.
#' @param file A string indicating the file address where to write `data`. If
#'   unassigned, a dialog window will open allowing browsing (may not work in
#'   some OSs).
#' @param type (optional) A `csv`, `tsv` or `RData` string indicating the type
#'   of file to write (default: `"csv"`).
#' @param delim (optional) A string indicating the field separator for writing
#'   delimiter-separated files (default: `","`).
#' @param na (optional) A string indicating the value for `NA`s. Missing values
#'   will never be quoted; strings with the same value as `NA` will always be
#'   quoted (default: `"NA"`).
#' @param append (optional) A logical value indicating if the function must
#'   append data in `file` or overwrite it. If `FALSE`, will overwrite existing
#'   file. If `TRUE`, will append to the existing file. In both cases, if the
#'   file does not exist a new file is created (default: `FALSE`).
#' @param col_names (optional) A logical value indicating if the column names
#'   must be placed at the top of the file. If not specified, `col_names` will
#'   take the opposite value given to `append` (default: `!append`).
#' @param decimal (optional) A `"."` or `","` (European format) string,
#'   indicating a new decimal separator when writing delimiter-separated values
#'   files (default: `"."`).
#'
#' @return Write `data` in an address indicated in `file`. No value is returned.
#'
#' @family utility functions
#' @importFrom dplyr across
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' write_data(std_mctq)
#' }
write_data <- function(data,
                       file = as.character(tcltk::tkgetSaveFile()),
                       type = "csv",
                       delim = ",",
                       na = "NA",
                       append = FALSE,
                       col_names = !append,
                       decimal = ".") {
    # Check arguments -----

    checkmate::assert_data_frame(data)
    checkmate::assert_string(file)
    checkmate::assert_choice(type, c("csv", "tsv", "RData"))
    checkmate::assert_string(delim)
    checkmate::assert_string(na)
    checkmate::assert_flag(append)
    checkmate::assert_flag(col_names)
    checkmate::assert_choice(decimal, c(".", ","))

    # R CMD Check variable bindings fix --------------------

    where <- NULL

    # Set values -----

    out <- data

    if (stringr::str_detect(file, "\\.RData$")) type <- "RData"
    if (stringr::str_detect(file, "\\.csv$")) type <- "csv"

    if (stringr::str_detect(file, "\\.tsv$")) {
        type <- "tsv"
        delim <- "\t"
    }

    # Prepare text output -----

    if (!decimal == ".") {
        out <- data %>% dplyr::mutate(
            across(where(is_numeric_), ~ swap_decimal(.x, quiet = TRUE)))
    }

    out <- out %>%
        dplyr::mutate(across(where(is.list), ~ dplyr::na_if(
            paste(sapply(.x, unlist), collapse = ","), "NA")),
               across(where(lubridate::is.duration), ~ convert(.x, "hms")),
               across(where(lubridate::is.period), ~ convert(.x, "hms")),
               across(where(lubridate::is.difftime), ~ convert(.x, "hms")),
               across(where(lubridate::is.POSIXt), ~
                          convert(.x, "character")),
               across(where(lubridate::is.interval),
                      ~ convert(.x, "character")))

    # Write data -----

    if (type != "RData") {
        if (stringr::str_detect(file, "\\..+$", negate = TRUE)) {
            file <- paste0(file, paste0(".", type))
        }

        if (type == "tsv") delim <- "\t"

        out %>%
            readr::write_delim(file,
                               delim = delim,
                               na = na,
                               append = append,
                               col_names = col_names)
    } else if (type == "RData") {
        if (stringr::str_detect(file, "\\..+$")) {
            file <- stringr::str_replace(file, "\\..+$", ".RData")
        }

        data %>% saveRDS(file)
    } else {
        rlang::abort("Critical error.")
    }
}

# HELPERS =====

#' Swap decimal separators
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `swap_decimal` swaps
#' [decimal separators](https://en.wikipedia.org/wiki/Decimal_separator)
#' from `numeric` or `character` objects with numbers.
#'
#' @param x A `numeric` or `character` vector.
#' @param new_decimal (optional) A `"."` or `","` string, indicating the new
#'   decimal separator for `x` (default: `","`).
#' @param quiet (optional) A logical value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A `character` or `numeric` vector with the new decimal separator.
#'
#' `swap_decimal` will convert a character vector to `numeric` if
#' `new_decimal == "."` and if all the data can be converted to `numeric`
#' without any coercion.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' swap_decimal(c(1.1, 2, 3.33))
#' #> [1] "1,1"  "2"    "3,33" # Expected
#' swap_decimal(c("1,1", "2", "3,33"), ".")
#' #> [1] 1.10 2.00 3.33 # Expected
#' swap_decimal(c(6.2, NA, 5.43))
#' #> [1] "6,2"  NA     "5,43" # Expected
#' swap_decimal(c("6,2", NA, "5,43"), ".")
#' #> [1] 6.20   NA 5.43 # Expected
swap_decimal <- function(x, new_decimal = ",", quiet = FALSE) {
    # Check arguments -----

    assert_custom_1(x)
    checkmate::assert_choice(new_decimal, c(".", ","))

    # Get decimal separators -----

    new_decimal <- escape_regex(new_decimal)
    old_decimal <- stringr::str_extract(
        x[which(!is.na(x) & stringr::str_detect(x, "[^\\d]"))[1]], "[^\\d]")
    old_decimal <- escape_regex(old_decimal)

    # Check for errors and warnings -----

    check <-
        all(stringr::str_detect(x, paste0("^[\\d", old_decimal, "]+$|^NA$")))

    if (isFALSE(check)) {
        rlang::abort(paste0(
            "Ambiguity detected. Please check your data. \n",
            "Your data may have more than one decimal separator or ",
            "non-numeric characters.")
        )
    }

    if (is.na(old_decimal)) {
        shush(rlang::warn(paste0(
            "None decimal separator was found. ",
            "Returning 'x' whithout any changes.")
        ), quiet = quiet)

        return(x)
    } else if (old_decimal == new_decimal) {
        shush(rlang::warn(paste0(
            "'x' already have the new decimal separator. ",
            "Returning 'x' whithout any changes.")
        ), quiet = quiet)

        return(x)
    }

    # Transform `x` and return output -----

    out <- stringr::str_replace(as.character(x), old_decimal, new_decimal)

    check <- all(stringr::str_detect(out[which(!is.na(out))],
                                      "^[\\d\\.]+$|^NA$"))

    if (all(is.na(out))) {
        as.numeric(out)
    } else if (new_decimal == escape_regex(".") && isTRUE(check)) {
            as.numeric(out)
    } else {
        out
    }
}
