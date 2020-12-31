#' Load and tidy an actimetry raw dataset
#'
#' @description
#'
#' A wrapper for [load_data(]) and [tidy_data()] functions.
#'
#' If you need to set specific parameters, please use the functions
#' mentioned above.
#'
#' @param file A string with the csv file address for the raw dataset. If left
#'   with no value, a dialog window will open allowing browsing and file
#'   selection.
#' @param device A string with the name of the device used to generate the data.
#'   See [tidy_data()] documentation to know the values available.
#' @param tz a character string that specifies which time zone to parse the
#'   dates with. The string must be a time zone that is recognized by the
#'   user's OS. For more information, see `?lubridate::ymd_hms`.
#' @return A tibble.
#' @examples
#' \dontrun{
#' load_and_tidy(data_example("test_log.txt"))}
#' @importFrom magrittr %>%
#' @export

load_and_tidy <- function(file = file.choose(),
                          device = "acttrust1",
                          tz = "America/Sao_Paulo") {

    # Load and tidy data --------------------

    output <- load_data(file, device = device) %>%
        tidy_data(device = device, tz = tz)

    # Return output --------------------

    output

}

#' Load an actimetry raw dataset from a CSV file
#'
#' @description
#'
#' This is a wrapper function to help simple data loading. It's based on
#' [read_delim()] function found in the `readr` package (tidyverse). You don't
#' need to use this function if you already loaded your file.
#'
#' If this function doesn't work for your file, we recommend using the `readr`
#' package to load it.
#'
#' @param file A string with the csv file address for the raw dataset. If left
#'   with no value, a dialog window will open allowing browsing and file
#'   selection.
#' @param device A string with the name of the device use to generate the file.
#'   i.e. If this parameter is not `NULL`, all the other parameters, except
#'   `file` will be overwritten. At the moment, the only value accepted is
#'   `"acttrust1"`.
#' @param delim A string with the field separator in `file`.
#' @param na A character vector indicating values that must be interpreted
#'   as `NA`.
#' @param skip A 0 or integer value with the number of rows to skip from file.
#' @param skip_empty_rows A logical value indicating if blank rows must be
#'   ignored altogether. i.e. If this option is TRUE then blank rows will
#'   not be represented at all. If it is `FALSE` then they will be represented
#'   by `NA` values in all the columns.
#' @param trim_ws A logical value indicating if leading and trailing
#'   whitespace must be trimmed from each field before parsing it.
#' @return A tibble with all variables as character.
#' @examples
#' \dontrun{
#' load_data(data_example("test_log.txt"))}
#' @importFrom magrittr %>%
#' @export

load_data <- function(file = file.choose(),
                      device = "acttrust1",
                      delim = ",",
                      na = c("", " ", "NA"),
                      skip = 0,
                      skip_empty_rows = TRUE,
                      trim_ws = TRUE) {

    # Check arguments --------------------

    for (i in c("file", "delim", "na")) {
        if (!(is.character(get(i)))) {
            stop(paste(i, "is not class character"), call. = FALSE)
        }
    }

    for (i in c("file", "delim")) {
        if (!(nzchar(get(i)))) {
            stop(paste(i, "must be at least one character"), call. = FALSE)
        }
    }

    if (!(file.exists(file))) {
        stop(paste("file do not exist"), call. = FALSE)
    }

    for (i in c("skip")) {
        if (!(is.numeric(get(i)))) {
            stop(paste(i, "is not 0 or a integer"), call. = FALSE)
        }

        if (!(get(i) %% 1 == 0)) {
            stop(paste(i, "is not 0 or a integer"), call. = FALSE)
        }
    }

    for (i in c("trim_ws", "skip_empty_rows")) {
        if (!(is.logical(get(i)))) {
            stop(paste(i, "is not class logical"), call. = FALSE)
        }
    }

    if (!(is.null(device))) {
        if (!(device %in% c("acttrust1"))) {
            stop("device is not a valid. See documentation.", call. = FALSE)
        }
    }

    # Set values --------------------

    if (is.null(device)) {
        # do nothing
    } else if (device == "acttrust1") {
        if (stringr::str_detect(readLines(file, n = 1),
                                "Condor Instruments Report")) {
            skip <- 25
            n <- 26
        } else {
            skip <- 0
            n <- 1
        }

        if (stringr::str_detect(readLines(file, n = n)[n], ";")) {
            delim <- ";"
        } else {
            delim <- "\t"
        }

        na <- c("", " ", "NA")
        skip_empty_rows <- TRUE
        trim_ws <- TRUE
    }

    # Load data --------------------

    output <- file %>%
        readr::read_delim(delim = delim,
                          na = na,
                          col_types = readr::cols(.default = "c"),
                          skip = skip,
                          trim_ws = trim_ws) %>%
        tibble::as_tibble()

    # Return output --------------------

    output

}

#' Tidy an actimetry raw dataset
#'
#' @description
#'
#' This function tidy a already loaded raw actimetry dataset. If your data
#' ain't loaded yet, please use the [load_data()] function or load it by
#' yourself.
#'
#' To use this package is best that your data conforms to the proposed
#' data structure of this function. You can still use the tools available
#' without tiding your data, but by doing that you may spend more time using
#' the functions, and may be more prone to error.
#'
#' @param data A data frame or tibble.
#' @param device A string with the name of the device used to generate the data.
#'   i.e. If this parameter is not `NULL`, all the other parameters, except
#'   `tz` will be overwritten. At the moment, the only value accepted is
#'   "acttrust1".
#' @param tz A character string that specifies which time zone to parse the
#'   dates with. The string must be a time zone that is recognized by the
#'   user's OS. For more information, see `?lubridate::ymd_hms`.
#' @return A tibble.
#' @examples
#' \dontrun{
#' tidy_data(load_data(data_example("test_log.txt")))}
#' @note
#'
#' To do list:
#'
#' * Add new devices
#' * Add parameters for unconformed data loading
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := .data is_true
#' @importFrom utils head
#' @export

tidy_data <- function(data,
                      device = "acttrust1",
                      tz = "America/Sao_Paulo") {

    # Check arguments --------------------

    if (!(is.data.frame(data))) {
        stop("data is not a data frame", call. = FALSE)
    }

    if (!(is.null(device))) {
        if (!(device %in% c("acttrust1"))) {
            stop("device is not a valid. See documentation.", call. = FALSE)
        }
    }

    # Transform values --------------------

    output <- data %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::mutate_all(stringr::str_trim)

    # Adjust variables and values --------------------

    if (is.null(device)) {
        # do nothing
    } else if (device == "acttrust1") {

        ## Rename and create new variables

        replacement <- list(
            c("ms", "MS"),
            c("x_axis", NA),
            c("y_axis", NA),
            c("z_axis", NA),
            c("event", "EVENT"),
            c("body_temperature", "TEMPERATURE"),
            c("external_temperature", "EXT TEMPERATURE"),
            c("orientation", "ORIENTATION"),
            c("pim", "PIM"),
            c("pim_n", "PIMn"),
            c("tat", "TAT"),
            c("tat_n", "TATn"),
            c("zcm", "ZCM"),
            c("zcm_n", "ZCMn"),
            c("light", "LIGHT"),
            c("ambient_light", "AMB LIGHT"),
            c("red_light", "RED LIGHT"),
            c("green_light", "GREEN LIGHT"),
            c("blue_light", "BLUE LIGHT"),
            c("ir_light", "IR LIGHT"),
            c("uva_light", "UVA LIGHT"),
            c("uvb_light", "UVB LIGHT"),
            c("state", "STATE")
        )

        if (!(names(output)[1] == "DATE")) {
            replacement[[length(replacement) + 1]] <-
                c("timestamp", "DATE/TIME")
        } else {
            replacement[[length(replacement) + 1]] <-
                c("date", "DATE")
            replacement[[length(replacement) + 1]] <-
                c("time", "TIME")
        }

        for (i in seq_along(names(output))) {
            for (j in seq_along(replacement)) {
                if (is.na(replacement[[j]][2])) {
                    output[replacement[[j]][1]] <- as.character(NA)
                } else if (names(output)[i] == replacement[[j]][2]) {
                    names(output)[i] <- replacement[[j]][1]
                }
            }
        }

        ## Adjust decimal marker

        if (is_true(any(head((
            stringr::str_detect(output$body_temperature, ",")))))) {
            output %>% dplyr::mutate_at(
                dplyr::vars(-.data$date, -.data$time),
                function(x) stringr::str_replace(x, ",", "\\."))
        }


        ## Adjust values of the event variable (logical variable)

        for (i in c("event")) {
            output <- output %>%
                dplyr::mutate(!!as.symbol(i) := dplyr::case_when(
                    !!as.symbol(i) == "0" ~ "FALSE",
                    !!as.symbol(i) == "1" ~ "TRUE",
                    TRUE ~ !!as.symbol(i)))
        }

        ## Adjust values of the state variable (factor variable)

        output <- output %>%
            dplyr::mutate(!!as.symbol("state") := dplyr::case_when(
                .data$state == "0" ~ "Awake",
                .data$state == "1" ~ "Sleeping",
                .data$state == "2" ~ "Resting",
                .data$state == "4" ~ "Offwrist",
                .data$state == "6" ~ "Editable 1",
                .data$state == "7" ~ "Editable 2",
                .data$state == "8" ~ "Editable 3",
                TRUE ~ .data$state))

        ## Adjust date and time values

        if (!("timestamp" %in% names(output))) {
            output <- output %>%
                dplyr::mutate(timestamp = lubridate::dmy_hms(paste(
                    .data$date, .data$time), tz = tz))
        } else {
            output <- output %>%
                dplyr::mutate(timestamp = lubridate::dmy_hms(paste(
                    .data$timestamp), tz = tz))
        }
    }

    # Tidy output --------------------

    output <- output %>%
        dplyr::transmute(
            timestamp = .data$timestamp,
            ms = as.numeric(.data$ms),
            x_axis = as.numeric(.data$x_axis),
            y_axis = as.numeric(.data$x_axis),
            z_axis = as.numeric(.data$x_axis),
            pim = as.numeric(.data$pim),
            pim_n = as.numeric(.data$pim_n),
            tat = as.numeric(.data$tat),
            tat_n = as.numeric(.data$tat_n),
            zcm = as.numeric(.data$zcm),
            zcm_n = as.numeric(.data$zcm_n),
            orientation = as.numeric(.data$orientation),
            body_temperature = as.numeric(.data$body_temperature),
            external_temperature =
                as.numeric(.data$external_temperature),
            light = as.numeric(.data$light),
            ambient_light = as.numeric(.data$ambient_light),
            red_light = as.numeric(.data$red_light),
            green_light = as.numeric(.data$green_light),
            blue_light = as.numeric(.data$blue_light),
            ir_light = as.numeric(.data$ir_light),
            uva_light = as.numeric(.data$uva_light),
            uvb_light = as.numeric(.data$uvb_light),
            event = as.logical(.data$event),
            state = factor(.data$state, ordered = FALSE))

    # Return output --------------------

    output

}

#' Tidy GIPSO's web 0.5.0 MCTQ raw dataset
#'
#' @param file A character vector with the csv file names for the
#'  raw dataset.
#' @param sep A string with the field separator in "file".
#' @param dec A string with the decimal point in "file".
#' @param path A string with the path directory where the output
#'   files must be written. NOTE: this variable must be assigned
#'   only if write_data == TRUE.
#' @param return_data A logic value indicating if the function must
#'   return the output data.
#' @param write_data A logic value indicating if the function must
#'   write the output data in "path".
#'
#' @return
#'
#' * If return_data is TRUE, returns a tibble
#' * If write_data is TRUE, write a RData and csv files in the
#'   path directory
#'
#' @details
#'
#' * This function requires the auxiliary_functions.R list of
#'   functions loaded on the parent frame
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := .data is_true
#' @importFrom utils head
#' @export

.source_1 <- function(file,
                     sep = ",",
                     dec = ".",
                     path = ".",
                     return_data = TRUE,
                     write_data = TRUE) {

    # Check arguments --------------------

    for (i in file) {

        if (!(file.exists(i))) {

            stop(paste(i, ", in file, do not exist"))

        }

    }

    if (!(sep %in% c(";", ",", "\t"))) {

        stop("sep is not a valid delimiter")

    }

    if (!(dec %in% c(".", ","))) {

        stop("dec is not a valid decimal marker")

    }

    for (i in c("return_data", "write_data")) {

        if (!(is.logical(get(i)))) {

            stop(paste(i, "value is not logical"))

        }
    }

    if (isTRUE(write_data)) {

        if (!(is.character(path))) {

            stop(paste("path value is not character"))

        }

        if (!(dir.exists(path))) {

            stop("path directory does not exist")

        }
    }

    if (isFALSE(return_data) &&
        isFALSE(write_data)) {

        stop(paste("What you want to do champ?",
                   "return_data and write_data can't both be FALSE!",
                   "Help me out here."))

    }

    # Load data --------------------

    suppressWarnings(

        for (i in file) {

            data_i <- i %>%
                readr::read_delim(delim = sep,
                           na = c("", " ", "NA"),
                           col_types = cols(.default = "c"),
                           locale = locale("en",
                                           date_format = "%Y-%m-%d",
                                           decimal_mark = dec),
                           trim_ws = TRUE) %>%
                tibble::as_tibble

            ## Double encoding correction

            data_i <- data_i %>%
                dplyr::mutate_all(
                    function(x) iconv(x, from="UTF-8", to="latin1")) %>%
                dplyr::mutate_all(
                    function(x) iconv(x, from="UTF-8", to="latin1")) %>%
                dplyr::mutate_all(
                    function(x) iconv(x, from="latin1", to="UTF-8"))

            ## Bind rows

            if (i == file[1]) {

                data <- data_i

            } else {

                data <- dplyr::bind_rows(data, data_i)

            }

        }

    )

    # Select data --------------------

    output <- data %>%
        dplyr::select(ID:mctqWD, mctqBTwHH:mctqBTwMM, mctqSPrepwHH:mctqSPrepwMM,
                      mctqSLatwMM, mctqSEwHH:mctqSEwMM, mctqAlarmw,
                      mctqSIwMM, mctqLEwHH:mctqLEwMM, mctqBTfHH:mctqBTfMM,
                      mctqSPrepfHH:mctqSPrepfMM, mctqSLatfMM,
                      mctqSEfHH:mctqSEfMM, mctqAlarmf, mctqSIfMM,
                      mctqLEfHH:mctqLEfMM)

    # Transform variables --------------------

    output <- output %>%
        dplyr::mutate_all(str_to_upper) %>%
        dplyr::mutate_all(str_trim)

    # Pre-adjust values --------------------

    ## Adjust values of pdGENDER variable (factor variable)

    output <- output %>% dplyr::mutate(pdGENDER = dplyr::case_when(
        pdGENDER == "1" ~ "FEMALE",
        pdGENDER == "2" ~ "MALE"))

    ## Adjust values of pdGENDERidentity variable (factor variable)

    output <- output %>% dplyr::mutate(pdGENDERidentity = case_when(
        pdGENDERidentity == "1" ~ "WOMAN",
        pdGENDERidentity == "2" ~ "MAN",
        pdGENDERidentity == "3" ~ "NON-BINARY"))

    ## Adjust values of pdSEXUALORIENTATION variable (factor variable)

    output <- output %>% dplyr::mutate(pdSEXUALORIENTATION = dplyr::case_when(
        pdSEXUALORIENTATION == "1" ~ "HETEROSEXUAL",
        pdSEXUALORIENTATION == "2" ~ "HOMOSEXUAL",
        pdSEXUALORIENTATION == "3" ~ "BISEXUAL",
        pdSEXUALORIENTATION == "4" ~ "ASEXUAL"))

    ## Adjust values of hhSNORE, hhWORK, hhSTUDY, hhNOWORKORSTUDY,
    ## hhWORKmorning, hhWORKafternoon, hhWORKevening, hhWORKweehours,
    ## hhSTUDYmorning, hhSTUDYafternoon, hhSTUDYevening, hhSTUDYweehours,
    ## mctqAlarmw, mctqAlarmf variables (logical variables)

    for (i in c("hhSNORE", "hhWORK", "hhSTUDY", "hhWORKmorning",
                "hhWORKafternoon", "hhWORKevening", "hhWORKweehours",
                "hhSTUDYmorning", "hhSTUDYafternoon", "hhSTUDYevening",
                "hhSTUDYweehours", "mctqAlarmw", "mctqAlarmf")) {

        output <- output %>% dplyr::mutate(!!as.symbol(i) := dplyr::case_when(
            !!as.symbol(i) == "0" ~ "FALSE",
            !!as.symbol(i) == "1" ~ "TRUE"))

    }

    ## Adjust values of mctqBTwHH, mctqSPrepwHH, mctqSEwHH,
    ## mctqLEwHH, mctqBTfHH, mctqSPrepfHH, mctqSEfHH,
    ## mctqLEfHH variables

    for (i in c("mctqBTwHH", "mctqSPrepwHH", "mctqSEwHH", "mctqLEwHH",
                "mctqBTfHH", "mctqSPrepfHH", "mctqSEfHH", "mctqLEfHH")) {

        output <- output %>% dplyr::mutate(!!as.symbol(i) := dplyr::case_when(
            as.numeric(!!as.symbol(i)) %in% seq(0,23) ~
                as.numeric(!!as.symbol(i))))

    }

    ## Adjust values of mctqBTwMM, mctqSPrepwMM, mctqSLatwMM,
    ## mctqSEwMM, mctqSIwMM, mctqLEwMM, mctqBTfMM, mctqSPrepfMM,
    ## mctqSLatfMM, mctqSEfMM, mctqSIfMM, mctqLEfMM variables

    for (i in c("mctqBTwMM", "mctqSPrepwMM", "mctqSLatwMM",
                "mctqSEwMM", "mctqSIwMM", "mctqLEwMM", "mctqBTfMM",
                "mctqSPrepfMM", "mctqSLatfMM", "mctqSEfMM",
                "mctqSIfMM", "mctqLEfMM")) {

        output <- output %>% dplyr::mutate(!!as.symbol(i) := dplyr::case_when(
            as.numeric(!!as.symbol(i)) %in% seq(0,60, by = 5) ~
                as.numeric(!!as.symbol(i))))

    }

    # Collapse variables --------------------

    ## Collapse hhDRUGS and hhDRUGSwhich variables to a character
    ## variable call sleep_drugs,
    ## hhSLEEPDISORDER and hhSLEEPDISORDERwhich variables to a character
    ## variable call sleep_disorders,
    ## hhMEDICATION and hhMEDICATIONwhich variables to a character
    ## variable call daily_medication

    list <- list(c("sleep_drugs", "hhDRUGS", "hhDRUGSwhich"),
                 c("sleep_disorders", "hhSLEEPDISORDER",
                   "hhSLEEPDISORDERwhich"),
                 c("daily_medication", "hhMEDICATION",
                   "hhMEDICATIONwhich"))

    for (i in list) {

        output <- output %>% dplyr::mutate(!!as.symbol(i[1]) := dplyr::case_when(
            !(is.na(!!as.symbol(i[3]))) ~ !!as.symbol(i[3]),
            !!as.symbol(i[2]) == "1" ~ "YES (NOT SPECIFIED)"))

    }

    ## Collapse mctqBTwHH and mctqBTwMM to a hms variable call bt_w,
    ## mctqSPrepwHH and mctqSPrepwMM to a hms variable call s_prep_w,
    ## mctqSEwHH and mctqSEwMM to a hms variable call se_w
    ## mctqBTfHH and mctqBTfMM to a hms variable call bt_f,
    ## mctqSPrepfHH and mctqSPrepfMM to a hms variable call s_prep_f, and
    ## mctqSEfHH and mctqSEfMM to a hms variable call se_f

    list <- list(c("bt_w", "mctqBTwHH", "mctqBTwMM"),
                 c("s_prep_w", "mctqSPrepwHH", "mctqSPrepwMM"),
                 c("se_w", "mctqSEwHH", "mctqSEwMM"),
                 c("bt_f", "mctqBTfHH", "mctqBTfMM"),
                 c("s_prep_f", "mctqSPrepfHH", "mctqSPrepfMM"),
                 c("se_f", "mctqSEfHH", "mctqSEfMM"))

    for (i in list) {

        output <- output %>%
            dplyr::mutate(!!as.symbol(i[1]) :=
                              convert_to_hms(
                                  hours(!!as.symbol(i[2])) +
                                      minutes(!!as.symbol(i[3])),
                                  class = "vector"))

    }

    ## Collapse hhSTUDYmorning, hhSTUDYafternoon, and hhSTUDYevening
    ## to a variable call study_period

    output$study_period <- as.character(NA)

    for (i in seq_len(nrow(output))) {

        study_period_i <- NULL

        if (isTRUE(as.logical(output$hhSTUDYmorning[i]))) {

            study_period_i <- append(study_period_i, "MORNING")

        }

        if (isTRUE(as.logical(output$hhSTUDYafternoon[i]))) {

            study_period_i <- append(study_period_i, "AFTERNOON")

        }

        if (isTRUE(as.logical(output$hhSTUDYevening[i]))) {

            study_period_i <- append(study_period_i, "NIGHT")

        }

        if (isTRUE(as.logical(output$hhSTUDYweehours[i]))) {

            study_period_i <- append(study_period_i, "AFTER MIDNIGHT")

        }

        if (is.null(study_period_i)) {

            next

        } else {

            output$study_period[i] <- paste(study_period_i,
                                            collapse = ",")

        }

    }

    # Collapse hhWORKmorning, hhWORKafternoon, hhWORKevening,
    ## and hhWORKweehours to a variable call work_period

    output$work_period <- as.character(NA)

    for (i in seq_len(nrow(output))) {

        work_period_i <- NULL

        if (isTRUE(as.logical(output$hhWORKmorning[i]))) {

            work_period_i <- append(work_period_i, "MORNING")

        }

        if (isTRUE(as.logical(output$hhWORKafternoon[i]))) {

            work_period_i <- append(work_period_i, "AFTERNOON")

        }

        if (isTRUE(as.logical(output$hhWORKevening[i]))) {

            work_period_i <- append(work_period_i, "NIGHT")

        }

        if (isTRUE(as.logical(output$hhWORKweehours[i]))) {

            work_period_i <- append(work_period_i, "AFTER MIDNIGHT")

        }

        if (is.null(work_period_i)) {

            next

        } else {

            output$work_period[i] <- paste(work_period_i,
                                           collapse = ",")

        }

    }

    ## Transform study_period and work_period variables to list

    output <- output %>%
        dplyr::mutate_at(c("study_period", "work_period"),
                         function(x) str_split(x, ","))

    # Tidy output --------------------

    suppressWarnings(

        output <- dplyr::tibble(timestamp = lubridate::ymd_hms(paste(output$date,
                                                          output$time),
                                                    tz = "America/Sao_Paulo"),
                                name = as.character(output$pdNAME),
                                email = as.character(output$pdEMAIL),
                                sex = factor(output$pdGENDER,
                                             levels = c("FEMALE",
                                                        "MALE"),
                                             ordered = FALSE),
                                birth_date = lubridate::ymd(output$pdBIRTH),
                                country = as.character(output$pdCOUNTRY),
                                state = as.character(output$pdSTATE),
                                city = as.character(output$pdCITY),
                                postal_code = as.character(output$pdPOSTAL),
                                study = as.logical(output$hhSTUDY),
                                study_period = as.list(output$study_period),
                                work = as.logical(output$hhWORK),
                                work_period = as.list(output$work_period),
                                weight = as.numeric(output$pdWEIGHT),
                                height = as.numeric(output$pdHEIGHT),
                                original_id = as.integer(output$ID),
                                track = as.character(output$track),
                                gender_identity =
                                    factor(output$pdGENDERidentity,
                                           levels = c("WOMAN",
                                                      "MAN",
                                                      "NON-BINARY"),
                                           ordered = FALSE),
                                sexual_orientation =
                                    factor(output$pdSEXUALORIENTATION,
                                           levels = c("HETEROSEXUAL",
                                                      "HOMOSEXUAL",
                                                      "BISEXUAL",
                                                      "ASEXUAL"),
                                           ordered = FALSE),
                                sleep_drugs = as.character(output$sleep_drugs),
                                sleep_disorders =
                                    as.character(output$sleep_disorders),
                                daily_medication =
                                    as.character(output$daily_medication),
                                snore = as.logical(output$hhSNORE),
                                wd = as.integer(output$mctqWD),
                                bt_w = hms::as_hms(output$bt_w),
                                s_prep_w = hms::as_hms(output$s_prep_w),
                                s_lat_w = lubridate::as.duration(lubridate::minutes(output$mctqSLatwMM)),
                                se_w = hms::as_hms(output$se_w),
                                si_w = lubridate::as.duration(lubridate::minutes(output$mctqSIwMM)),
                                alarm_w = as.logical(output$mctqAlarmw),
                                le_w = lubridate::as.duration(lubridate::hours(output$mctqLEwHH) +
                                                                  lubridate::minutes(output$mctqLEwMM)),
                                bt_f = hms::as_hms(output$bt_f),
                                s_prep_f = hms::as_hms(output$s_prep_f),
                                s_lat_f = lubridate::as.duration(lubridate::minutes(output$mctqSLatfMM)),
                                se_f = hms::as_hms(output$se_f),
                                si_f = lubridate::as.duration(lubridate::minutes(output$mctqSIfMM)),
                                alarm_f = as.logical(output$mctqAlarmf),
                                le_f = lubridate::as.duration(lubridate::hours(output$mctqLEfHH) +
                                                                  lubridate::minutes(output$mctqLEfMM)))

    )

    # Write data --------------------

    if (isTRUE(write_data)) {

        ## Prepare csv output

        output_csv <- output %>%
            unlist_() %>%
            convert_to_hms() %>%
            dplyr::mutate_if(is.POSIXct, as.character)

        ## Write data

        if (!(str_extract(path, ".$") == "/")) {

            path <- paste0(path, "/")

        }

        output_csv %>%
            readr::write_delim(paste0(path,
                                      "source_1.csv"),
                               delim = ",",
                               col_names = TRUE)

        output %>% saveRDS(paste0(path,
                                  "source_1.RData"))

    }

    # Return output --------------------

    if (isTRUE(return_data)) {

        return(output)

    }

}

#' Tidy Baependi DLMO's MCTQ raw dataset
#'
#' @param file A string with the csv file name for the
#'  raw dataset.
#' @param sep A string with the field separator in "file".
#' @param dec A string with the decimal point in "file".
#' @param path A string with the path directory where the output
#'   files must be written. NOTE: this variable must be assigned
#'   only if write_data == TRUE.
#' @param return_data A logic value indicating if the function must
#'   return the output data.
#' @param write_data A logic value indicating if the function must
#'   write the output data in "path".
#'
#' @return
#'
#' * If return_data is TRUE, returns a tibble
#' * If write_data is TRUE, write a RData and csv files in the
#'   path directory
#'
#' @details
#'
#' * This function requires the auxiliary_functions.R list of
#'   functions loaded on the parent frame
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := .data is_true
#' @importFrom utils head
#' @export

.source_2 <- function(file,
                      sep = ",",
                      dec = ".",
                      path = ".",
                      return_data = TRUE,
                      write_data = TRUE) {

    # Check arguments --------------------

    for (i in file) {

        if (!(file.exists(i))) {

            stop(paste(i, ", in file, do not exist"))

        }

    }

    if (!(sep %in% c(";", ",", "\t"))) {

        stop("sep is not a valid delimiter")

    }

    if (!(dec %in% c(".", ","))) {

        stop("dec is not a valid decimal marker")

    }

    for (i in c("return_data", "write_data")) {

        if (!(is.logical(get(i)))) {

            stop(paste(i, "value is not logical"))

        }
    }

    if (isTRUE(write_data)) {

        if (!(is.character(path))) {

            stop(paste("path value is not character"))

        }

        if (!(dir.exists(path))) {

            stop("path directory does not exist")

        }
    }

    if (isFALSE(return_data) &&
        isFALSE(write_data)) {

        stop(paste("What you want to do champ?",
                   "return_data and write_data can't both be FALSE!",
                   "Help me out here."))

    }

    # Load data --------------------

    data <- file %>%
        readr::read_delim(delim = sep,
                          na = c("", " ", "NA"),
                          col_types = cols(.default = "c"),
                          locale = locale("en",
                                          date_format = "%Y-%m-%d",
                                          decimal_mark = dec),
                          col_names = FALSE,
                          trim_ws = TRUE) %>%
        tibble::as_tibble

    data <- data[-1, ]

    # Select data --------------------

    output <- data %>% select(X1, X3:X26)

    # Transform variables --------------------

    output <- output %>%
        dplyr::mutate_all(str_to_upper) %>%
        dplyr::mutate_all(str_trim)

    # Pre-adjust values --------------------

    ## Adjust values of "Timestamp" (X1) variable (POSIXct variable)

    output <- output %>%
        date_fix("X1") %>%
        dplyr::mutate(date = str_extract(as.character(X1),
                                  "^[0-9]{2}/[0-9]{2}/[0-9]{4}")) %>%
        mutate(time = str_trim(
            str_replace(as.character(X1), date, ""))) %>%
        hour_fix("time") %>%
        mutate(X1 = paste(date, time))

    ## Adjust values of "A5 - Sexo" (X6) variable
    ## (factor variable)

    output <- output %>% mutate(X6 = case_when(
        X6 == "FEMININO" ~ "FEMALE",
        X6 == "MASCULINO" ~ "MALE"))

    ## Adjust values of "A6 - Zona de habitação" (X7) variable
    ## (factor variable)

    output <- output %>% mutate(X7 = case_when(
        X7 == "URBANA" ~ "URBAN",
        X7 == "RURAL" ~ "RURAL"))

    ## Adjust values of "A7 - Escolaridade" (X8) variable
    ## (factor variable)

    output <- output %>% mutate(X8 = case_when(
        X8 == "ILETRADO" ~ "ILLITERATE",
        X8 == "FUNDAMENTAL I" ~ "ELEMENTARY SCHOOL",
        X8 == "FUNDAMENTAL II" ~ "MIDDLE SCHOOL",
        X8 == "ENSINO MÉDIO" ~ "HIGH SCHOOL",
        X8 == "ENSINO TÉCNICO" ~ "VOCATIONAL EDUCATION",
        X8 == "ENSINO SUPERIOR" ~ "HIGHER EDUCATION",
        X8 == "TECNÓLOGO" ~ "ASSOCIATE",
        X8 == "GRADUAÇÃO" ~ "UNDERGRADUATE",
        X8 == "PÓS GRADUAÇÃO" ~ "POSTGRADUATE",
        X8 == "MESTRADO" ~ "MASTER",
        X8 == "DOUTORADO" ~ "PHD",
        X8 == "PÓS-DOUTORADO" ~ "POSTDOCTORATE"))

    ## Adjust values of "A9 - Escola" (X10) variable
    ## (factor variable)

    output <- output %>% mutate(X10 = case_when(
        X10 == "PÚBLICA" ~ "PUBLIC",
        X10 == "PRIVADA" ~ "PRIVATE"))

    ## Adjust values of "D1.1 - Horário de trabalho:" (X13),
    ## "Despertador? DIAS DE TRABALHO" (X19),
    ## "Despertador DIAS LIVRES" (X25)
    ## variables (logical variables)

    for (i in c("X13", "X19", "X25")) {

        output <- output %>% mutate(!!as.symbol(i) := case_when(
            str_detect(!!as.symbol(i), "SIM") |
                str_detect(!!as.symbol(i), "COM") ~ "TRUE",
            str_detect(!!as.symbol(i), "NÃO") |
                str_detect(!!as.symbol(i), "SEM") ~ "FALSE"))

    }

    ## Adjust values of
    ## "D2 DIAS de TRABALHO – Vou para a cama às:" (X15),
    ## "D3 DIAS DE TRABALHO – Decido dormir às:" (X16),
    ## "D5 DIAS DE TRABALHO – Acordo às:" (X18),
    ## "D7 DIAS LIVRES– Vou para a cama às:" (X21),
    ## "D8 DIAS LIVRES– Decido dormir às:" (X22),
    ## "D10 DIAS LIVRES – Acordo às:" (X24),
    ## variables (hms variables)

    hour_fix <- parent.frame()$auxiliary_functions$hour_fix

    for (i in c("X15", "X16", "X18", "X21", "X22", "X24")) {

        output <- output %>% hour_fix(i)

    }

    ## Adjust values of
    ## "D4DIAS DE TRABALHO  – Eu necessito _____ minutos para adormecer."
    ## (X17),
    ## "D6 DIAS DE TRABALHO – Passados _____ minutos, me levanto." (X20),
    ## "D9 DIAS LIVRES– Eu necessito _____ minutos para dormir." (X23),
    ## "D11DIAS LIVRES – Passados _____ minutos, me levanto." (X26),
    ## variables (duration variables)

    output$X17[1] <- "30"
    output$X17[2] <- "18"
    output$X20[1] <- "2"
    output$X20[2] <- "10"
    output$X23[1] <- "30"
    output$X23[2] <- "16"
    output$X26[1] <- "5"
    output$X26[2] <- "8"

    # Tidy output --------------------

    suppressWarnings(

        output <- tibble(timestamp = mdy_hms(output$X1,
                                             tz = "America/Sao_Paulo"),
                         sex = factor(output$X6,
                                      levels = c("FEMALE",
                                                 "MALE"),
                                      ordered = FALSE),
                         schooling_level =
                             factor(output$X8,
                                    levels = c("ILLITERATE",
                                               "ELEMENTARY SCHOOL",
                                               "MIDDLE SCHOOL",
                                               "HIGH SCHOOL",
                                               "VOCATIONAL EDUCATION",
                                               "HIGHER EDUCATION",
                                               "ASSOCIATE",
                                               "UNDERGRADUATE",
                                               "POSTGRADUATE",
                                               "MASTER",
                                               "PHD",
                                               "POSTDOCTORATE"),
                                    ordered = TRUE),
                         habitation_zone = factor(output$X7,
                                                  levels = c("URBAN",
                                                             "RURAL"),
                                                  ordered = FALSE),
                         original_id = as.integer(output$X3),
                         family_id = as.integer(output$X4),
                         age = as.integer(output$X5),
                         years_of_schooling = as.integer(output$X9),
                         father_years_of_schooling =
                             as.integer(output$X11),
                         mother_years_of_schooling =
                             as.integer(output$X12),
                         school_type = factor(output$X10,
                                              levels = c("PUBLIC",
                                                         "PRIVATE"),
                                              ordered = FALSE),
                         regular_work_schedule = as.logical(output$X13),
                         wd = as.integer(output$X14),
                         bt_w = as_hms(output$X15),
                         s_prep_w = as_hms(output$X16),
                         s_lat_w = as.duration(minutes(output$X17)),
                         se_w = as_hms(output$X18),
                         si_w = as.duration(minutes(output$X20)),
                         alarm_w = as.logical(output$X19),
                         bt_f = as_hms(output$X21),
                         s_prep_f = as_hms(output$X22),
                         s_lat_f = as.duration(minutes(output$X23)),
                         se_f = as_hms(output$X24),
                         si_f = as.duration(minutes(output$X26)),
                         alarm_f = as.logical(output$X25))

    )

    # Write data --------------------

    if (isTRUE(write_data)) {

        ## Prepare csv output

        unlist_ <- parent.frame()$auxiliary_functions$unlist_
        convert_to_hms <- parent.frame()$auxiliary_functions$convert_to_hms

        output_csv <- output %>%
            unlist_() %>%
            convert_to_hms() %>%
            mutate_if(is.POSIXct, as.character)

        ## Write data

        if (!(str_extract(path, ".$") == "/")) {

            path <- paste0(path, "/")

        }

        output_csv %>%
            write_delim(paste0(path,
                               "source_2.csv"),
                        delim = ",",
                        col_names = TRUE)

        output %>% saveRDS(paste0(path,
                                  "source_2.RData"))

    }

    # Return output --------------------

    if (isTRUE(return_data)) {

        return(output)

    }

}

#' Tidy Sleep Genetics's MCTQ raw dataset
#'
#' @param file A string with the csv file name for the
#'  raw dataset.
#' @param sep A string with the field separator in "file".
#' @param dec A string with the decimal point in "file".
#' @param path A string with the path directory where the output
#'   files must be written. NOTE: this variable must be assigned
#'   only if write_data == TRUE.
#' @param return_data A logic value indicating if the function must
#'   return the output data.
#' @param write_data A logic value indicating if the function must
#'   write the output data in "path".
#'
#' @return
#'
#' * If return_data is TRUE, returns a tibble
#' * If write_data is TRUE, write a RData and csv files in the
#'   path directory
#'
#' @details
#'
#' * This function requires the auxiliary_functions.R list of
#'   functions loaded on the parent frame
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := .data is_true
#' @importFrom utils head
#' @export

.source_3 <- function(file,
                    sep = ",",
                    dec = ".",
                    path = ".",
                    return_data = TRUE,
                    write_data = TRUE) {

    # Check arguments --------------------

    for (i in file) {

        if (!(file.exists(i))) {

            stop(paste(i, ", in file, do not exist"))

        }

    }

    if (!(sep %in% c(";", ",", "\t"))) {

        stop("sep is not a valid delimiter")

    }

    if (!(dec %in% c(".", ","))) {

        stop("dec is not a valid decimal marker")

    }

    for (i in c("return_data", "write_data")) {

        if (!(is.logical(get(i)))) {

            stop(paste(i, "value is not logical"))

        }
    }

    if (isTRUE(write_data)) {

        if (!(is.character(path))) {

            stop(paste("path value is not character"))

        }

        if (!(dir.exists(path))) {

            stop("path directory does not exist")

        }
    }

    for (i in c("auxiliary_functions")) {

        if (!(i %in% ls(envir = parent.frame()))) {

            stop(paste(i, "is not loaded"))

        }
    }

    if (isFALSE(return_data) &&
        isFALSE(write_data)) {

        stop(paste("What you want to do champ?",
                   "return_data and write_data can't both be FALSE!",
                   "Help me out here."))

    }

    # Load data --------------------

    data <- file %>%
        read_delim(delim = sep,
                   na = c("", " ", "NA"),
                   col_types = cols(.default = "c"),
                   locale = locale("en",
                                   date_format = "%Y-%m-%d",
                                   decimal_mark = dec),
                   col_names = FALSE,
                   trim_ws = TRUE) %>%
        as_tibble

    data <- data[-1, ]

    # Select data --------------------

    output <- data %>% select(X1:X3, X5:X11, X16:X55, X58:X61)

    # Transform variables --------------------

    output <- output %>%
        mutate_all(str_to_upper) %>%
        mutate_all(str_trim)

    # Pre-adjust values --------------------

    ## Adjust values of "Você estuda?" (X5),
    ## "Você estuda no período matutino?" (X6),
    ## "Você estuda no período vespertino?" (X8),
    ## "Você estuda no período noturno?" (X10),
    ## "Você trabalha?" (X26),
    ## "Você tem um horário regular de trabalho
    ## (também como dona(o) de casa, etc.)?" (X29),
    ## "Você usa um despertador nos dias de trabalho?" (X36),
    ## "Se responder “SIM” na questão anterior, você acorda
    ## regularmente ANTES do alarme tocar?" (X37),
    ## "Você usa um despertador nos dias de trabalho?" (X44),
    ## "Se responder “SIM” na questão anterior,
    ## você acorda regularmente ANTES do alarme tocar?" (X45), and
    ## "Os horários que mencionou acima são dependentes do
    ## despertador mesmo fora dos dias de trabalho?" (X52)
    ## (logical variables)

    for (i in c("X5", "X6", "X8", "X10", "X26", "X29",
                "X36", "X37", "X44", "X45", "X52")) {

        output <- output %>% mutate(!!as.symbol(i) := case_when(
            !!as.symbol(i) == "SIM" ~ "TRUE",
            !!as.symbol(i) == "NÃO" ~ "FALSE"))

    }

    ## Adjust values of "Sexo biológico" (X18) variable
    ## (factor variable)

    output <- output %>% mutate(X18 = case_when(
        X18 == "FEMININO" ~ "FEMALE",
        X18 == "MASCULINO" ~ "MALE"))

    ## Adjust values of "Qual é o grau do curso/programa que
    ## você frequenta?" (X24) variable (factor variable)

    output <- output %>% mutate(X24 = case_when(
        X24 == "ILETRADO" ~ "ILLITERATE",
        X24 == "FUNDAMENTAL I" ~ "ELEMENTARY SCHOOL",
        X24 == "FUNDAMENTAL II" ~ "MIDDLE SCHOOL",
        X24 == "ENSINO MÉDIO" ~ "HIGH SCHOOL",
        X24 == "ENSINO TÉCNICO" ~ "VOCATIONAL EDUCATION",
        X24 == "ENSINO SUPERIOR" ~ "HIGHER EDUCATION",
        X24 == "TECNÓLOGO" ~ "ASSOCIATE",
        X24 == "GRADUAÇÃO" ~ "UNDERGRADUATE",
        X24 == "PÓS GRADUAÇÃO" ~ "POSTGRADUATE",
        X24 == "MESTRADO" ~ "MASTER",
        X24 == "DOUTORADO" ~ "PHD",
        X24 == "PÓS-DOUTORADO" ~ "POSTDOCTORATE",
        TRUE ~ X24))

    ## Adjust values of
    ## "Em quais dias da semana você trabalha?" (X27) and
    ## "Em quais dias da semana você estuda?" (X59) variables
    ## (list variables)

    for (i in c("X27", "X59")) {

        output <- output %>% mutate(
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "SEGUNDA-FEIRA", "MONDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "TERÇA-FEIRA", "TUESDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "QUARTA-FEIRA", "WEDNESDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "QUINTA-FEIRA", "THURSDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "SEXTA-FEIRA", "FRIDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "SÁBADO", "SATURDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i),
                                              "DOMINGO", "SUNDAY"),
            !!as.symbol(i) := str_replace_all(!!as.symbol(i), " ", ""))

    }

    ## Adjust values of "Em quais períodos do dia você
    ## trabalha?" (X28) variable (list variable)

    output <- output %>%
        mutate(X28 = str_replace_all(X28, "MANHÃ", "MORNING"),
               X28 = str_replace_all(X28, "TARDE", "AFTERNOON"),
               X28 = str_replace_all(X28, "NOITE", "NIGHT"),
               X28 = str_replace_all(X28, "MADRUGADA", "AFTER MIDNIGHT"),
               X28 = str_replace_all(X28, ", ", ","))

    ## Adjust values of "Você estuda no período matutino?" (X6),
    ## "Você estuda no período vespertino?" (X8), and
    ## "Você estuda no período noturno?" (X10) variables
    ## (list variables)

    output <- output %>% mutate(X6 = case_when(
        X7 == "0" ~ "FALSE",
        !(is.na(X7)) ~ "TRUE",
        TRUE ~ X6))

    output <- output %>% mutate(X8 = case_when(
        X9 == "0" ~ "FALSE",
        !(is.na(X9)) ~ "TRUE",
        TRUE ~ X8))

    output <- output %>% mutate(X10 = case_when(
        X11 == "0" ~ "FALSE",
        !(is.na(X11)) ~ "TRUE",
        TRUE ~ X10))

    ## Adjust values of "Durante a semana, quantos dias você estuda
    ## no período matutino?" (X7), "Durante a semana, quantos dias
    ## você estuda no período vespertino?" (X9), and "Durante a
    ## semana, quantos dias você estuda no período noturno?" (X11)
    ## variables (list variables)

    output <- output %>% mutate(X7 = case_when(
        X7 == "0" ~ as.character(NA),
        TRUE ~ X7))

    output <- output %>% mutate(X9 = case_when(
        X9 == "0" ~ as.character(NA),
        TRUE ~ X9))

    output <- output %>% mutate(X11 = case_when(
        X11 == "0" ~ as.character(NA),
        TRUE ~ X11))

    ## Adjust values of
    ## "(Figura 1) Vou para a cama às _________ horas" (X31),
    ## "(Figura 3) Às _________ horas, estou pronto para dormir" (X32),
    ## "(Figura 4) Necessito de _________ minutos para adormecer" (X33),
    ## "(Figura 5) Acordo às_________ horas" (X34),
    ## "(Figura 6) Passados _________ minutos, levanto-me" (X35),
    ## "Em média, quanto tempo por dia você passa exposto à luz
    ## do dia (ao ar livre) nos dias de trabalho?" (X38),
    ## "(Figura 1) Vou para a cama às _________ horas" (X39),
    ## "(Figura 3) Às _________ horas, estou pronto para dormir" (X40),
    ## "(Figura 4) Necessito de _________ minutos para adormecer" (X41),
    ## "(Figura 5) Acordo às_________ horas" (X42),
    ## "(Figura 6) Passados _________ minutos, levanto-me" (X43),
    ## "Em média, quanto tempo por dia você passa exposto à
    ## luz do dia (ao ar livre) nos dias de trabalho?" (X46),
    ## "(Figura 1) Vou para a cama às _________ horas" (X47),
    ## "(Figura 3) Às _________ horas, estou pronto para dormir" (X48),
    ## "(Figura 4) Necessito de _________ minutos para adormecer" (X49),
    ## "(Figura 5) Acordo às_________ horas" (X50),
    ## "(Figura 6) Passados _________ minutos, levanto-me" (X51), and
    ## "Em média, quanto tempo por dia você passa exposto à
    ## luz do dia (ao ar livre) fora dos dias de trabalho?" (X54)
    ## variables (hms and duration variables)

    hour_fix <- parent.frame()$auxiliary_functions$hour_fix

    for (i in c("X31", "X32", "X33", "X34", "X35", "X38",
                "X39", "X40", "X41", "X42", "X43", "X46",
                "X47", "X48", "X49", "X50", "X51", "X54")) {

        output <- output %>% hour_fix(i)

    }

    # Collapse variables --------------------

    ## Collapse "Você estuda no período matutino?" (X6),
    ## "Você estuda no período vespertino?" (X8),
    ## "Você estuda no período noturno?" (X10)
    ## to a variable call study_period

    output$study_period = as.character(NA)

    for (i in seq_len(nrow(output))) {

        study_period_i <- NULL

        if (isTRUE(as.logical(output$X6[i]))) {

            study_period_i <- append(study_period_i, "MORNING")

        }

        if (isTRUE(as.logical(output$X8[i]))) {

            study_period_i <- append(study_period_i, "AFTERNOON")

        }

        if (isTRUE(as.logical(output$X10[i]))) {

            study_period_i <- append(study_period_i, "NIGHT")

        }

        if (is.null(study_period_i)) {

            next

        } else {

            output$study_period[i] <- paste(study_period_i,
                                            collapse = ",")

        }

    }

    ## Collapse "Durante a semana, quantos dias você estuda no período
    ## matutino??" (X7), "Durante a semana, quantos dias você estuda no
    ## período vespertino?" (X9), and "Durante a semana, quantos dias
    ## você estuda no período noturno?" (X11) to a variable call
    ## study_frequency

    output$study_frequency = as.character(NA)

    for (i in seq_len(nrow(output))) {

        study_frequency_i <- NULL

        if (!(is.na(output$X7[i]))) {

            study_frequency_i <- append(study_frequency_i, output$X7[i])

        }

        if (!(is.na(output$X9[i]))) {

            study_frequency_i <- append(study_frequency_i, output$X9[i])

        }

        if (!(is.na(output$X11[i]))) {

            study_frequency_i <- append(study_frequency_i, output$X11[i])

        }

        if (is.null(study_frequency_i)) {

            next

        } else {

            output$study_frequency[i] <- paste(study_frequency_i,
                                               collapse = ",")

        }

    }

    ## Transform all collapsed variables to list

    output <- output %>%
        mutate_at(c("X27", "X28", "X59", "study_period",
                    "study_frequency"),
                  function(x) str_split(x, ","))

    output <- output %>% mutate(
        study_frequency = lapply(study_frequency, as.integer))

    # Tidy output --------------------

    suppressWarnings(

        output <-
            tibble(timestamp = dmy_hms(output$X1,
                                       tz = "America/Sao_Paulo"),
                   name = as.character(output$X16),
                   cpf = as.character(output$X19),
                   email = as.character(output$X2),
                   phone = as.character(output$X20),
                   sex = factor(output$X18,
                                levels = c("FEMALE",
                                           "MALE"),
                                ordered = FALSE),
                   birth_date = dmy(output$X17),
                   postal_code = as.character(output$X58),
                   study = as.logical(output$X5),
                   study_days = as.list(output$X59),
                   study_period = as.list(output$study_period),
                   study_frequency = as.list(output$study_frequency),
                   work = as.logical(output$X26),
                   work_days = as.list(output$X27),
                   work_period = as.list(output$X28),
                   weight = as.numeric(output$X60),
                   height = as.numeric(output$X61),
                   operator = as.character(output$X3),
                   usp_number = as.numeric(output$X22),
                   university = as.character(output$X21),
                   university_unit = as.character(output$X23),
                   course = as.character(output$X25),
                   course_level = factor(output$X24,
                                         levels = c("ILLITERATE",
                                                    "ELEMENTARY SCHOOL",
                                                    "MIDDLE SCHOOL",
                                                    "HIGH SCHOOL",
                                                    "VOCATIONAL EDUCATION",
                                                    "HIGHER EDUCATION",
                                                    "ASSOCIATE",
                                                    "UNDERGRADUATE",
                                                    "POSTGRADUATE",
                                                    "MASTER",
                                                    "PHD",
                                                    "POSTDOCTORATE"),
                                         ordered = TRUE),
                   dna_kit_number = as.character(output$X55),
                   regular_work_schedule = as.logical(output$X29),
                   wd = as.integer(output$X30),
                   bt_w = if_else(output$X30 == 7,
                                  as.hms(output$X31),
                                  as.hms(output$X39)),
                   s_prep_w = if_else(output$X30 == 7,
                                      as.hms(output$X32),
                                      as.hms(output$X40)),
                   s_lat_w = if_else(output$X30 == 7,
                                     as.duration(
                                         lubridate::hms(output$X33)),
                                     as.duration(
                                         lubridate::hms(output$X41))),
                   se_w = if_else(output$X30 == 7,
                                  as.hms(output$X34),
                                  as.hms(output$X42)),
                   si_w = if_else(output$X30 == 7,
                                  as.duration(
                                      lubridate::hms(output$X35)),
                                  as.duration(
                                      lubridate::hms(output$X43))),
                   alarm_w = if_else(output$X30 == 7,
                                     as.logical(output$X36),
                                     as.logical(output$X44)),
                   wake_before_alarm_w = if_else(output$X30 == 7,
                                                 as.logical(output$X37),
                                                 as.logical(output$X45)),
                   le_w = if_else(output$X30 == 7,
                                  as.duration(
                                      lubridate::hms(output$X38)),
                                  as.duration(
                                      lubridate::hms(output$X46))),
                   bt_f = as.hms(output$X47),
                   s_prep_f = as.hms(output$X48),
                   s_lat_f = as.duration(lubridate::hms(output$X49)),
                   se_f = as.hms(output$X50),
                   si_f = as.duration(lubridate::hms(output$X51)),
                   alarm_f = as.logical(output$X52),
                   reasons_f = as.character(output$X53),
                   le_f = as.duration(lubridate::hms(output$X54)))

    )

    # Write data --------------------

    if (isTRUE(write_data)) {

        ## Prepare csv output

        unlist_ <- parent.frame()$auxiliary_functions$unlist_
        convert_to_hms <- parent.frame()$auxiliary_functions$convert_to_hms

        output_csv <- output %>%
            unlist_() %>%
            convert_to_hms() %>%
            mutate_if(is.POSIXct, as.character)

        ## Write data

        if (!(str_extract(path, ".$") == "/")) {

            path <- paste0(path, "/")

        }

        output_csv %>%
            write_delim(paste0(path,
                               "source_3.csv"),
                        delim = ",",
                        col_names = TRUE)

        output %>% saveRDS(paste0(path,
                                  "source_3.RData"))

    }

    # Return output --------------------

    if (isTRUE(return_data)) {

        return(output)

    }

}
