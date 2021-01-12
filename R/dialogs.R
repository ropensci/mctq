#' @family dialog functions
#' @noRd
dialog_menu <- function(abort = FALSE, title = "Do you wish to continue?",
                        space_above = TRUE, space_below = TRUE) {

    checkmate::assert_flag(abort)
    checkmate::assert_string(title, min.chars = 3)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)

    if (!rlang::is_interactive()) return(invisible(999))
    if (isTRUE(abort)) return(invisible(999))

    if(isTRUE(space_above)) space_above <- "\n" else space_above <- ""
    if(isTRUE(space_below)) space_below <- "\n\n" else space_below <- ""

    choices <- c("Yes", "No")
    for (i in seq_along(choices)) {
        choices[i] <- crayon::black$bold(choices[i])
    }

    title <- crayon::green$bold(paste0(space_above, title, space_below))

    utils::menu(choices, title = title)

}

#' @family dialog functions
#' @noRd
dialog_line <- function(abort = FALSE,
                        line = paste0("Insert `quit` to exit or press any key ",
                                      "to continue: "),
                        space_above = TRUE, space_below = TRUE) {

    checkmate::assert_flag(abort)
    checkmate::assert_string(line, min.chars = 3)

    if (!rlang::is_interactive()) return(invisible(999))
    if (isTRUE(abort)) return(invisible(999))

    if(isTRUE(space_above)) cat("\n")
    answer <- readline(crayon::red(line))
    if(isTRUE(space_below))cat("\n")

    if (tolower(answer) == "quit") {
        return(1)
    } else {
        return(0)
    }

}

#' @family dialog functions
#' @noRd
crayon_message <- function(text = NULL, title = NULL, tag = NULL,
                           space_above = TRUE, space_below = TRUE) {

    checkmate::assert_string(text, null.ok = TRUE)
    checkmate::assert_string(title, null.ok = TRUE)
    checkmate::assert_string(tag, null.ok = TRUE)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)

    # Text formatting

    if (!is.null(text)) {
        text <- crayon::reset(text)
    } else {
        text <- NA
    }

    # Title formatting

    if (!is.null(title)) {
        title <- crayon::green$bold(title)
    } else {
        title <- NA
    }

    # Tag formatting

    if (!is.null(tag)) {
        tag <- crayon::black$bold$inverse("  ", toupper(tag), "  ")
    } else {
        tag <- NA
    }

    # Output

    cat(paste0(
        ifelse(isTRUE(space_above), "\n", ""),
        ifelse(is.na(tag), "", tag),
        ifelse(is.na(title), "", paste0("  ", title)),
        ifelse(is.na(text), "", "\n\n"),
        ifelse(is.na(text), "", text),
        ifelse(isTRUE(space_below), "\n\n", "")
    ))

}

#' @family dialog functions
#' @noRd
message_generator <- function(type = "ok", funny = FALSE) {

    type <- stringr::str_to_lower(type)

    if (type == "ok") {
        if (isTRUE(funny)) {
            ok_messages <- c("Noice!", "Everything's A-OK",
                             "Good job (yay!)",
                             "Everything appears to be in order, Sr.!")
        } else {
            ok_messages <- c("Success", "All in order")
        }
        rlang::inform(sample(ok_messages, 1))
    }

}
