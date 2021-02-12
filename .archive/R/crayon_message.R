#' @family utility functions
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
