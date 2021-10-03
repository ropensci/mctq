dialog_line <- function(..., combined_styles = NULL,
                        space_above = TRUE, space_below = TRUE,
                        abort = FALSE) {
    assert_has_length(list(...))
    checkmate::assert_character(combined_styles, null.ok = TRUE)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)
    checkmate::assert_flag(abort)

    if (!is_interactive()) return(NULL)
    if (isTRUE(abort)) return(NULL)

    line <- vapply(list(...), paste0, character(1), collapse = "")
    line <- paste0(line, collapse = "")
    line <- paste0(paste(strwrap(line), collapse = "\n"), " ")

    if(isTRUE(space_above)) cat("\n")
    answer <- read_line(line)
    if(isTRUE(space_below)) cat("\n")

    answer
}
