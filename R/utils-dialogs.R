# Sort by type or alphabetical order.

dialog_line <- function(..., space_above = TRUE, space_below = TRUE,
                        abort = FALSE) {
    assert_has_length(list(...))
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)
    checkmate::assert_flag(abort)

    if (!is_interactive() || isTRUE(abort)) return(NULL)

    line <- list(...) %>%
        vapply(paste0, character(1), collapse = "") %>%
        paste0(collapse = "") %>%
        strwrap() %>%
        paste(collapse = "\n") %>%
        paste0(" ")

    if (isTRUE(space_above)) cli::cat_line()
    out <- read_line(line)
    if (isTRUE(space_below)) cli::cat_line()

    out
}
