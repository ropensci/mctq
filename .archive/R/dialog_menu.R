#' @family utility functions
#' @noRd
dialog_menu <- function(title = "Do you wish to continue?",
                        choices = c("Yes", "No"), space_above = TRUE,
                        abort = FALSE) {
    checkmate::assert_flag(abort)
    checkmate::assert_string(title, min.chars = 3)
    checkmate::assert_character(choices, any.missing = FALSE, unique = TRUE,
                                min.len = 1)
    checkmate::assert_flag(space_above)

    if (isTRUE(abort)) return(invisible(999))

    if (!interactive() || !isNamespaceLoaded("utils")) {
        return(invisible(999))
    }

    if(isTRUE(space_above)) space_above <- "\n" else space_above <- ""

    for (i in seq_along(choices)) {
        choices[i] <- crayon::black$bold(choices[i])
    }

    title <- crayon::green$bold(paste0(space_above, title))

    utils::menu(choices, title = title)
}
