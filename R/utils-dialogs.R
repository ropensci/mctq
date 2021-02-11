#' @family utility functions
#' @noRd
dialog_line <- function(line = paste0("Insert `quit` to exit or press enter ",
                                      "to continue > "),
                        space_above = TRUE, space_below = TRUE, abort = FALSE) {

    checkmate::assert_string(line, min.chars = 3)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)
    checkmate::assert_flag(abort)

    if (!is_interactive()) return(999)
    if (isTRUE(abort)) return(999)

    if (is_namespace_loaded("crayon")) {
        line <- crayon::red(line)
    }

    if(isTRUE(space_above)) cat("\n")
    answer <- read_line(line)
    if(isTRUE(space_below)) cat("\n")

    answer

}
