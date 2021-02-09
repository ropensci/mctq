#' @family utility functions
#' @noRd
dialog_line <- function(line = paste0("Insert `quit` to exit or press enter ",
                                      "to continue: "),
                        space_above = TRUE, space_below = TRUE, abort = FALSE) {

    checkmate::assert_flag(abort)
    checkmate::assert_string(line, min.chars = 3)

    if (!interactive()) return(invisible(999))
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
