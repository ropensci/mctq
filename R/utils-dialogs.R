dialog_line <- function(..., combined_styles = NULL,
                        space_above = TRUE, space_below = TRUE,
                        abort = FALSE) {
    styles <- c("reset", "bold", "blurred", "italic", "underline", "inverse",
                "hidden", "strikethrough")
    color <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan",
               "white", "silver")
    bg_colors <- c("bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue",
                   "bgMagenta", "bgCyan", "bgWhite")

    assert_has_length(list(...))
    checkmate::assert_subset(combined_styles, c(styles, color, bg_colors),
                             empty.ok = TRUE)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)
    checkmate::assert_flag(abort)

    if (!is_interactive()) return(999)
    if (isTRUE(abort)) return(999)

    line <- vapply(list(...), paste0, character(1), collapse = "")
    line <- paste0(line, collapse = "")
    line <- paste0(paste(strwrap(line), collapse = "\n"), " ")

    if (is_namespace_loaded("crayon")) {
        crayonize <- shush(crayon::combine_styles(combined_styles))
        line <- (crayonize(line))
    }

    if(isTRUE(space_above)) cat("\n")
    answer <- read_line(line)
    if(isTRUE(space_below)) cat("\n")

    answer
}

alert <- function(..., combined_styles = c("bold", "red"), abort = FALSE) {
    styles <- c("reset", "bold", "blurred", "italic", "underline", "inverse",
                "hidden", "strikethrough")
    color <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan",
               "white", "silver")
    bg_colors <- c("bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue",
                   "bgMagenta", "bgCyan", "bgWhite")

    assert_has_length(list(...))
    checkmate::assert_subset(combined_styles, c(styles, color, bg_colors))
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    message <- vapply(list(...), paste0, character(1), collapse = "")
    message <- paste0(message, collapse = "")

    if (is_namespace_loaded("crayon")) {
        crayonize <- shush(crayon::combine_styles(combined_styles))
        message <- (crayonize(message))
    }

    message(message)

    invisible(NULL)
}
