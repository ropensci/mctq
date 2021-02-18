dialog_line <- function(line = paste0("Insert 'q' to exit or press 'enter' ",
                                      "to continue > "),
                        space_above = TRUE, space_below = TRUE, abort = FALSE) {
    checkmate::assert_string(line, min.chars = 3)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)
    checkmate::assert_flag(abort)

    if (!is_interactive()) return(999)
    if (isTRUE(abort)) return(999)

    line <- paste0(paste(strwrap(line), collapse = "\n"), " ")

    if (is_namespace_loaded("crayon")) {
        line <- crayon::red(line)
    }

    if(isTRUE(space_above)) cat("\n")
    answer <- read_line(line)
    if(isTRUE(space_below)) cat("\n")

    answer
}

crayon_message <- function(message, combined_styles, abort = FALSE) {

    styles <- c("reset", "bold", "blurred", "italic", "underline", "inverse",
                "hidden", "strikethrough")
    color <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan",
               "white", "silver")
    bg_colors <- c("bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue",
                   "bgMagenta", "bgCyan", "bgWhite")

    checkmate::assert_string(message)
    checkmate::assert_subset(combined_styles, c(styles, color, bg_colors))
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    if (is_namespace_loaded("crayon")) {
        alert <- shush(crayon::combine_styles(combined_styles))
        message <- (alert(message))
    }

    message(message)

    invisible(NULL)
}
