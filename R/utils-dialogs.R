# dialog_menu_yn <- function(title = "Do you wish to continue?",
#                            n_yes = 1, n_no = 2, shuffle = TRUE,
#                            space_above = TRUE, space_below = TRUE,
#                            abort = FALSE) {
#     checkmate::assert_string(title, min.chars = 3)
#     checkmate::assert_number(n_yes)
#     checkmate::assert_number(n_no)
#     checkmate::assert_flag(shuffle)
#     checkmate::assert_flag(space_above)
#     checkmate::assert_flag(space_below)
#     checkmate::assert_flag(abort)
#
#     if (!is_interactive()) return(NULL)
#     if (isTRUE(abort)) return(NULL)
#     require_pkg("utils")
#
#     yes <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree",
#             "Absolutely")
#     no <- c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not")
#
#     if(isTRUE(space_above)) cat("\n")
#
#     for (i in seq_along(choices)) {
#         choices[i] <- crayon::black$bold(choices[i])
#     }
#
#     title <- crayon::green$bold(paste0(space_above, title))
#
#     out <- utils::menu(choices, title = title)
#
#     if(isTRUE(space_below)) cat("\n")
#
#     out != 0L && qs[[out]] %in% yes
# }

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

    if (require_namespace("crayon", quietly = TRUE) &&
        !is.null(combined_styles)) {
        line <- crayonize(line, combined_styles = combined_styles)
    }

    if(isTRUE(space_above)) cat("\n")
    answer <- read_line(line)
    if(isTRUE(space_below)) cat("\n")

    answer
}

alert <- function(..., combined_styles = c("bold", "red"), type = "message",
                  abort = FALSE) {
    assert_has_length(list(...))
    checkmate::assert_character(combined_styles, null.ok = TRUE)
    checkmate::assert_choice(type, c("cat", "message", "warning"))
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    message <- vapply(list(...), paste0, character(1), collapse = "")
    message <- paste0(message, collapse = "")
    message <- paste(strwrap(message), collapse = "\n")

    if (require_namespace("crayon", quietly = TRUE) &&
        !is.null(combined_styles)) {
        message <- crayonize(message, combined_styles = combined_styles)
    }

    if (type == "cat") {
        cat(message)
    } else if (type == "message") {
        message(message)
    } else if (type == "warning") {
        warning(message, call. = FALSE)
    }

    invisible(NULL)
}

crayonize <- function(..., combined_styles = c("bold", "red"), abort = FALSE) {
    styles <- c("reset", "bold", "blurred", "italic", "underline", "inverse",
                "hidden", "strikethrough")
    color <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan",
               "white", "silver")
    bg_colors <- c("bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue",
                   "bgMagenta", "bgCyan", "bgWhite")

    assert_has_length(list(...))
    checkmate::assert_subset(combined_styles, c(styles, color, bg_colors),
                             empty.ok = TRUE)
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    out <- unlist(list(...))

    if (require_namespace("crayon", quietly = TRUE) &&
        !is.null(combined_styles)) {
        crayonize <- shush(crayon::combine_styles(combined_styles))
        out <- vapply(out, crayonize, character(1), USE.NAMES = FALSE)
    }

    out
}
