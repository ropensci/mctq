.onAttach <- function(...) {
    tip <- "Learn how to use 'mctq' at gipsousp.github.io/mctq ."

    if (is_interactive()) {
        packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }
}
