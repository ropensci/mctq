.onAttach <- function(...) {
    tip <- "Learn how to use 'mctq' at gipsousp.github.io/mctq ."
    package_startup_message(paste(strwrap(tip), collapse = "\n"))
}
