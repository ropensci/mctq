.onAttach <- function(...) {
    tip <- "Learn how to use 'mctq' at gipsousp.github.io/mctq ."
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
