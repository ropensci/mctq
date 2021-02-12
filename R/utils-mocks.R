#' @family utility functions
#' @noRd
is_interactive <- function(...) {
    # To be used with the `mockr` package
    interactive()
}

#' @family utility functions
#' @noRd
is_namespace_loaded <- function(name, ...) {
    # To be used with the `mockr` package
    isNamespaceLoaded(name)
}

#' @family utility functions
#' @noRd
read_line <- function(prompt, ...) {
    # To be used with the `mockr` package
    readline(prompt)
}
