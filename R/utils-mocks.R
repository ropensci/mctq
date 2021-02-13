# To be used with the `mockr` package

is_interactive <- function(...) {
    interactive()
}

is_namespace_loaded <- function(name, ...) {
    isNamespaceLoaded(name)
}

read_line <- function(prompt, ...) {
    readline(prompt)
}
