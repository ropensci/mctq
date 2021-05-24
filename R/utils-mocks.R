# To be used with the `mockr` package.

is_interactive <- function(...) {
    interactive()
}

require_namespace <- function(x, ..., quietly = TRUE) {
    requireNamespace(x, ..., quietly = quietly)
}

read_line <- function(prompt, ...) {
    readline(prompt)
}
