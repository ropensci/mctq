#' @family utility functions
#' @noRd
message_generator <- function(type = "ok", funny = FALSE) {
    type <- tolower(type)

    if (type == "ok") {
        if (isTRUE(funny)) {
            ok_messages <- c("Noice!", "Everything's A-OK",
                             "Good job (yay!)",
                             "Everything appears to be in order, Sr.!")
        } else {
            ok_messages <- c("Success", "All in order")
        }
        message(sample(ok_messages, 1))
    }
}
