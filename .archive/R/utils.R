#' @family utility functions
#' @noRd
check_that_ <- function(data, ...) {

    checkmate::assert_data_frame(data)

    confront <- validate::check_that(data, ...)
    validate::summary(confront)

}
