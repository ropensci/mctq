cycle_time <- function(time, cycle, reverse = TRUE) {
  UseMethod("cycle_time")
}

#' @export
cycle_time.numeric <- function(time, cycle, reverse = TRUE) {
  time |> cycle_time_build(cycle, reverse)
}

#' @export
cycle_time.Duration <- function(time, cycle, reverse = TRUE) {
  time |>
    cycle_time_build(cycle, reverse) |>
    lubridate::dseconds()
}

#' @export
cycle_time.difftime <- function(time, cycle, reverse = TRUE) {
  out <- time
  units(out) <- "secs"

  out <- out |>
    cycle_time_build(cycle, reverse) |>
    lubridate::seconds() |>
    lubridate::as.difftime(units = "secs")

  units(out) <- units(time)
  out
}

#' @export
cycle_time.hms <- function(time, cycle, reverse = TRUE) {
  time |>
    cycle_time_build(cycle, reverse) |>
    hms::hms()
}

cycle_time_build <- function(time, cycle, reverse) {
  checkmate::assert_multi_class(cycle, c("numeric", "Duration"))
  checkmate::assert_number(as.numeric(cycle), lower = 0, null.ok = FALSE)
  checkmate::assert_flag(reverse)

  cycle <- cycle |> as.numeric()

  sign <-
    time |>
    as.numeric() |>
    sign()

  out <-
    time |>
    as.numeric() |>
    abs() %>% # Don't change the pipe.
    `%%`(cycle)

  if (isTRUE(reverse)) {
    dplyr::if_else(sign < 0, cycle - out, out)
  } else {
    dplyr::if_else(sign < 0, - out, out)
  }
}
