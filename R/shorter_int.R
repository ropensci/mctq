shorter_int <- function(x, y) {
  int_build(x, y, method = "shorter")
}

longer_int <- function(x, y) {
  int_build(x, y, method = "longer")
}

shorter_duration <- function(x, y) {
  shorter_int(x, y) |> lubridate::as.duration()
}

longer_duration <- function(x, y) {
  longer_int(x, y) |> lubridate::as.duration()
}

int_build <- function(x, y, method = "shorter") {
  method_choices <- c("shorter", "longer")

  checkmate::assert_multi_class(x, c("hms", "POSIXt"))
  checkmate::assert_numeric(as.numeric(hms::as_hms(x)), lower = 0,
                            upper = 86400)
  checkmate::assert_multi_class(y, c("hms", "POSIXt"))
  checkmate::assert_numeric(as.numeric(hms::as_hms(y)), lower = 0,
                            upper = 86400)
  assert_identical(x, y, type = "length")
  checkmate::assert_choice(method, method_choices)

  x <- x |>
    hms::as_hms() |>
    as.POSIXct() |>
    flat_posixt()

  y <- y |>
    hms::as_hms() |>
    as.POSIXct() |>
    flat_posixt()

  list2env(swap(x, y, x > y), envir = environment())

  x1_y1_interval <- lubridate::interval(x, y)
  y1_x2_interval <- lubridate::interval(y, x + lubridate::days())

  if (method == "shorter") {
    out <- dplyr::case_when(
      is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
      x == y ~ lubridate::as.interval(lubridate::hours(0), x),
      x1_y1_interval <= y1_x2_interval ~ x1_y1_interval,
      x1_y1_interval > y1_x2_interval ~ y1_x2_interval,
    )
  } else if (method == "longer") {
    out <- dplyr::case_when(
      is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
      x == y ~ lubridate::as.interval(lubridate::hours(24), x),
      x1_y1_interval >= y1_x2_interval ~ x1_y1_interval,
      x1_y1_interval < y1_x2_interval ~ y1_x2_interval,
    )
  }

  if (any(x1_y1_interval == y1_x2_interval, na.rm = TRUE)) {
    flags <- which(x1_y1_interval == y1_x2_interval) # nolint

    cli::cli_alert_warning(paste0(
      "Element{?s} {single_quote_(as.character(flags))} of 'x' ",
      "and 'y' have intervals equal to 12 hours, i.e., ",
      "there's no shorter or longer interval ",
      "between the two hours (they are equal). Only one ",
      "possible interval was returned."
    ))
  }

  out
}
