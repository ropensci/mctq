test_that("na_as() | general test", {
    expect_equal(na_as(TRUE),as.logical(NA))
    expect_equal(na_as(""),as.character(NA))
    expect_equal(na_as(1L),as.integer(NA))
    expect_equal(na_as(1),as.numeric(NA))
    expect_equal(na_as(lubridate::dhours()), lubridate::as.duration(NA))
    expect_equal(na_as(lubridate::hours()), lubridate::as.period(NA))
    expect_equal(na_as(as.difftime(1, units = "secs")),
                 as.difftime(as.numeric(NA), units = "secs"))
    expect_equal(na_as(hms::hms(1)), hms::as_hms(NA))
    expect_equal(na_as(as.Date("1970-01-01")), as.Date(NA))
    expect_equal(na_as(lubridate::as_datetime(0)), lubridate::as_datetime(NA))
    expect_equal(na_as(as.POSIXlt(lubridate::as_datetime(0))),
                 as.POSIXlt(lubridate::as_datetime(NA)))

    # Nonexistent method error
    expect_error(na_as(list(NA)))
})
