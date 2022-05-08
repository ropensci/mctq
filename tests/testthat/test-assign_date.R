test_that("assign_date() | scalar test", {
    expect_equal(assign_date(
        hms::parse_hm("02:10"), hms::parse_hm("05:30")
    ),
    lubridate::interval(
        lubridate::as_datetime("1970-01-01 02:10:00"),
        lubridate::as_datetime("1970-01-01 05:30:00")
    )
    )
})

test_that("assign_date() | vector test", {
    expect_equal(assign_date(
        c(hms::parse_hm("05:40"), hms::parse_hm("21:30")),
        c(hms::parse_hm("18:05"), hms::parse_hm("10:30"))
    ),
    c(lubridate::interval(
        lubridate::as_datetime("1970-01-01 05:40:00"),
        lubridate::as_datetime("1970-01-01 18:05:00")
    ),
    lubridate::interval(
        lubridate::as_datetime("1970-01-01 21:30:00"),
        lubridate::as_datetime("1970-01-02 10:30:00"))
    )
    )
})

test_that("assign_date() | `ambiguity` test", {
    expect_equal(assign_date(
        lubridate::as_datetime("1985-01-15 12:00:00"),
        lubridate::as_datetime("2020-09-10 12:00:00"),
        ambiguity = 0
    ),
    lubridate::interval(
        lubridate::as_datetime("1970-01-01 12:00:00"),
        lubridate::as_datetime("1970-01-01 12:00:00")
    )
    )

    expect_equal(assign_date(
        lubridate::as_datetime("1985-01-15 12:00:00"),
        lubridate::as_datetime("2020-09-10 12:00:00"),
        ambiguity = 24
    ),
    lubridate::interval(
        lubridate::as_datetime("1970-01-01 12:00:00"),
        lubridate::as_datetime("1970-01-02 12:00:00")
    )
    )

    expect_equal(assign_date(
        lubridate::as_datetime("1985-01-15 12:00:00"),
        lubridate::as_datetime("2020-09-10 12:00:00"),
        ambiguity = NA
    ),
    lubridate::as.interval(NA)
    )
})

test_that("assign_date() | error test", {
    # checkmate::assert_multi_class(start, c("hms", "POSIXt"))
    expect_error(assign_date(
        start = 1, end = hms::hms(1), ambiguity = 0
    ),
    "Assertion on 'start' failed"
    )

    # checkmate::assert_numeric(as.numeric(hms::as_hms(start)),
    #                           lower = 0, upper = 86400)
    expect_error(assign_date(
        start = hms::hms(-1), end = hms::hms(1), ambiguity = 0
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(start\\)\\)' failed"
    )

    expect_error(assign_date(
        start = hms::hms(86401), end = hms::hms(1), ambiguity = 0
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(start\\)\\)' failed"
    )

    # checkmate::assert_multi_class(end, c("hms", "POSIXt"))
    expect_error(assign_date(
        start = hms::hms(1), end = 1, ambiguity = 0
    ), "Assertion on 'end' failed"
    )

    # checkmate::assert_numeric(as.numeric(hms::as_hms(end)),
    #                           lower = 0, upper = 86400)
    expect_error(assign_date(
        start = hms::hms(1), end = hms::hms(-1), ambiguity = 0
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(end\\)\\)' failed"
    )

    expect_error(assign_date(
        start = hms::hms(1), end = hms::hms(86401), ambiguity = 0
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(end\\)\\)' failed"
    )

    # assert_identical(start, end, type = "length")
    expect_error(assign_date(
        start = hms::hms(1), end = c(hms::hms(1), hms::hms(1)), ambiguity = 0
        )
    )

    # checkmate::assert_choice(ambiguity, c(0, 24 , NA))
    expect_error(assign_date(
        start = hms::hms(1), end = hms::hms(1) , ambiguity = "x"
    ),
    "Assertion on 'ambiguity' failed"
    )
})
