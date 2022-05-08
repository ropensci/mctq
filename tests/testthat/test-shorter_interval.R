test_that("interval_build() | scalar test", {
    expect_equal(interval_build(
        x = hms::parse_hm("23:00"), y = hms::parse_hm("01:00"),
        method = "shorter"
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 23:00:00"),
        lubridate::ymd_hms("1970-01-02 01:00:00")
    ))

    # `x == y`
    expect_equal(interval_build(
        x = lubridate::as_datetime("1985-01-15 12:00:00"),
        y = lubridate::as_datetime("2020-09-10 12:00:00"),
        method = "shorter"
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 12:00:00"),
        lubridate::ymd_hms("1970-01-01 12:00:00")
    ))

    # `method = "longer"`
    expect_equal(interval_build(
        x = lubridate::as_datetime("1985-01-15 12:00:00"),
        y = lubridate::as_datetime("2020-09-10 12:00:00"),
        method = "longer"
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 12:00:00"),
        lubridate::ymd_hms("1970-01-02 12:00:00")
    ))

    # `NA` cases
    expect_equal(interval_build(
        x = hms::parse_hm("23:00"), y = hms::as_hms(NA), method = "shorter"
    ),
    lubridate::as.interval(NA)
    )
})

test_that("interval_build() | vector test", {
    expect_equal(interval_build(
        x = c(hms::parse_hm("22:45"), hms::parse_hm("12:00")),
        y = c(hms::parse_hm("04:15"), hms::parse_hm("09:00")),
        method = "shorter"
    ),
    c(lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 22:45:00"),
        lubridate::ymd_hms("1970-01-02 04:15:00")
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 09:00:00"),
        lubridate::ymd_hms("1970-01-01 12:00:00"))
    ))
})

test_that("interval_build() | error test", {
    # checkmate::assert_multi_class(x, c("hms", "POSIXt"))
    expect_error(interval_build(
        x = 1, y = hms::hms(1), method = "shorter"
    ),
    "Assertion on 'x' failed"
    )

    # checkmate::assert_numeric(as.numeric(hms::as_hms(x)), lower = 0,
    #                           upper = 86400)
    expect_error(interval_build(
        x = hms::hms(-1), y = hms::hms(1), method = "shorter"
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(x\\)\\)' failed"
    )

    expect_error(interval_build(
        x = - hms::hms(86401), y = hms::hms(1), method = "shorter"
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(x\\)\\)' failed"
    )

    # checkmate::assert_multi_class(y, c("hms", "POSIXt"))
    expect_error(interval_build(
        x = hms::hms(1), y = 1, method = "shorter"
    ),
    "Assertion on 'y' failed"
    )

    # checkmate::assert_numeric(as.numeric(hms::as_hms(x)), lower = 0,
    #                           upper = 86400)
    expect_error(interval_build(
        x = hms::hms(1), y = hms::hms(-1), method = "shorter"
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(y\\)\\)' failed"
    )

    expect_error(interval_build(
        x = hms::hms(1), y = hms::hms(86401), method = "shorter"
    ),
    "Assertion on 'as.numeric\\(hms::as_hms\\(y\\)\\)' failed"
    )

    # assert_identical(x, y, type = "length")
    expect_error(interval_build(
        x = hms::hms(1), y = c(hms::hms(1), hms::hms(1)), method = "shorter"
    ))

    # checkmate::assert_choice(method, method_choices)
    expect_error(interval_build(
        x = hms::hms(1), y = hms::hms(1), method = 1
    ),
    "Assertion on 'method' failed"
    )
})

test_that("interval_build() | warning test", {
    expect_message(interval_build(
        x = hms::parse_hm("00:00"), y = hms::parse_hm("12:00"),
        method = "shorter"
    ),
    "Element '1' of 'x' and 'y' have intervals equal to "
    )
})

test_that("shorter_interval() | general test", {
    expect_equal(shorter_interval(
        x = lubridate::parse_date_time("01:10:00", "HMS"),
        y = lubridate::parse_date_time("11:45:00", "HMS")
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 01:10:00"),
        lubridate::ymd_hms("1970-01-01 11:45:00"))
    )

    # `x == y`
    expect_equal(shorter_interval(
        x = lubridate::as_datetime("1915-02-14 05:00:00"),
        y = lubridate::as_datetime("1970-07-01 05:00:00")
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 05:00:00"),
        lubridate::ymd_hms("1970-01-01 05:00:00")
    ))
})

test_that("shorter_duration() | general test", {
    expect_equal(shorter_duration(
        x = lubridate::parse_date_time("01:10:00", "HMS"),
        y = lubridate::parse_date_time("11:45:00", "HMS")
    ),
    lubridate::as.duration(lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 01:10:00"),
        lubridate::ymd_hms("1970-01-01 11:45:00")
    )))
})

test_that("longer_interval() | general test", {
    expect_equal(longer_interval(
        x = lubridate::parse_date_time("01:10:00", "HMS"),
        y = lubridate::parse_date_time("11:45:00", "HMS")
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 11:45:00"),
        lubridate::ymd_hms("1970-01-02 01:10:00"))
    )

    # `x == y`
    expect_equal(longer_interval(
        x = lubridate::as_datetime("1915-02-14 05:00:00"),
        y = lubridate::as_datetime("1970-07-01 05:00:00")
    ),
    lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 05:00:00"),
        lubridate::ymd_hms("1970-01-02 05:00:00")
    ))
})

test_that("longer_duration() | general test", {
    expect_equal(longer_duration(
        x = lubridate::parse_date_time("01:10:00", "HMS"),
        y = lubridate::parse_date_time("11:45:00", "HMS")
    ),
    lubridate::as.duration(lubridate::as.interval(
        lubridate::ymd_hms("1970-01-01 11:45:00"),
        lubridate::ymd_hms("1970-01-02 01:10:00")
    )))
})
