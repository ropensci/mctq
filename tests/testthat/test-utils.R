test_that("flat_posixct()", {
    object <- flat_posixt(lubridate::dmy_hms("17/04/1995 10:55:00"))
    expect_equal(object, lubridate::ymd_hms("0000-01-01 10:55:00"))
})

test_that("midday_change()", {
    object <- midday_change(lubridate::ymd_hms("2000-05-04 19:43:33"))
    expect_equal(object, lubridate::ymd_hms("0000-01-01 19:43:33"))

    object <- midday_change(lubridate::ymd_hms("2000-05-04 05:34:02"))
    expect_equal(object, lubridate::ymd_hms("0000-01-02 05:34:02"))

    object <- midday_change(
        c(lubridate::ymd_hms("2020-01-01 22:00:00"),
          lubridate::ymd_hms("2020-01-01 06:00:00")))
    expect_equal(object, c(lubridate::ymd_hms("0000-01-01 22:00:00"),
                           lubridate::ymd_hms("0000-01-02 06:00:00")))
})

test_that("is_time()", {
    expect_true(is_time(lubridate::days(1)))
    expect_false(is_time(1))
    expect_false(is_time(letters))
    expect_false(is_time(iris))
    expect_true(is_time(as.Date("2000-01-01")))
    expect_false(is_time(as.Date("2000-01-01"), rm = "Date"))
})
