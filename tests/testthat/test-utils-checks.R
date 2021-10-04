test_that("*_length_one() | general test", {
    expect_true(test_length_one(NA))
    expect_false(test_length_one(NULL))

    checkmate::expect_string(check_length_one(c(1, 1)),
                             pattern = "'c\\(1, 1\\)' must have length 1, ")
    expect_true(check_length_one(1))

    expect_equal(assert_length_one(1), 1)
    expect_error(assert_length_one(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_has_length() | general test", {
    expect_true(test_has_length(c(1, 2)))
    expect_false(test_has_length(NULL))

    checkmate::expect_string(check_has_length(numeric()),
                             pattern = "'numeric\\(\\)' must have length ")
    checkmate::expect_string(check_has_length(c(1, NA), any.missing = FALSE),
                             pattern = "'c\\(1, NA\\)' cannot have missing ")
    expect_true(check_has_length(1))

    expect_equal(assert_has_length(1), 1)
    expect_error(assert_has_length(numeric()),
                 "Assertion on 'numeric\\(\\)' failed")
})

test_that("*_whole_number() | general test", {
    expect_true(test_whole_number(0))
    expect_true(test_whole_number(as.integer(1)))
    expect_true(test_whole_number(as.double(11)))
    expect_true(test_whole_number(as.numeric(475)))
    expect_true(test_whole_number(c(1, NA), any.missing = TRUE))
    expect_true(test_whole_number(NULL, null.ok = TRUE))
    expect_false(test_whole_number(-1L))
    expect_false(test_whole_number(-55))
    expect_false(test_whole_number(1.58))
    expect_false(test_whole_number(lubridate::dhours()))
    expect_false(test_whole_number(letters))
    expect_false(test_whole_number(datasets::iris))
    expect_false(test_whole_number(c(1, NA), any.missing = FALSE))
    expect_false(test_whole_number(NULL, null.ok = FALSE))

    checkmate::expect_string(check_whole_number(c(1, 1.5)),
                             pattern = "'c\\(1, 1.5\\)' must consist of whole ")
    checkmate::expect_string(check_whole_number(c(1, NA), any.missing = FALSE),
                             pattern = "'c\\(1, NA\\)' cannot have missing ")
    checkmate::expect_string(check_whole_number(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    expect_true(check_whole_number(c(1, 1)))
    expect_true(check_whole_number(c(1, NA), any.missing = TRUE))
    expect_true(check_whole_number(NULL, null.ok = TRUE))

    expect_equal(assert_whole_number(c(1, 1)), c(1, 1))
    expect_error(assert_whole_number(c(1, 1.5)),
                 "Assertion on 'c\\(1, 1.5\\)' failed")
})

test_that("*_numeric_() | general test", {
    expect_true(test_numeric_(as.integer(1)))
    expect_true(test_numeric_(as.double(1)))
    expect_true(test_numeric_(as.numeric(1)))
    expect_true(test_numeric_(c(1, NA), any.missing = TRUE))
    expect_true(test_numeric_(NULL, null.ok = TRUE))
    expect_false(test_numeric_(lubridate::dhours()))
    expect_false(test_numeric_(letters))
    expect_false(test_numeric_(datasets::iris))
    expect_false(test_numeric_(c(1, NA), any.missing = FALSE))
    expect_false(test_numeric_(NULL, null.ok = FALSE))

    checkmate::expect_string(check_numeric_(c("a", "b")),
                             pattern = "Must be of type 'numeric', ")
    checkmate::expect_string(check_numeric_(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_numeric_(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    expect_true(check_numeric_(c(1, 1)))
    expect_true(check_numeric_(NULL, null.ok = TRUE))

    expect_equal(assert_numeric_(c(1, 1)), c(1, 1))
    expect_error(assert_numeric_(c("a", "b")),
                 "Assertion on 'c\\(\"a\", \"b\"\\)' failed")
})

test_that("*_duration() | general test", {
    expect_true(test_duration(lubridate::dhours(1)))
    expect_true(test_duration(c(lubridate::dhours(1), NA), any.missing = TRUE))
    expect_true(test_duration(NULL, null.ok = TRUE))
    expect_false(test_duration("a"))
    expect_false(test_duration(1))
    expect_false(test_duration(lubridate::hours()))
    expect_false(test_duration(hms::hms(1)))
    expect_false(test_duration(datasets::iris))
    expect_false(test_duration(c(lubridate::dhours(1), NA),
                               any.missing = FALSE))
    expect_false(test_duration(NULL, null.ok = FALSE))

    checkmate::expect_string(check_duration(c(1, 1)),
                             "Must be of type 'Duration', not 'numeric'")
    checkmate::expect_string(check_duration(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_duration(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    expect_true(check_duration(c(lubridate::dhours(1),
                                 lubridate::dhours(1))))
    expect_true(check_duration(NULL, null.ok = TRUE))

    expect_equal(assert_duration(c(lubridate::dhours(1),
                                   lubridate::dhours(1))),
                 c(lubridate::dhours(1), lubridate::dhours(1)))
    expect_error(assert_duration(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_posixt() | general test", {
    expect_true(test_posixt(lubridate::as_datetime(1)))
    expect_true(test_posixt(as.POSIXlt(lubridate::as_datetime(1))))
    expect_true(test_posixt(c(lubridate::as_datetime(1), NA),
                            any.missing = TRUE))
    expect_true(test_posixt(NULL, null.ok = TRUE))
    expect_false(test_posixt("a"))
    expect_false(test_posixt(1))
    expect_false(test_posixt(lubridate::hours()))
    expect_false(test_posixt(hms::hms(1)))
    expect_false(test_posixt(datasets::iris))
    expect_false(test_posixt(c(lubridate::as_datetime(1), NA),
                               any.missing = FALSE))
    expect_false(test_posixt(NULL, null.ok = FALSE))

    checkmate::expect_string(check_posixt(c(1, 1)),
                             "Must be of type 'POSIXct' or 'POSIXlt', ")
    checkmate::expect_string(check_posixt(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_posixt(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    expect_true(check_posixt(c(lubridate::as_datetime(1),
                               lubridate::as_datetime(1))))
    expect_true(check_posixt(NULL, null.ok = TRUE))

    expect_equal(assert_posixt(c(lubridate::as_datetime(1),
                                 lubridate::as_datetime(1))),
                 c(lubridate::as_datetime(1), lubridate::as_datetime(1)))
    expect_error(assert_posixt(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_temporal() | general test", {
    expect_true(test_temporal(lubridate::dhours()))
    expect_true(test_temporal(lubridate::hours()))
    expect_true(test_temporal(as.difftime(1, units = "secs")))
    expect_true(test_temporal(hms::hms(1)))
    expect_true(test_temporal(as.Date("2000-01-01")))
    expect_true(test_temporal(lubridate::as_datetime(1)))
    expect_true(test_temporal(as.POSIXlt(lubridate::as_datetime(1))))
    expect_true(test_temporal(lubridate::as.interval(
        lubridate::dhours(), lubridate::as_datetime(0))))
    expect_true(test_temporal(NULL, null.ok = TRUE))
    expect_false(test_temporal(1))
    expect_false(test_temporal(letters))
    expect_false(test_temporal(datasets::iris))
    expect_false(test_temporal(lubridate::dhours(), rm = "Duration"))
    expect_false(test_temporal(lubridate::hours(), rm = "Period"))
    expect_false(test_temporal(as.difftime(1, units = "secs"), rm = "difftime"))
    expect_false(test_temporal(hms::hms(1), rm = "hms"))
    expect_false(test_temporal(as.Date("2000-01-01"), rm = "Date"))
    expect_false(test_temporal(lubridate::as_datetime(1), rm = "POSIXct"))
    expect_false(test_temporal(as.POSIXlt(lubridate::as_datetime(1)),
                rm = "POSIXlt"))
    expect_false(test_temporal(lubridate::as.interval(
        lubridate::dhours(), lubridate::as_datetime(0)), rm = "Interval"))
    expect_false(test_temporal(c(1, NA), any.missing = FALSE))
    expect_false(test_temporal(NULL, null.ok = FALSE))

    checkmate::expect_string(check_temporal(c(1, 1)),
                             pattern = "Must be a temporal object ")
    checkmate::expect_string(check_temporal(c(1, NA), any.missing = FALSE),
                             pattern = "'c\\(1, NA\\)' cannot have missing ")
    checkmate::expect_string(check_temporal(NULL, null.ok = FALSE),
                             pattern = "'NULL' cannot have 'NULL' values")
    expect_true(check_temporal(c(lubridate::hours(1), lubridate::hours(1))))
    expect_true(check_temporal(NULL, null.ok = TRUE))

    expect_equal(assert_temporal(c(lubridate::hours(1), lubridate::hours(1))),
                 c(lubridate::hours(1), lubridate::hours(1)))
    expect_error(assert_temporal(c(1, 1)), "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("assert_identical() | general test", {
    expect_error(assert_identical(1))
    expect_error(assert_identical(1, c(1, 1), type = "value"))
    expect_true(assert_identical(1, 1, type = "value"))

    expect_error(assert_identical(1, c(2, 2), type = "length"))
    expect_true(assert_identical(1, 2, type = "length"))

    expect_error(assert_identical(1, "a", type = "class"))
    expect_true(assert_identical(1, 3, type = "class"))

    expect_true(assert_identical(NULL, NULL, null.ok = TRUE))
    expect_error(assert_identical(1, NA, any.missing = FALSE))
    expect_error(assert_identical(NULL, NULL, null.ok = FALSE))
})

test_that("*_custom_1() | general test", {
    expect_true(test_custom_1(c("a", "b")))
    expect_true(test_custom_1(c(1, 2)))
    expect_true(test_custom_1(c(1, NA), any.missing = TRUE))
    expect_true(test_custom_1(NULL, null.ok = TRUE))
    expect_false(test_custom_1(datasets::iris))
    expect_false(test_custom_1(lubridate::dhours()))
    expect_false(test_custom_1(c(1, NA), any.missing = FALSE))
    expect_false(test_custom_1(NULL, null.ok = FALSE))

    checkmate::expect_string(check_custom_1(lubridate::dminutes(1)),
                             pattern = "'lubridate::dminutes\\(1\\)' must ")
    checkmate::expect_string(check_custom_1(c(1, NA), any.missing = FALSE),
                             pattern = "'c\\(1, NA\\)' cannot have missing ")
    checkmate::expect_string(check_custom_1(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    expect_true(check_custom_1(c(1, 1)))
    expect_true(check_custom_1(c("a", "a")))
    expect_true(check_custom_1(NULL, null.ok = TRUE))

    expect_equal(assert_custom_1(c(1, 1)), c(1, 1))
    expect_equal(assert_custom_1(c("a", "a")), c("a", "a"))
    expect_error(assert_custom_1(lubridate::dminutes(1)),
                 "Assertion on 'lubridate::dminutes\\(1\\)' failed")
})
