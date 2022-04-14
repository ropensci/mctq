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

    checkmate::expect_string(check_has_length(c(1, NA), any.missing = FALSE),
                             pattern = "'c\\(1, NA\\)' cannot have missing ")
    checkmate::expect_string(check_has_length(numeric()),
                             pattern = "'numeric\\(\\)' must have length ")
    expect_true(check_has_length(1))

    expect_equal(assert_has_length(1), 1)
    expect_error(assert_has_length(numeric()),
                 "Assertion on 'numeric\\(\\)' failed")
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
    expect_false(test_numeric_(1, lower = 2))
    expect_false(test_numeric_(1, upper = 0))

    checkmate::expect_string(check_numeric_(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_numeric_(NULL, null.ok = FALSE),
                             "'NULL' cannot be 'NULL'")
    checkmate::expect_string(check_numeric_(1, lower = 2),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_numeric_(1, upper = 0),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_numeric_(c("a", "b")),
                             "Must be of type 'numeric', not 'character'")
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
    expect_false(test_duration(lubridate::dhours(1),
                               lower = lubridate::dhours(2)))
    expect_false(test_duration(lubridate::dhours(1),
                               upper = lubridate::dhours(0)))

    checkmate::expect_string(check_duration(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_duration(NULL, null.ok = FALSE),
                             "'NULL' cannot be 'NULL'")
    checkmate::expect_string(check_duration(lubridate::dhours(1),
                                            lower = lubridate::dhours(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_duration(lubridate::dhours(1),
                                            upper = lubridate::dhours(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_duration(c(1, 1)),
                             "Must be of type 'Duration', not 'numeric'")
    expect_true(check_duration(c(lubridate::dhours(1),
                                 lubridate::dhours(1))))
    expect_true(check_duration(NULL, null.ok = TRUE))

    expect_equal(assert_duration(c(lubridate::dhours(1),
                                   lubridate::dhours(1))),
                 c(lubridate::dhours(1), lubridate::dhours(1)))
    expect_error(assert_duration(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_hms() | general test", {
    expect_true(test_hms(hms::hms(1)))
    expect_true(test_hms(c(hms::hms(1), NA), any.missing = TRUE))
    expect_true(test_hms(NULL, null.ok = TRUE))
    expect_false(test_hms("a"))
    expect_false(test_hms(1))
    expect_false(test_hms(lubridate::hours()))
    expect_false(test_hms(lubridate::dhours()))
    expect_false(test_hms(datasets::iris))
    expect_false(test_hms(c(lubridate::dhours(1), NA),
                          any.missing = FALSE))
    expect_false(test_hms(NULL, null.ok = FALSE))
    expect_false(test_hms(hms::hms(1),
                          lower = hms::hms(2)))
    expect_false(test_hms(hms::hms(1),
                          upper = hms::hms(0)))

    checkmate::expect_string(check_hms(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_hms(NULL, null.ok = FALSE),
                             "'NULL' cannot be 'NULL'")
    checkmate::expect_string(check_hms(hms::hms(1),
                                       lower = hms::hms(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_hms(hms::hms(1),
                                       upper = hms::hms(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_hms(c(1, 1)),
                             "Must be of type 'hms', not 'numeric'")
    expect_true(check_hms(c(hms::hms(1), hms::hms(1))))
    expect_true(check_hms(NULL, null.ok = TRUE))

    expect_equal(assert_hms(c(hms::hms(1), hms::hms(1))),
                 c(hms::hms(1), hms::hms(1)))
    expect_error(assert_hms(c(1, 1)),
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
    expect_false(test_posixt(lubridate::as_datetime(1),
                             lower = lubridate::as_datetime(2)))
    expect_false(test_posixt(lubridate::as_datetime(1),
                             upper = lubridate::as_datetime(0)))

    checkmate::expect_string(check_posixt(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_posixt(NULL, null.ok = FALSE),
                             "'NULL' cannot be 'NULL'")
    checkmate::expect_string(check_posixt(lubridate::as_datetime(1),
                                          lower = lubridate::as_datetime(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_posixt(lubridate::as_datetime(1),
                                          upper = lubridate::as_datetime(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_posixt(c(1, 1)),
                             "Must be of type 'POSIXct' or 'POSIXlt', ")
    expect_true(check_posixt(c(lubridate::as_datetime(1),
                               lubridate::as_datetime(1))))
    expect_true(check_posixt(NULL, null.ok = TRUE))

    expect_equal(assert_posixt(c(lubridate::as_datetime(1),
                                 lubridate::as_datetime(1))),
                 c(lubridate::as_datetime(1), lubridate::as_datetime(1)))
    expect_error(assert_posixt(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
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
