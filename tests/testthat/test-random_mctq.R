# Don't forget to run devtools::load_all(".") and uncomment the variables
# before trying to run the tests interactively.

test_that("random_mctq() | general test", {
    checkmate::expect_list(shush(random_mctq(model = "standard")))
    checkmate::expect_list(shush(random_mctq(model = "micro")))
    expect_equal(shush(random_mctq(model = "shift")), NULL)
    # checkmate::expect_list(shush(random_mctq(model = "shift")))

    checkmate::expect_subset(c("bt_w", "le_w"),
                             names(shush(random_mctq(model = "standard"))))
    checkmate::expect_subset(c("so_w", "se_w"),
                             names(shush(random_mctq(model = "micro"))))
    # checkmate::expect_subset(c("napd_w", "sjl_weighted"),
    #                          names(shush(random_mctq(model = "shift"))))
})

test_that("random_mctq() | error test", {
    # is_namespace_loaded <- mctq:::is_namespace_loaded
    # random_mctq <- mctq::random_mctq

    # "This function requires the `stats` package to run [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_namespace_loaded = function(...) FALSE,
            random_mctq())
    }

    # mock()
    expect_error(mock())

    # Invalid values for `model` and `quiet`
    expect_error(random_mctq(model = 1))
    expect_error(random_mctq(quiet = 1))
})

test_that("random_mctq() | message test", {
    # "\nModel: Standard MCTQ\n"
    expect_message(random_mctq(model = "standard"))

    # "\nModel: Micro MCTQ\n"
    expect_message(random_mctq(model = "micro"))

    # "\nModel: MCTQ Shift\n"
    # expect_message(random_mctq(model = "shift"))
})

test_that("random_std_mctq() | general test", {
    # # Finder
    # for (i in seq_len(100)) {
    #     set.seed(i)
    #     x <- random_std_mctq()
    #
    #     # if (x$work == FALSE) break # 7
    #
    #     # check <- shortest_interval(x$bt_w, x$bt_f, "Interval")
    #     # check <- lubridate::int_end(check)
    #     # if (hms::as_hms(check) == x$bt_f) break # 2
    #
    #     # check_w <- shortest_interval(x$bt_w, x$sprep_w)
    #     # check_f <- shortest_interval(x$bt_f, x$sprep_f)
    #     # if (check_f >= check_w) break # 1
    #
    #     # if (x$slat_f >= x$slat_w) break # 1
    #     # if (x$si_f >= x$si_w) break # 1
    #
    #     # check_w <- shortest_interval(x$sprep_w, x$se_w)
    #     # check_f <- shortest_interval(x$sprep_f, x$se_f)
    #     # if (check_f >= check_w) break # 1
    #
    #     # if (x$le_f >= x$le_w) break # 1
    #     # if (isFALSE(x$alarm_w)) break # 7
    #     # if (isFALSE(x$reasons_f)) break # 3
    # }

    # "if (work == FALSE)" and "if (isFALSE(alarm_w))"
    set.seed(7)
    x <- random_std_mctq()
    expect_false(x$work)

    # "if (hms::as_hms(check) == bt_f)" OK
    set.seed(2)
    x <- random_std_mctq()
    check <- shortest_interval(x$bt_w, x$bt_f, "Interval")
    check <- lubridate::int_end(check)
    expect_true(hms::as_hms(check) == x$bt_f)

    # "if (check_f >= check_w) [bt-sprep]", "if (slat_f >= slat_w)"
    # "if (si_f >= si_w)", "if (check_f >= check_w) [sprep-se]", and
    # "if (le_f >= le_w)"
    set.seed(1)
    x <- random_std_mctq()
    expect_true(x$si_f >= x$si_w)

    # "if (isFALSE(reasons_f))"
    set.seed(3)
    x <- random_std_mctq()
    expect_true(x$reasons_f == FALSE)
})

test_that("random_micro_mctq() | general test", {
    # # Finder
    # for (i in seq_len(100)) {
    #     set.seed(i)
    #     x <- random_micro_mctq()
    #
    #     # check <- shortest_interval(x$so_w, x$so_f, "Interval")
    #     # check <- lubridate::int_end(check)
    #     # if (hms::as_hms(check) == x$so_f) break
    #
    #     # check_w <- shortest_interval(x$so_w, x$se_w)
    #     # check_f <- shortest_interval(x$so_f, x$se_f)
    #     # if (check_f >= check_w) break
    # }

    # "if (hms::as_hms(check) == x$so_f)" and (check_f >= check_w)"
    set.seed(1)
    x <- random_micro_mctq()
    check <- shortest_interval(x$so_w, x$so_f, "Interval")
    check <- lubridate::int_end(check)
    expect_true(hms::as_hms(check) == x$so_f)
})

test_that("sample_time() | general test", {
    checkmate::expect_numeric(as.numeric(sample_time()),
                              lower = as.numeric(hms::parse_hms("00:00:00")),
                              max = as.numeric(hms::parse_hms("23:59:59")))

    set.seed(1)
    expect_equal(sample_time(), hms::parse_hms("13:50:00"))
})

test_that("sample_time() | error test", {
    # You cannot take a sample larger than the population [...]
    expect_error(sample_time(
        min = lubridate::dseconds(1),
        max = lubridate::dseconds(3),
        by = lubridate::dseconds(1),
        size = 100,
        replace = FALSE))
})
