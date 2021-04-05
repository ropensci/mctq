# Don't forget to run devtools::load_all(".") and uncomment the variables
# before trying to run the tests interactively.

test_that("random_mctq() | general test", {
    set.seed(1)
    checkmate::expect_list(shush(random_mctq(model = "standard")))
    checkmate::expect_list(shush(random_mctq(model = "micro")))
    checkmate::expect_list(shush(random_mctq(model = "shift")))

    checkmate::expect_subset(c("bt_w", "le_w"),
                             names(shush(random_mctq(model = "standard"))))
    checkmate::expect_subset(c("so_w", "se_w"),
                             names(shush(random_mctq(model = "micro"))))
    checkmate::expect_subset(c("napo_w_m", "napo_f_e"),
                             names(shush(random_mctq(model = "shift"))))
})

test_that("random_mctq() | error test", {
    # require_namespace <- mctq:::require_namespace
    # random_mctq <- mctq::random_mctq

    # "This function requires the `stats` package to run [...]"
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) FALSE,
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

    "\nModel: MCTQ Shift\n"
    expect_message(random_mctq(model = "shift"))
})

test_that("random_std_mctq() | general test", {
    # # Finder
    # for (i in seq_len(100)) {
    #     set.seed(i)
    #     x <- random_std_mctq()
    #
    #     # if (x$work == FALSE) break # 7
    #     # if (isFALSE(x$alarm_w)) break # 1
    #     # if (isFALSE(x$reasons_f)) break # 2
    # }

    # "if (work == FALSE)"
    set.seed(7)
    x <- random_std_mctq()
    expect_false(x$work)

    # "if (isFALSE(reasons_f))"
    set.seed(2)
    x <- random_std_mctq()
    check <- shortest_interval(x$bt_w, x$bt_f, "Interval")
    check <- lubridate::int_end(check)
    expect_true(hms::as_hms(check) == x$bt_f)

    # "if (isFALSE(alarm_w))"
    set.seed(1)
    x <- random_std_mctq()
    expect_true(x$si_f >= x$si_w)
})

test_that("random_micro_mctq() | general test", {
    set.seed(1)
    x <- random_micro_mctq()
    expect_true(x$se_f > x$se_w)
})

test_that("random_shift_mctq() | general test", {
    # # Finder
    # for (i in seq_len(100)) {
    #     set.seed(i)
    #     x <- random_shift_mctq()
    #
    #     # if (isFALSE(x$nap_f_n)) break # 1
    #     # if (isFALSE(x$reasons_f_e)) break # 1
    # }

    # "if (isFALSE(x$nap_w_m))" and "if (isFALSE(x$reasons_f_e))"
    set.seed(1)
    x <- random_shift_mctq()
    expect_false(x$nap_f_n)

    # Invalid values for `n_w` and `n_f`
    expect_error(random_shift_mctq(n_w = ""))
    expect_error(random_shift_mctq(n_w = c(1, 1)))
    expect_error(random_shift_mctq(n_f = ""))
    expect_error(random_shift_mctq(n_f = c(1, 1)))
})

test_that("normalize() | general test", {
    # else if (check_2)
    min <- hms::parse_hm("22:00") + lubridate::ddays(1)
    max <- hms::parse_hm("05:00")
    mean <- hms::parse_hm("02:00")
    object <- normalize(min, max, mean)
    expected <- hms::hms(as.numeric(hms::parse_hm("02:00") +
                                        lubridate::ddays()))
    expect_equal(object$mean, expected)

    # if (check_1)
    min <- hms::parse_hm("01:00")
    max <- hms::parse_hm("12:00") + lubridate::ddays(1)
    mean <- hms::parse_hm("06:00")
    object <- normalize(min, max, mean)
    expected <- hms::parse_hm("12:00")
    expect_equal(object$max, expected)

    # "'mean' can't be found within the interval between 'min' and 'max'"
    min <- hms::parse_hm("12:00")
    max <- hms::parse_hm("03:00")
    mean <- hms::parse_hm("06:00")
    expect_error(normalize(min, max, mean))

    # Invalid values for `min`, `max`, `mean`, and `ambiguity`
    test <- c(hms::hms(1), hms::hms(1))

    expect_error(normalize("", hms::hms(1), hms::hms(1)))
    expect_error(normalize(hms::hms(1), "", hms::hms(1)))
    expect_error(normalize(hms::hms(1), hms::hms(1), ""))
    expect_error(normalize(test, hms::hms(1), hms::hms(1)))
    expect_error(normalize(hms::hms(1), test, hms::hms(1)))
    expect_error(normalize(hms::hms(1), hms::hms(1), test))
    expect_error(normalize(hms::hms(1), hms::hms(1), hms::hms(1), 1))
})

test_that("sample_time() | general test", {
    lower <- as.numeric(hms::parse_hms("00:00:00"))
    max <- as.numeric(hms::parse_hms("23:59:59"))
    object <- as.numeric(sample_time())
    checkmate::expect_numeric(object, lower = lower, max = max)

    set.seed(1)
    expect_equal(sample_time(), hms::parse_hms("13:50:00"))

    # "You cannot take a sample larger than the population [...]"
    expect_error(sample_time(
        min = lubridate::dseconds(1),
        max = lubridate::dseconds(3),
        by = lubridate::dseconds(1),
        size = 100,
        replace = FALSE))

    # Invalid values for `class`, `min`, `max`, `by`, `size`, `replace`
    # and `prob`
    test <- c(hms::hms(1), hms::hms(1))

    expect_error(sample_time(class = ""))
    expect_error(sample_time(min = ""))
    expect_error(sample_time(max = ""))
    expect_error(sample_time(by = ""))
    expect_error(sample_time(min = test))
    expect_error(sample_time(max = test))
    expect_error(sample_time(by = test))
    expect_error(sample_time(replace = ""))
    expect_error(sample_time(size = ""))
    expect_error(sample_time(size = - 1))
    expect_error(sample_time(prob = ""))
})

test_that("sampler_1() | general test", {
    set.seed(1)
    x <- list(name = "a", min = hms::parse_hm("23:00"),
              max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
              sd = hms::parse_hm("01:00"))
    by <- hms::parse_hm("00:05")
    envir <- new.env()
    sampler_1(x, by, envir)
    expect_equal(envir$a, hms::parse_hm("09:40"))

    # Invalid values for `x`, `by`, and `envir`
    test_1 <- list(a = 1, b = 2)
    test_2 <- list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L)

    expect_error(sampler_1("", hms::hms(1), envir = environment()))
    expect_error(sampler_1(test_1, hms::hms(1), envir = environment()))
    expect_error(sampler_1(test_2, hms::hms(1), envir = environment()))
    expect_error(sampler_1(x, "", envir = environment()))
    expect_error(sampler_1(x, hms::hms(1), envir = ""))
})

test_that("sampler_2() | general test", {
    set.seed(1)
    x <- list(name = "a_f", min = hms::parse_hm("23:00"),
              max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
              sd = hms::parse_hm("01:00"))
    by <- hms::parse_hm("00:05")
    envir <- new.env()
    envir$a_w <- hms::parse_hm("10:00")
    sampler_2(x, by, envir)
    expect_equal(envir$a_f, hms::parse_hm("08:20"))

    # Invalid values for `x`, `by`, and `envir`
    test_1 <- list(a = 1, b = 2)
    test_2 <- list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L)

    expect_error(sampler_2("", hms::hms(1), envir = environment()))
    expect_error(sampler_2(test_1, hms::hms(1), envir = environment()))
    expect_error(sampler_2(test_2, hms::hms(1), envir = environment()))
    expect_error(sampler_2(x, "", envir = environment()))
    expect_error(sampler_2(x, hms::hms(1), envir = ""))
})

test_that("sampler_3() | general test", {
    set.seed(1)
    x <- list(name = "a_f", min = hms::parse_hm("23:00"),
              max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
              sd = hms::parse_hm("01:00"))
    y <- "b"
    by <- hms::parse_hm("00:05")
    envir <- new.env()
    envir$a_w <- hms::parse_hm("10:00")
    envir$b_w <- hms::parse_hm("00:00")
    envir$b_f <- hms::parse_hm("11:00")
    sampler_3(x, y, by, envir)
    expect_equal(envir$a_f, hms::parse_hm("08:20"))

    # Invalid values for `x`, `by`, and `envir`
    test_1 <- list(a = 1, b = 2)
    test_2 <- list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L)

    expect_error(sampler_3("", "1", hms::hms(1), envir = environment()))
    expect_error(sampler_3(test_1, "1", hms::hms(1), envir = environment()))
    expect_error(sampler_3(test_2, "1", hms::hms(1), envir = environment()))
    expect_error(sampler_3(x, 1, hms::hms(1), envir = environment()))
    expect_error(sampler_3(x, "", envir = environment()))
    expect_error(sampler_3(x, hms::hms(1), envir = ""))
})

test_that("sampler_4() | general test", {
    set.seed(1)
    x <- list(name = "a_f", min = hms::parse_hm("00:00"),
              max = hms::parse_hm("01:00"), mean = hms::parse_hm("00:30"),
              sd = hms::parse_hm("00:15"))
    by <- hms::parse_hm("00:05")
    envir <- new.env()
    envir$a_w <- lubridate::dhours(24)
    sampler_4(x, by, envir)
    expect_equal(envir$a_f, lubridate::duration(3300))

    # Invalid values for `x`, `by`, and `envir`
    test_1 <- list(a = 1, b = 2)
    test_2 <- list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L)

    expect_error(sampler_4("", hms::hms(1), envir = environment()))
    expect_error(sampler_4(test_1, hms::hms(1), envir = environment()))
    expect_error(sampler_4(test_2, hms::hms(1), envir = environment()))
    expect_error(sampler_4(x, "", envir = environment()))
    expect_error(sampler_4(x, hms::hms(1), envir = ""))
})
