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
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # require_namespace <- mctq:::require_namespace
    # random_mctq <- mctq::random_mctq

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) FALSE,
            random_mctq())
    }

    # mock()
    expect_error(mock(), "This function requires the 'stats' package to run. ")

    expect_error(random_mctq(model = 1), "Assertion on 'model' failed")
    expect_error(random_mctq(quiet = 1), "Assertion on 'quiet' failed")
})

test_that("random_mctq() | message test", {
    expect_message(random_mctq(model = "standard"), "\nModel: Standard MCTQ\n")
    expect_message(random_mctq(model = "micro"), "\nModel: Micro MCTQ\n")
    expect_message(random_mctq(model = "shift"), "\nModel: MCTQ Shift\n")
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
    check <- shorter_interval(x$bt_w, x$bt_f, "Interval")
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
    expect_error(random_shift_mctq(n_w = ""), "Assertion on 'n_w' failed")
    expect_error(random_shift_mctq(n_w = c(1, 1)), "Assertion on 'n_w' failed")
    expect_error(random_shift_mctq(n_f = ""), "Assertion on 'n_f' failed")
    expect_error(random_shift_mctq(n_f = c(1, 1)), "Assertion on 'n_f' failed")
})

test_that("normalize() | general test", {
    # else if (check_2)
    object <- normalize(hms::parse_hm("22:00") + lubridate::ddays(1),
                        hms::parse_hm("05:00"),
                        hms::parse_hm("02:00"))
    expect_equal(object$mean,
                 hms::hms(as.numeric(hms::parse_hm("02:00") +
                                         lubridate::ddays())))

    # if (check_1)
    object <- normalize(hms::parse_hm("01:00"),
                        hms::parse_hm("12:00") + lubridate::ddays(1),
                        hms::parse_hm("06:00"))
    expect_equal(object$max, hms::parse_hm("12:00"))

    expect_error(normalize(hms::parse_hm("12:00"),
                           hms::parse_hm("03:00"),
                           hms::parse_hm("06:00")),
                 "'mean' can't be found within the interval between 'min' ")

    expect_error(normalize("", hms::hms(1), hms::hms(1)),
                 "Assertion on 'min' failed")
    expect_error(normalize(c(hms::hms(1), hms::hms(1)), hms::hms(1),
                           hms::hms(1)),
                 "Assertion on 'min' failed")
    expect_error(normalize(hms::hms(1), "", hms::hms(1)),
                 "Assertion on 'max' failed")
    expect_error(normalize(hms::hms(1), c(hms::hms(1), hms::hms(1)),
                           hms::hms(1)),
                 "Assertion on 'max' failed")
    expect_error(normalize(hms::hms(1), hms::hms(1), ""),
                 "Assertion on 'mean' failed")
    expect_error(normalize(hms::hms(1), hms::hms(1),
                           c(hms::hms(1), hms::hms(1))),
                 "Assertion on 'mean' failed")
    expect_error(normalize(hms::hms(1), hms::hms(1), hms::hms(1), 1),
                 "Assertion on 'ambiguity' failed")
})

test_that("sample_time() | general test", {
    checkmate::expect_numeric(as.numeric(sample_time()),
                              lower = as.numeric(hms::parse_hms("00:00:00")),
                              max = as.numeric(hms::parse_hms("23:59:59")))

    set.seed(1)
    expect_equal(sample_time(), hms::parse_hms("13:50:00"))

    expect_error(sample_time(min = lubridate::dseconds(1),
                             max = lubridate::dseconds(3),
                             by = lubridate::dseconds(1),
                             size = 100,
                             replace = FALSE),
                 "You cannot take a sample larger than the population ")

    expect_error(sample_time(class = ""),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(sample_time(min = ""), "Assertion on 'min' failed")
    expect_error(sample_time(min = c(hms::hms(1), hms::hms(1))),
                 "Assertion on 'min' failed")
    expect_error(sample_time(max = ""), "Assertion on 'max' failed")
    expect_error(sample_time(max = c(hms::hms(1), hms::hms(1))),
                 "Assertion on 'max' failed")
    expect_error(sample_time(by = ""), "Assertion on 'by' failed")
    expect_error(sample_time(by = c(hms::hms(1), hms::hms(1))),
                 "Assertion on 'by' failed")
    expect_error(sample_time(replace = ""), "Assertion on 'replace' failed")
    expect_error(sample_time(size = ""), "Assertion on 'size' failed")
    expect_error(sample_time(size = - 1), "Assertion on 'size' failed")
    expect_error(sample_time(prob = ""), "Assertion on 'prob' failed")
})

test_that("sampler_1() | general test", {
    set.seed(1)
    envir = new.env()
    sampler_1(list(name = "a",
                   min = hms::parse_hm("23:00"),
                   max = hms::parse_hm("16:00"),
                   mean = hms::parse_hm("10:00"),
                   sd = hms::parse_hm("01:00")),
              hms::parse_hm("00:05"),
              envir)
    expect_equal(envir$a, hms::parse_hm("09:40"))

    expect_error(sampler_1("", hms::hms(1), envir = environment()),
                 "Assertion on 'x' failed")
    expect_error(sampler_1(list(a = 1, b = 2),
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'names\\(x\\)' failed")
    expect_error(sampler_1(list(name = "", min = 1L, max = 1L, mean = 1L,
                                sd = 1L),
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(sampler_1(list(name = "a",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           "",
                           envir = environment()),
                 "Assertion on 'by' failed")
    expect_error(sampler_1(list(name = "a",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           hms::hms(1),
                           envir = ""),
                 "Assertion on 'envir' failed")
})

test_that("sampler_2() | general test", {
    set.seed(1)
    envir <- new.env()
    envir$a_w <- hms::parse_hm("10:00")
    sampler_2(list(name = "a_f",
                   min = hms::parse_hm("23:00"),
                   max = hms::parse_hm("16:00"),
                   mean = hms::parse_hm("10:00"),
                   sd = hms::parse_hm("01:00")),
              hms::parse_hm("00:05"),
              envir)
    expect_equal(envir$a_f, hms::parse_hm("08:20"))

    expect_error(sampler_2("", hms::hms(1), envir = environment()),
                 "Assertion on 'x' failed")
    expect_error(sampler_2(list(a = 1, b = 2), hms::hms(1),
                           envir = environment()),
                 "Assertion on 'names\\(x\\)' failed")
    expect_error(sampler_2(list(name = "", min = 1L, max = 1L, mean = 1L,
                                sd = 1L),
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(sampler_2(list(name = "a_f",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           "",
                           envir = environment()),
                 "Assertion on 'by' failed")
    expect_error(sampler_2(list(name = "a_f",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           hms::hms(1),
                           envir = ""),
                 "Assertion on 'envir' failed")
})

test_that("sampler_3() | general test", {
    set.seed(1)
    envir <- new.env()
    envir$a_w <- hms::parse_hm("10:00")
    envir$b_w <- hms::parse_hm("00:00")
    envir$b_f <- hms::parse_hm("11:00")
    sampler_3(list(name = "a_f",
                   min = hms::parse_hm("23:00"),
                   max = hms::parse_hm("16:00"),
                   mean = hms::parse_hm("10:00"),
                   sd = hms::parse_hm("01:00")),
              "b",
              hms::parse_hm("00:05"),
              envir)
    expect_equal(envir$a_f, hms::parse_hm("08:20"))

    expect_error(sampler_3("", "1", hms::hms(1), envir = environment()),
                 "Assertion on 'x' failed")
    expect_error(sampler_3(list(a = 1, b = 2),
                           "1",
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'names\\(x\\)' failed")
    expect_error(sampler_3(list(name = "", min = 1L, max = 1L, mean = 1L,
                                sd = 1L), "1",
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(sampler_3(list(name = "a_f",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           1,
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'y' failed")
    expect_error(sampler_3(list(name = "a_f",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           "",
                           envir = environment()),
                 "Assertion on 'y' failed")
    expect_error(sampler_3(list(name = "a_f",
                                min = hms::parse_hm("23:00"),
                                max = hms::parse_hm("16:00"),
                                mean = hms::parse_hm("10:00"),
                                sd = hms::parse_hm("01:00")),
                           hms::hms(1),
                           envir = ""),
                 "Assertion on 'y' failed")
})

test_that("sampler_4() | general test", {
    set.seed(1)
    envir <- new.env()
    envir$a_w <- lubridate::dhours(24)
    sampler_4(list(name = "a_f",
                   min = hms::parse_hm("00:00"),
                   max = hms::parse_hm("01:00"),
                   mean = hms::parse_hm("00:30"),
                   sd = hms::parse_hm("00:15")),
              hms::parse_hm("00:05"),
              envir)
    expect_equal(envir$a_f, lubridate::duration(3300))

    expect_error(sampler_4("", hms::hms(1), envir = environment()),
                 "Assertion on 'x' failed")
    expect_error(sampler_4(list(a = 1, b = 2),
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'names\\(x\\)' failed")
    expect_error(sampler_4(list(name = "", min = 1L, max = 1L, mean = 1L,
                                sd = 1L),
                           hms::hms(1),
                           envir = environment()),
                 "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(sampler_4(list(name = "a_f",
                                min = hms::parse_hm("00:00"),
                                max = hms::parse_hm("01:00"),
                                mean = hms::parse_hm("00:30"),
                                sd = hms::parse_hm("00:15")),
                           "",
                           envir = environment()),
                 "Assertion on 'by' failed")
    expect_error(sampler_4(list(name = "a_f",
                                min = hms::parse_hm("00:00"),
                                max = hms::parse_hm("01:00"),
                                mean = hms::parse_hm("00:30"),
                                sd = hms::parse_hm("00:15")),
                           hms::hms(1),
                           envir = ""),
                 "Assertion on 'envir' failed")
})
