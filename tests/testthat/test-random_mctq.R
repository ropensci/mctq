test_that("random_mctq() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    set.seed(1)
    checkmate::expect_list(shush(random_mctq(model = "standard")))
    checkmate::expect_list(shush(random_mctq(model = "micro")))
    checkmate::expect_list(shush(random_mctq(model = "shift")))

    checkmate::expect_subset(
        c("bt_w", "le_w"),
        names(shush(random_mctq(model = "standard")))
        )

    checkmate::expect_subset(
        c("so_w", "se_w"),
        names(shush(random_mctq(model = "micro")))
        )

    checkmate::expect_subset(
        c("napo_w_m", "napo_f_e"),
        names(shush(random_mctq(model = "shift")))
        )
})

test_that("random_mctq() | error test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))
    expect_error(random_mctq(model = 1), "Assertion on 'model' failed")
})

test_that("random_std_mctq() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # # Finder
    #
    # for (i in seq_len(100)) {
    #     set.seed(i)
    #     x <- random_std_mctq()
    #
    #     # if (x$work == FALSE) break # 7
    #     # if (isFALSE(x$alarm_w)) break # 1
    #     # if (isFALSE(x$reasons_f)) break # 2
    # }

    # if (work == FALSE)
    set.seed(7)
    x <- random_std_mctq()
    expect_false(x$work)

    # if (isFALSE(reasons_f))
    set.seed(2)
    x <- random_std_mctq()
    check <- shorter_interval(x$bt_w, x$bt_f)
    check <- lubridate::int_end(check)
    expect_true(hms::as_hms(check) == x$bt_f)

    # if (isFALSE(alarm_w))
    set.seed(1)
    x <- random_std_mctq()
    expect_true(x$si_f >= x$si_w)
})

test_that("random_micro_mctq() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    set.seed(1)
    x <- random_micro_mctq()
    expect_true(x$se_f > x$se_w)
})

test_that("random_shift_mctq() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # # Finder
    #
    # for (i in seq_len(100)) {
    #     set.seed(i)
    #     x <- random_shift_mctq()
    #
    #     # if (isFALSE(x$nap_f_n)) break # 1
    #     # if (isFALSE(x$reasons_f_e)) break # 1
    # }

    # if (isFALSE(x$nap_w_m))" and "if (isFALSE(x$reasons_f_e))
    set.seed(1)
    x <- random_shift_mctq()
    expect_false(x$nap_f_n)

    # checkmate::assert_integerish(n_w, lower = 0, any.missing = FALSE, len = 3)
    expect_error(random_shift_mctq(n_w = ""), "Assertion on 'n_w' failed")

    expect_error(random_shift_mctq(
        n_w = c(-1, 1, 1)
    ),
    "Assertion on 'n_w' failed"
    )

    expect_error(random_shift_mctq(
        n_w = c(1, 1, NA)
    ),
    "Assertion on 'n_w' failed"
    )

    expect_error(random_shift_mctq(n_w = c(1, 1)), "Assertion on 'n_w' failed")

    # checkmate::assert_integerish(n_f, lower = 0, any.missing = FALSE, len = 3)
    expect_error(random_shift_mctq(n_f = ""), "Assertion on 'n_f' failed")

    expect_error(random_shift_mctq(
        n_f = c(-1, 1, 1)
    ),
    "Assertion on 'n_f' failed"
    )

    expect_error(random_shift_mctq(
        n_f = c(1, 1, NA)
    ),
    "Assertion on 'n_f' failed"
    )

    expect_error(random_shift_mctq(n_f = c(1, 1)), "Assertion on 'n_f' failed")
})

test_that("normalize() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    # else if (check_2)
    object <- normalize(
        min = hms::parse_hm("22:00") + lubridate::ddays(1),
        max = hms::parse_hm("05:00"), mean = hms::parse_hm("02:00"),
        ambiguity = 24
    )

    expect_equal(
        object$mean,
        hms::hms(as.numeric(hms::parse_hm("02:00") + lubridate::ddays()))
    )

    # if (check_1)
    object <- normalize(
        min = hms::parse_hm("01:00"),
        max = hms::parse_hm("12:00") + lubridate::ddays(1),
        mean = hms::parse_hm("06:00"), ambiguity = 24
    )

    expect_equal(object$max, hms::parse_hm("12:00"))

    expect_error(normalize(
        min = hms::parse_hm("12:00"), max = hms::parse_hm("03:00"),
        mean = hms::parse_hm("06:00"), ambiguity = 24
    ),
    "'mean' cannot be found within the interval between 'min' "
    )

    # checkmate::assert_multi_class(min, classes)
    expect_error(normalize(
        min = "", max = hms::hms(1), mean = hms::hms(1), ambiguity = 24
    ),
    "Assertion on 'min' failed"
    )

    # assert_length_one(min)
    expect_error(normalize(
        min = c(hms::hms(1), hms::hms(1)), max = hms::hms(1),
        mean = hms::hms(1), ambiguity = 24
    ),
    "Assertion on 'min' failed"
    )

    # checkmate::assert_multi_class(max, classes)
    expect_error(normalize(
        min = hms::hms(1), max = "", mean = hms::hms(1), ambiguity = 24
    ),
    "Assertion on 'max' failed"
    )

    # assert_length_one(max)
    expect_error(normalize(
        min = hms::hms(1), max = c(hms::hms(1), hms::hms(1)),
        mean = hms::hms(1), ambiguity = 24
    ),
    "Assertion on 'max' failed"
    )

    # checkmate::assert_multi_class(mean, classes)
    expect_error(normalize(
        min = hms::hms(1), max = hms::hms(1), mean = "", ambiguity = 24
    ),
    "Assertion on 'mean' failed"
    )

    # assert_length_one(mean)
    expect_error(normalize(
        min = hms::hms(1), max = hms::hms(1),
        mean = c(hms::hms(1), hms::hms(1)), ambiguity = 24
    ),
    "Assertion on 'mean' failed"
    )

    # checkmate::assert_choice(ambiguity, c(0, 24 , NA))
    expect_error(normalize(
        min = hms::hms(1), max = hms::hms(1), mean = hms::hms(1), ambiguity = 1
    ),
    "Assertion on 'ambiguity' failed"
    )
})

test_that("sample_time() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    checkmate::expect_numeric(
        as.numeric(sample_time()),
        lower = as.numeric(hms::parse_hms("00:00:00")),
        max = as.numeric(hms::parse_hms("23:59:59"))
    )

    set.seed(1)
    expect_equal(sample_time(), hms::parse_hms("13:50:00"))

    # checkmate::assert_multi_class(min, classes)
    expect_error(sample_time(min = ""), "Assertion on 'min' failed")

    # assert_length_one(min)
    expect_error(sample_time(
        min = c(hms::hms(1), hms::hms(1))
    ),
    "Assertion on 'min' failed"
    )

    # checkmate::assert_multi_class(max, classes)
    expect_error(sample_time(max = ""), "Assertion on 'max' failed")

    # assert_length_one(max)
    expect_error(sample_time(
        max = c(hms::hms(1), hms::hms(1))
    ),
    "Assertion on 'max' failed"
    )

    # checkmate::assert_multi_class(by, classes)
    expect_error(sample_time(by = ""), "Assertion on 'by' failed")

    # assert_length_one(by)
    expect_error(sample_time(
        by = c(hms::hms(1), hms::hms(1))
    ),
    "Assertion on 'by' failed"
    )

    # checkmate::assert_flag(replace)
    expect_error(sample_time(replace = ""), "Assertion on 'replace' failed")

    # checkmate::assert_number(size, lower = 0)
    expect_error(sample_time(size = ""), "Assertion on 'size' failed")
    expect_error(sample_time(size = - 1), "Assertion on 'size' failed")

    # checkmate::assert_numeric(prob, null.ok = TRUE)
    expect_error(sample_time(prob = ""), "Assertion on 'prob' failed")

    # if (size > length(seq(min, max, by)) && isFALSE(replace)) {
    expect_error(sample_time(
        min = lubridate::dseconds(1), max = lubridate::dseconds(3),
        by = lubridate::dseconds(1), size = 100, replace = FALSE, prob = NULL
    ),
    "You cannot take a sample larger than the population "
    )
})

test_that("sampler_1() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    set.seed(1)
    envir <- new.env()
    sampler_1(
        x = list(
            name = "a", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        by = hms::parse_hm("00:05"), envir = envir
        )
    expect_equal(envir$a, hms::parse_hm("09:40"))

    # checkmate::assert_list(x)
    expect_error(sampler_1(
        x = "", by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'x' failed"
    )

    # checkmate::assert_names(names(x), identical.to = names)
    expect_error(sampler_1(
        x = list(a = 1, b = 2), by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'names\\(x\\)' failed"
    )

    # lapply(x, checkmate::assert_multi_class, classes = classes)
    expect_error(sampler_1(
        x = list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L),
        by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'X\\[\\[i\\]\\]' failed"
    )

    # checkmate::assert_multi_class(by, classes)
    expect_error(sampler_1(
        x = list(
            name = "a", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        by = "", envir = environment()
    ),
    "Assertion on 'by' failed"
    )

    # checkmate::assert_environment(envir)
    expect_error(sampler_1(
        x = list(
            name = "a", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        by = hms::hms(1), envir = ""
    ),
    "Assertion on 'envir' failed"
    )
})

test_that("sampler_2() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    set.seed(1)
    envir <- new.env()
    envir$a_w <- hms::parse_hm("10:00")
    sampler_2(list(
        name = "a_f", min = hms::parse_hm("23:00"),
        max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
        sd = hms::parse_hm("01:00")
    ),
    by = hms::parse_hm("00:05"), envir = envir
    )
    expect_equal(envir$a_f, hms::parse_hm("08:20"))

    # checkmate::assert_list(x)
    expect_error(sampler_2(
        x = "", by = hms::hms(1), envir = environment()
        ),
                 "Assertion on 'x' failed"
        )

    # checkmate::assert_names(names(x), identical.to = names)
    expect_error(sampler_2(
        x = list(a = 1, b = 2), by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'names\\(x\\)' failed"
    )

    # lapply(x, checkmate::assert_multi_class, classes = classes)
    expect_error(sampler_2(
        x = list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L),
        by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'X\\[\\[i\\]\\]' failed"
    )

    # checkmate::assert_multi_class(by, classes)
    expect_error(sampler_2(
        x = list(
            name = "a_f", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        by = "", envir = environment()
    ),
    "Assertion on 'by' failed"
    )

    # checkmate::assert_environment(envir)
    expect_error(sampler_2(
        x = list(
            name = "a_f", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        by = hms::hms(1), envir = ""
    ),
    "Assertion on 'envir' failed"
    )
})

test_that("sampler_3() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    set.seed(1)
    envir <- new.env()
    envir$a_w <- hms::parse_hm("10:00")
    envir$b_w <- hms::parse_hm("00:00")
    envir$b_f <- hms::parse_hm("11:00")
    sampler_3(
        x = list(
        name = "a_f", min = hms::parse_hm("23:00"),
        max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
        sd = hms::parse_hm("01:00")
        ),
        y = "b", by = hms::parse_hm("00:05"), envir = envir
        )
    expect_equal(envir$a_f, hms::parse_hm("08:20"))

    # checkmate::assert_list(x)
    expect_error(sampler_3(
        x = "", y = "1", by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'x' failed"
    )

    # checkmate::assert_names(names(x), identical.to = names)
    expect_error(sampler_3(
        x = list(a = 1, b = 2), y = "1", by = hms::hms(1),
        envir = environment()
    ),
    "Assertion on 'names\\(x\\)' failed"
    )

    # lapply(x, checkmate::assert_multi_class, classes = classes)
    expect_error(sampler_3(
        x = list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L),
        y = "1", by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'X\\[\\[i\\]\\]' failed"
    )

    # checkmate::assert_string(y, min.chars = 1)
    expect_error(sampler_3(
        x = list(
            name = "a_f", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        y = 1, by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'y' failed"
    )

    # checkmate::assert_string(y, min.chars = 1)
    expect_error(sampler_3(
        x = list(
            name = "a_f", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        y = "", by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'y' failed"
    )

    # checkmate::assert_multi_class(by, classes)
    expect_error(sampler_3(
        x = list(
            name = "a_f", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        y = "1", by = "", envir = environment()
    ),
    "Assertion on 'by' failed"
    )

    # checkmate::assert_environment(envir)
    expect_error(sampler_3(
        x = list(
            name = "a_f", min = hms::parse_hm("23:00"),
            max = hms::parse_hm("16:00"), mean = hms::parse_hm("10:00"),
            sd = hms::parse_hm("01:00")
        ),
        y = "1", by = hms::hms(1), envir = ""
    ),
    "Assertion on 'envir' failed"
    )
})

test_that("sampler_4() | general test", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    
    set.seed(1)
    envir <- new.env()
    envir$a_w <- lubridate::dhours(24)
    sampler_4(
        x = list(
            name = "a_f", min = hms::parse_hm("00:00"),
            max = hms::parse_hm("01:00"), mean = hms::parse_hm("00:30"),
            sd = hms::parse_hm("00:15")
        ),
        by = hms::parse_hm("00:05"), envir = envir
    )
    expect_equal(envir$a_f, lubridate::duration(3300))

    # checkmate::assert_list(x)
    expect_error(sampler_4(
        x = "", by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'x' failed"
    )

    # checkmate::assert_names(names(x), identical.to = names)
    expect_error(sampler_4(
        x = list(a = 1, b = 2), by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'names\\(x\\)' failed"
    )

    # lapply(x, checkmate::assert_multi_class, classes = classes)
    expect_error(sampler_4(
        x = list(name = "", min = 1L, max = 1L, mean = 1L, sd = 1L),
        by = hms::hms(1), envir = environment()
    ),
    "Assertion on 'X\\[\\[i\\]\\]' failed"
    )

    # checkmate::assert_multi_class(by, classes)
    expect_error(sampler_4(
        x = list(
            name = "a_f", min = hms::parse_hm("00:00"),
            max = hms::parse_hm("01:00"), mean = hms::parse_hm("00:30"),
            sd = hms::parse_hm("00:15")
        ),
        by = "", envir = environment()
    ),
    "Assertion on 'by' failed"
    )

    # checkmate::assert_environment(envir)
    expect_error(sampler_4(
        x = list(
            name = "a_f", min = hms::parse_hm("00:00"),
            max = hms::parse_hm("01:00"), mean = hms::parse_hm("00:30"),
            sd = hms::parse_hm("00:15")
        ),
        by = hms::hms(1), envir = ""
    ),
    "Assertion on 'envir' failed"
    )
})
