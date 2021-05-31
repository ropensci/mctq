test_that("msf_sc() | scalar test", {
    expect_equal(msf_sc(hms::parse_hm("04:00"),
                        lubridate::dhours(6),
                        lubridate::dhours(8),
                        sd_week(lubridate::dhours(6),
                                lubridate::dhours(8),
                                5),
                        FALSE),
                 hms::parse_hms("03:17:08.571429"))
    expect_equal(msf_sc(hms::parse_hm("23:30"),
                        lubridate::dhours(7),
                        lubridate::dhours(9),
                        sd_week(lubridate::dhours(7),
                                lubridate::dhours(9),
                                3),
                        FALSE),
                 hms::parse_hms("23:04:17.142857"))
    expect_equal(msf_sc(hms::parse_hm("02:15"),
                        lubridate::dhours(9),
                        lubridate::dhours(7),
                        sd_week(lubridate::dhours(9),
                                lubridate::dhours(7),
                                5),
                        FALSE),
                 hms::parse_hm("02:15"))
    expect_equal(msf_sc(hms::parse_hm("00:00"),
                        lubridate::dhours(6.15),
                        lubridate::dhours(8.25),
                        lubridate::as.duration(NA),
                        FALSE),
                 hms::as_hms(NA))
    expect_equal(msf_sc(hms::parse_hm("01:00"),
                        lubridate::dhours(8),
                        lubridate::dhours(5.5),
                        lubridate::as.duration(NA),
                        FALSE),
                 hms::parse_hm("01:00"))
    expect_equal(msf_sc(hms::parse_hm("02:25"),
                        lubridate::as.duration(NA),
                        lubridate::dhours(9.5),
                        sd_week(lubridate::as.duration(NA),
                                lubridate::dhours(9.5),
                                5),
                        FALSE),
                 hms::as_hms(NA))
    expect_equal(msf_sc(hms::parse_hm("21:15"),
                        lubridate::dhours(8),
                        lubridate::dhours(8),
                        sd_week(lubridate::dhours(8),
                                lubridate::dhours(8),
                                6),
                        TRUE),
                 hms::as_hms(NA))
})

test_that("msf_sc() | vector test", {
    expect_equal(msf_sc(c(hms::parse_hm("03:45"), hms::parse_hm("22:30")),
                        c(lubridate::dhours(6), lubridate::dhours(5.5)),
                        c(lubridate::dhours(7.5), lubridate::dhours(8)),
                        c(sd_week(lubridate::dhours(6),
                                  lubridate::dhours(7.5),
                                  5),
                          sd_week(lubridate::dhours(5.5),
                                  lubridate::dhours(8),
                                  4)),
                        c(FALSE, FALSE)),
                 c(hms::parse_hms("03:12:51.428571"),
                   hms::parse_hms("21:47:08.571429")))
    expect_equal(msf_sc(c(hms::parse_hm("03:45"), hms::parse_hm("21:30")),
                        c(lubridate::dhours(7), lubridate::dhours(6.5)),
                        c(lubridate::dhours(7), lubridate::dhours(8)),
                        c(sd_week(lubridate::dhours(7),
                                  lubridate::dhours(7),
                                  5),
                          sd_week(lubridate::dhours(7),
                                  lubridate::dhours(8),
                                  5)),
                        c(FALSE, TRUE)),
                 c(hms::parse_hm("03:45"), hms::as_hms(NA)))
    expect_equal(msf_sc(c(hms::parse_hm("01:20"), hms::as_hms(NA)),
                        c(lubridate::dhours(5), lubridate::dhours(7)),
                        c(lubridate::dhours(8.5), lubridate::dhours(9)),
                        c(lubridate::as.duration(NA),
                          sd_week(lubridate::dhours(7),
                                  lubridate::dhours(9),
                                  3)),
                        c(FALSE, FALSE)),
                 c(hms::as_hms(NA), hms::as_hms(NA)))
})

test_that("msf_sc() | error test", {
    expect_error(msf_sc(1, lubridate::duration(1), lubridate::duration(1),
                        lubridate::duration(1), TRUE),
                 "Assertion on 'msf' failed")
    expect_error(msf_sc(hms::hms(1), 1, lubridate::duration(1),
                        lubridate::duration(1), TRUE),
                 "Assertion on 'sd_w' failed")
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1), 1,
                        lubridate::duration(1), TRUE),
                 "Assertion on 'sd_f' failed")
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1),
                        lubridate::duration(1), 1, TRUE),
                 "Assertion on 'sd_week' failed")
    expect_error(msf_sc(hms::hms(1), lubridate::duration(1),
                        lubridate::duration(1), lubridate::duration(1), 1),
                 "Assertion on 'alarm_f' failed")

    expect_error(msf_sc(hms::hms(1), lubridate::duration(1),
                        lubridate::duration(1), lubridate::duration(1),
                        c(TRUE, FALSE)),
                 "'msf', 'sd_w', 'sd_f', 'sd_week', and 'alarm_f' must have ")
})

test_that("msf_sc() | wrappers", {
    expect_equal(msf_sc, chronotype)
})
