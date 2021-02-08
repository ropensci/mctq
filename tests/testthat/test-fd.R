test_that("fd() | scalar test", {
    expect_equal(fd(0), 7)
    expect_equal(fd(1), 6)
    expect_equal(fd(2), 5)
    expect_equal(fd(3), 4)
    expect_equal(fd(4), 3)
    expect_equal(fd(5), 2)
    expect_equal(fd(6), 1)
    expect_equal(fd(7), 0)
})

test_that("fd() | vector test", {
    expect_equal(fd(c(1, 5)), c(6, 2))
    expect_equal(fd(c(4, 2)), c(3, 5))
    expect_equal(fd(c(3, NA)), c(4, NA))
})

test_that("fd() | error test", {
    # Invalid values for `wd`
    expect_error(fd("test"))
    expect_error(fd(1.5))
    expect_error(fd(10))
    expect_error(fd(-1))
    expect_error(fd(c(1, 10)))
    expect_error(fd(lubridate::dhours(1)))
    expect_error(fd(lubridate::minutes(1)))
})
