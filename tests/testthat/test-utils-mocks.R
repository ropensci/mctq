test_that("is_interactive() | general test", {
    expect_equal(is_interactive(), interactive())
})

test_that("require_namespace() | general test", {
    x <- "base"
    expect_equal(require_namespace(x), requireNamespace(x, quietly = TRUE))
})

test_that("read_line() | general test", {
    expect_equal(read_line(""), "")
})
