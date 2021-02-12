test_that("is_interactive() | general test", {
    expect_equal(is_interactive(), interactive())
})

test_that("is_namespace_loaded() | general test", {
    x <- "base"
    expect_equal(is_namespace_loaded(x), isNamespaceLoaded(x))
})

# test_that("read_line() | general test", {
#     expect_equal(read_line(""), "")
# })
