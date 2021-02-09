test_that("dialog_line()", {
    expect_equal(dialog_line(abort = TRUE), 999)
    expect_equal(dialog_line(), 999)
})

# test_that("dialog_menu()", {
#     expect_equal(dialog_menu(abort = TRUE), 999)
#     expect_equal(dialog_menu(), 999)
# })
