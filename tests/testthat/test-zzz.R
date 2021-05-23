test_that(".onAttach() | general test", {
    # is_interactive <- mctq:::is_interactive

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            .onAttach())
    }

    # mock()
    expect_null(mock(), NULL)
})
