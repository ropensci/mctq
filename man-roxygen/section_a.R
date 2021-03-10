#' @section Missing sections in standard and micro MCTQ versions:
#'
#' Although the standard and micro versions of the MCTQ asks for subjects to
#' complete the workdays and work-free days sections, even when he/she do not
#' have a regular work schedule (`wd = 0`) or have a 7 day/week work schedule
#' (`wd = 7`), some of them may still end skipping one of this parts of the
#' questionnaire. In those cases, `sd_week()`, `sloss_week()`, `le_week()`,
#' `msf_sc()`, `sjl_rel()`, and `sjl()` will produce `NA` (Not Available) as
#' output. That's because those computations combine workdays and work-free days
#' variables.
#'
#' For those special standard and micro MCTQ cases, where one section is
#' missing, a `NA` value is the correct output for the functions mentioned above
#' when `wd` (number of workdays per week) are `wd > 0 & wd < 7`, but it may not
#' be when `wd == 0` or `wd == 7`. There are different approaches to deal with
#' those cases. See `vignette("missing-sections", package = "mctq")` to learn
#' more.
