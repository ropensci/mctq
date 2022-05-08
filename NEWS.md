<!--- https://devguide.ropensci.org/releasing.html -->
<!--- https://style.tidyverse.org/news.html -->
<!--- https://semver.org/ -->

# mctq 0.2.0 (2022-05-08)

* Some minor documentation issues were resolved.
* `shorter_interval()` now don't have the `inverse` argument. Use `longer_interval()` instead.
* `shorter_duration()` and `longer_duration()`, two functions that return the shorter and the longer duration between two hours, were introduced.
* `sjl_sc()` and `sjl_sc_rel()`, two functions that allow the user to compute Jankowski's MCTQ sleep-corrected social jet lag, were introduced.


# mctq 0.1.0 (2021-11-04)

* Initial [CRAN](https://cran.r-project.org/package=mctq) release. 🎉
* You can now install `mctq` with `install.packages("mctq")`.
* We decided to wait for a little while before releasing a `1.0.0` (stable) 
`mctq` version. We don't intend to make any breaking changes, but we think is
better to wait and see if the user community don't have any issues with the
features.


# mctq 0.0.0.9002 (prerelease)

* `mctq` is now a
[peer-reviewed](https://github.com/ropensci/software-review/issues/434) package
by @ropensci! 🎉
* The package repository was transferred to the @ropensci organization. All
links related to `mctq` have been changed. Old links have a redirect protocol to
point to the new repository and new website.


# mctq 0.0.0.9001 (prerelease)

* @jonkeane was added as a reviewer ('rev').
* @leocadio-miguel was added as a reviewer ('rev').

## Breaking changes

* `assign_date()` now returns only `Interval` objects.
* `convert()` and all `convert_*()` functions were removed. See a dedicated note
about this below.
* `ms()` was renamed to `msl()`. See a dedicated note about this below.
* `sd()` was renamed to `sdu()`. See a dedicated note about this below.
* `shortest_interval()` was renamed to `shorter_interval()`.
* `shorter_interval()` and `longer_interval()` now returns only `Interval`
objects.
* `sum_time()` now have different arguments and was divided in two functions:
`sum_time()` (for non-vectorized sums) and `vct_sum_time()` (for vectorized
sums).
* `sum_time()` now returns only `Duration` objects.
* To avoid any unknown compatibility issues, all packages on imports will now
require the latest version of them at the moment of release.

## New features

* `cycle_time()`, a function to cycle time spans, was introduced.

## Minor improvements and fixes

* `round_time()` is now a S3 generic.
* The user interface has gotten more stylish, thanks to the
[`cli`](https://cli.r-lib.org) package (now on imports).
* A new vignette was introduced, explaining why the `mctq` package use
`Duration` instead of `Period` (objects from the
[lubridate](https://lubridate.tidyverse.org/) package) as the default object for
time spans.
* Several typos were fixed in the documentation.
* Several functions were optimized.

## Note about removing `convert()`

`convert()` was created considering the user experience (sleep and chronobiology
scientists). Since most of them don't have much experience with R and that time
can have different types of representations (e.g., decimal hours, radian),
`convert()` aim was to help transpose those difficulties, posing as an
"universal translator" (🖖).

However, after much thought and consideration, we believe that the `convert()`
feature may be out of the `mctq` scope. It can maybe be part of another package
(a `lubritime` package perhaps? 😄). Other `mctq` tools, like
`shorter_interval()` and `sum_time()`, could also be a part of that package (but
are necessary in `mctq` for the time being). Hence, we decided to remove
`convert()` and to instruct the user to check the
[lubridate](https://lubridate.tidyverse.org/) and
[hms](https://hms.tidyverse.org/) packages for parsing/conversion.

## Note about renaming `sd()` and `ms()`

That was a tough, but necessary, call. Although we tried to preserve the
original author's naming pattern for the MCTQ functions, the name `sd` provokes
a dangerous name collision with the widely used `stats::sd()` function (standard
deviation) and the name `ms` provokes a name collision with `lubridate::ms()`
(a function for parsing minutes and seconds components). That's why we
decided to renamed them. `sdu()` and `msl()` are the only exceptions, all the
other `mctq` functions maintain a strong naming resemblance with the original
author's naming pattern.


# mctq 0.0.0.9000 (prerelease)

* Added a `NEWS.md` file to track changes to the package.
