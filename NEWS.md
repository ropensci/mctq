# mctq 0.0.0.9001

## Added

* @jonkeane was added as a reviewer ('rev').
* @leocadio-miguel was added as a reviewer ('rev').
* The user interface has gotten more stylish, thanks to the [`cli`](https://cli.r-lib.org) package (now on imports).
* A new vignette was introduced, explaining why the `mctq` package use `Duration` instead of `Period` (objects from the [lubridate](https://lubridate.tidyverse.org/) package) as the default object for time spans.
* `cycle_time()`, a function to cycle time spans, was introduced.

## Changed

* To avoid any unknown compatibility issues, all packages on imports now require the latest version of them at the moment of this release.
* `assign_date()` now returns only `Interval` objects.
* `convert()` and all `convert_*()` functions were removed. See a dedicated note about this below.
* `round_time()` is now a S3 generic.
* `shortest_interval()` was renamed to `shorter_interval()`.
* `shorter_interval()` and `longer_interval()` now returns only `Interval` objects.
* `sum_time()` now have different arguments and was divided in two functions: `sum_time()` (for non-vectorized sums) and `vct_sum_time()` (for vectorized sums).
* `sum_time()` now only returns `Duration` objects.
* The `sd()` function was renamed to `sdu()`. See a dedicated note about this below.
* The `ms()` function was renamed to `msl()`. See a dedicated note about this below.

## Fixed

* Several typos were fixed in the documentation.
* Several functions were optimized.

## Note about removing `convert()`

`convert()` was created considering the user experience (sleep and chronobiology scientists). Since most of them don't have much experience with R and that time can have different types of representations (e.g., decimal hours, radian), `convert()` aim was to help transpose those difficulties, posing as an "universal translator" (🖖).

However, after much thought and consideration, we believe that the `convert()` feature may be out of the `mctq` scope. It can maybe be part of another package (a `lubritime` package perhaps? 😄). Other `mctq` tools, like `shorter_interval()` and `sum_time()`, could also be a part of that package (but are necessary in `mctq` for the time being). Hence, we decided to remove `convert()` and to instruct the user to check the [lubridate](https://lubridate.tidyverse.org/) and [hms](https://hms.tidyverse.org/) packages for parsing/conversion.

## Note about renaming `sd()` and `ms()`

That was a tough, but necessary, call. Although we tried to preserve the original author's naming pattern for the MCTQ functions, the name `sd` provokes a dangerous name collision with the widely used `stats::sd` function (standard deviation) and the name `ms` provokes a name collision with `lubridate::ms()` function (a function for parsing minutes and seconds components). That's why we decided to renamed them. `sdu()` and `msl()` are the only exceptions, all the other `mctq` functions maintain a strong naming resemblance with the original author's naming pattern.


# mctq 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
