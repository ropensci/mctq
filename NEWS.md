# mctq 0.0.0.9001

## Added

* @jonkeane was added as a reviewer ('rev').
* @leocadio-miguel was added as a reviewer ('rev').
* The user interface has gotten more stylish, thanks to the [`cli` package](https://cli.r-lib.org) (now on imports).
* A new vignette was introduced, explaining why the `mctq` package use `Duration` instead of `Period` (objects from the [lubridate](https://lubridate.tidyverse.org/) package) as the default object for time spans.

## Changed

* The DESCRIPTION file now requires a lubridate version `>= 1.7.9`. `vctrs` support was added in this version, fixing some issues related to rounding `Duration`, `Period`, and `Interval` objects. Click [here](https://github.com/tidyverse/lubridate/pull/871) to learn more.
* The DESCRIPTION file now requires a dplyr version `>= 0.2`. That's when the `dplyr` start to import the pipe operator. Click [here](https://github.com/tidyverse/dplyr/blob/master/NEWS.md#piping) to learn more.
* To avoid any unknown compatibility issues, all packages on imports now requires the latest version of them at the moment of this release.
* `convert()` and all `convert_*()` functions were removed. See a dedicated note about this below.
* `round_time()` is now a S3 generic.
* `shortest_interval()` was renamed to `shorter_interval()`.
* `shorter_interval()` and `longer_interval()` now return only `Interval` objects.
* `assign_date()` now return only `Interval` objects.
* `sum_time()` now have different arguments and was divided in two functions: `sum_time()` (for non-vectorized sums) and `vct_sum_time()` (for vectorized sums).
* The `sd()` function was renamed to `sdu()`. See a dedicated note about this below.
* Several functions were optimized.

## Fixed

* Several typos were fixed in the documentation.

## Note about removing `convert()`

`convert()` was created considering the user experience (sleep and chronobiology scientists). Since most of them don't have much experience with R and that time can have different types of representations (e.g., decimal hours, radian), `convert()` helped by trying to transpose those difficulties, posing as an "universal translator" (🖖).

After much thought and consideration, we believe that the `convert()` feature may be out of the `mctq` scope. It can maybe be part of another package (a `lubritime` package perhaps? 😄). Other `mctq` tools, like `shorter_interval()` and `sum_time()`, could also be a part of that package (but are really necessary in `mctq` for the time being). Hence, we decided to remove `convert()` and to instruct the user to check the [lubridate](https://lubridate.tidyverse.org/) and [hms](https://hms.tidyverse.org/) packages for parsing/conversion.

## Note about renaming `sd()`

That was a tough, but necessary, call. Although we tried to preserve the original authors naming pattern for the MCTQ functions, the name `sd` provokes a dangerous name collision with the widely used `stats::sd` (standard deviation) function. That's why we named it as `sdu`. This is the only exception, all the other `mctq` functions maintain a strong naming resemblance with the original authors naming pattern.


# mctq 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
