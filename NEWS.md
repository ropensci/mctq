# mctq 0.0.0.9001

## Breaking changes

* The `sd()` function is now named as `sdu()`. That was a tough, but necessary, call. Although we tried to preserve the original authors naming pattern for the MCTQ functions, the name `sd` provokes a dangerous name collision with the widely used `stats::sd` (standard deviation) function. That's why we named it as `sdu`. This is the only exception, all the other `mctq` functions maintain a strong naming resemblance with the original authors naming pattern.

## Added

* @jonkeane was added as a reviewer ('rev').
* @leocadio-miguel was added as a reviewer ('rev').
* The user interface has gotten more stylish, thanks to the [`cli` package](https://cli.r-lib.org) (now on imports).

## Changed

* The DESCRIPTION file now requires a lubridate version `>= 1.7.9`. `vctrs` support was added in this version, fixing some issues related to rounding `Duration`, `Period`, and `Interval` objects. Click [here](https://github.com/tidyverse/lubridate/pull/871) to learn more.
* The DESCRIPTION file now requires a dplyr version `>= 0.2`. That's when the `dplyr` start to import the pipe operator. Click [here](https://github.com/tidyverse/dplyr/blob/master/NEWS.md#piping) to learn more.
* `round_time()` is now a S3 generic.
* `shortest_interval()` was renamed to `shorter_interval()`.
* `shorter_interval()` will now return only `Interval` objects.
* Several functions were optimized.
* The test suite was optimized.

## Fixed

* Several typos were fixed in the documentation.


# mctq 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
