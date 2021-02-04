
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mctq <a href='https://gipsousp.github.io/mctq'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
<!-- To do: Add Code coverage (when possible) <https://docs.codecov.io/> -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/gipsousp/mctq/workflows/R-CMD-check/badge.svg)](https://github.com/gipsousp/mctq/actions)
[![Travis build
status](https://travis-ci.com/gipsousp/mctq.svg?branch=master)](https://travis-ci.com/gipsousp/mctq)
[![Codecov test
coverage](https://codecov.io/gh/gipsousp/mctq/branch/master/graph/badge.svg)](https://codecov.io/gh/gipsousp/mctq?branch=master)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://gipsousp.github.io/mctq/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`mctq` provides a consistent and fast way to process Munich Chronotype
Questionnaire (MCTQ) data in R for all of its three versions (standard,
micro, and shift).

Please note that this package is currently on the development stage and
have not been [peer
reviewed](https://devguide.ropensci.org/softwarereviewintro.html) yet.
That means that people can try it out and provide feedback, but it comes
with no promises for long term stability.

## About MCTQ

### Wait, a R package for a questionnaire?

If you’re familiar with MCTQ, you know that it is not a simple
questionnaire, as it requires a lot of date/time manipulation. That can
be real challenging, especially if you’re dealing with a large set of
data.

The main advantage to use the `mctq` package in your research is that
you will have reliable tools, thoroughly tested, at your disposition,
made and supported by a sleep science research group (GIPSO) from a well
know university ([USP](https://www5.usp.br/)). `mctq` also helps with
research reproducibility, since it’s a free and open source package that
anyone can find and use.

This package is also equip with several utility functions that allows
you to easily convert and visualize your MCTQ data. It also provides
fictional datasets for testing and learning purposes.

## Installation

The first stable `mctq` version is in its final development stage, we
hope that it will be available on [CRAN](https://cran.r-project.org/)
soon. Until that moment comes, you can install the development version
from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("gipsousp/mctq")
```

## Usage

`mctq` works with a set of object classes specially created to hold time
values. This classes can be found in the
[hms](https://hms.tidyverse.org/) and
[lubridate](https://lubridate.tidyverse.org/) package from
[tidyverse](https://www.tidyverse.org/packages/). If your data do not
conform to the object classes required, don’t worry, just use
`convert()` to convert it. You can always convert it back if you want.

``` r
library(mctq)
library(lubridate)
library(hms)

# __ Conversion from units to date/time objects __
## From decimal hours to `hms`
convert(6.5, "hms", input_unit = "H")
#> 06:30:00
## From radians to `Duration`
convert(1.308997, "Duration", input_unit = "rad")
#> [1] "18000s (~5 hours)"
## From degrees to `Duration`
convert(15, "Duration", input_unit = "deg")
#> [1] "3600s (~1 hours)"

# __ Conversion from `character` or `numeric` objects to date/time objects __
## From `character` `HMS` to `Duration`
convert("19:55:17", "Duration", orders = "HMS")
#> [1] "71717s (~19.92 hours)"
## From `character` `HM AM/PM ` to `hms`
convert("10:00 PM", "hms", orders = "IMp")
#> 22:00:00
## From `numeric` decimal minutes (M) to `Period`
convert(20.5, "Period", orders = "M")
#> [1] "20M 30S"

# __ Conversion between date/time objects __
## From `Duration` to `hms`
convert(lubridate::dseconds(120), "hms")
#> 00:02:00
## From `Date` to `POSIXct`
convert(lubridate::as_date("1765-10-05"), "POSIXct")
#> [1] "1765-10-05 UTC"
## From `POSIXct` to `Period`
convert(lubridate::as_datetime("2020-01-01 10:00:00"), "Period")
#> [1] "10H 0M 0S"

# __ Conversion of columns in a data frame __
## Converting a `hms` column to radians
data <- data.frame(bt_w = std_mctq$bt_w, bt_w_rad = std_mctq$bt_w)
data <- convert(data, "numeric", cols = "bt_w_rad", output_unit = "rad")
head(data)
#>       bt_w  bt_w_rad
#> 1       NA        NA
#> 2 00:30:00 0.1308997
#> 3 23:15:00 6.0868358
#> 4 23:00:00 6.0213859
#> 5 23:35:00 6.1741022
#> 6 02:00:00 0.5235988
```

After you data is all set, just use the `mctq` functions below to
process it.

Note that `mctq` uses a similar naming scheme as those used in the MCTQ
articles, that make it easy to find and apply any computation necessary.

For basic MCTQ computation, use:

-   `fd()` compute MCTQ work-free days
-   `so()` compute MCTQ sleep onset
-   `gu()` compute MCTQ local time of getting out of bed
-   `sd()` compute MCTQ sleep duration
-   `napd()` compute MCTQ nap duration (only for MCTQ Shift)
-   `tbt()` compute MCTQ total time in bed
-   `ms()` compute MCTQ mid-sleep
-   `sd24()` compute MCTQ 24h sleep duration (only for MCTQ Shift)

``` r
library(mctq)
library(lubridate)
library(hms)

# Local time of preparing to sleep on workdays.
sprep_w <- c(hms::parse_hms("23:45:00"), hms::parse_hms("02:15:00"))
# Sleep latency on workdays
slat_w <- c(lubridate::dminutes(30), lubridate::dminutes(90))
# Sleep onset
so(sprep_w, slat_w)
#> 00:15:00
#> 03:45:00
```

For computations combining workdays and work-free days, use:

-   `sd_week()` compute MCTQ average weekly sleep duration
-   `sd_overall()` compute MCTQ overall sleep duration (only for MCTQ
    Shift)
-   `sjl()` and `sjl_rel()` compute MCTQ social jet lag
-   `sjl_weighted()` compute MCTQ absolute social jetlag across all
    shifts (only for MCTQ Shift)
-   `sloss_week()` compute MCTQ weekly sleep loss
-   `msf_sc()` compute MCTQ chronotype or corrected mid-sleep on
    work-free days
-   `le_week()` compute MCTQ average weekly light exposure

``` r
library(mctq)
library(lubridate)
library(hms)

# Mid-sleep on workdays
msw <- c(hms::parse_hms("02:05:00"), hms::parse_hms("04:05:00"))
# Mid-sleep on work-free days
msf <- c(hms::parse_hms("23:05:00"), hms::parse_hms("08:30:00"))
# Relative social jetlag
sjl_rel(msw, msf)
#> [1] "-10800s (~-3 hours)"  "15900s (~4.42 hours)"
```

In addition to `convert()`, `mctq` is also equip with many other
utilities functions.

The functions are well documented, showing all the guidelines behind the
computations. Click
[here](https://gipsousp.github.io/mctq/reference/index.html) to view a
list of them.

## Citation

If you use `mctq` in your research, please consider citing it. We put a
lot of work to build and maintain a free and open source R package. You
can find `mctq` citation
[here](https://gipsousp.github.io/mctq/authors.html).

## Contributing

`mctq` is a community project, anyone and everyone is welcome to
contribute. Take a moment to review our [Guidelines for
Contributing](https://gipsousp.github.io/mctq/CONTRIBUTING.html).

Please note that `mctq` is released with a [Contributor Code of
Conduct](https://gipsousp.github.io/mctq/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
