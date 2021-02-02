
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mctq <a href='https://gipsousp.github.io/mctq'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
<!-- To do: Add Code coverage (when possible) <https://docs.codecov.io/> -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/gipsousp/mctq/workflows/R-CMD-check/badge.svg)](https://github.com/gipsousp/mctq/actions)
[![Codecov test
coverage](https://codecov.io/gh/gipsousp/mctq/branch/master/graph/badge.svg)](https://codecov.io/gh/gipsousp/mctq?branch=master)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://gipsousp.github.io/mctq/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`mctq` provides a consistent and fast way to process Munich Chronotype
Questionnaire (MCTQ) data in R. All of the three MCTQ versions
(standard, micro, and shift) are supported.

`mctq` is currently on the development stage and had not been [peer
reviewed](https://devguide.ropensci.org/softwarereviewintro.html) yet.
That means that people can try it out and provide feedback, but the
package comes with no promises for long term stability.

## Wait, a R package for a questionnaire?

If you’re familiar with MCTQ, you know that it is not a simple
questionnaire, as it requires a lot of date/time manipulation. That can
be real challenging, especially if you’re dealing with a large set of
data.

One main advantage to use the `mctq` package in your research is that
you will have reliable tools, thoroughly tested, at your disposition,
made and supported by a sleep science research group (GIPSO) from a well
know university ([USP](https://www5.usp.br/)). `mctq` can also help you
with research reproducibility, as it’s a free and open source package
that anyone can find and use.

The package is also equip with several utility functions that allows you
to easily convert and visualize your MCTQ data. It also provides
fictional datasets for testing and learning purposes.

## Installation

The first stable `mctq` version is on the final development stage, we
hope that it will be available on [CRAN](https://cran.r-project.org/)
soon. Until that moment comes, you can install the development version
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gipsousp/mctq")
```

## Usage

`mctq` uses similar naming abbreviations, as is used on the MCTQ
articles.

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

# local time of preparing to sleep on workdays.
sprep_w <- c(hms::parse_hms("22:00:00"), hms::parse_hms("01:15:00"))
# sleep latency on workdays
slat_w <- c(lubridate::dminutes(15), lubridate::dminutes(90))
# sleep onset
so(sprep_w, slat_w)
#> 22:15:00
#> 02:45:00
```

For computation combining workdays and work-free days or different
shifts, use:

-   `sd_week()` compute MCTQ average weekly sleep duration
-   `sd_overall()` compute MCTQ overall sleep duration (only for MCTQ
    Shift)
-   `sjl() sjl_rel()` compute MCTQ social jet lag
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

# mid-sleep on workdays
msw <- c(hms::parse_hms("02:05:00"), hms::parse_hms("04:05:00"))
# mid-sleep on work-free days
msf <- c(hms::parse_hms("23:05:00"), hms::parse_hms("08:30:00"))
# relative social jetlag
sjl_rel(msw, msf)
#> [1] "-10800s (~-3 hours)"  "15900s (~4.42 hours)"
```

`mctq` is also equip with many utilities function, like `convert()`.

``` r
library(mctq)
library(lubridate)
library(hms)

## Convert from character or numeric objects to date/time objects
convert("19:55:17", "Duration", orders = "HMS")
#> [1] "71717s (~19.92 hours)"
convert("0145", "numeric", orders = "HM", output_unit = "M")
#> [1] 105
convert(1.308997, "numeric", input_unit = "rad", output_unit = "H")
#> [1] 5
```

## Citation

We put a lot a work to build and maintain a free and open source R
package. If you use `mctq` on your research, please consider citing it.
You can find `mctq` citation
[here](https://gipsousp.github.io/mctq/authors.html).

## Contributing

`mctq` is a community project, anyone and everyone is welcome to
contribute. Take a moment to review the [Guidelines for
Contributing](https://gipsousp.github.io/mctq/CONTRIBUTING.html).

Please note that the `mctq` project is released with a [Contributor Code
of Conduct](https://gipsousp.github.io/mctq/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
