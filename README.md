
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mctq <a href='https://gipsousp.github.io/mctq'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Buy Me A Coffee donate
button](https://img.shields.io/badge/buy%20me%20a%20coffee-donate-yellow.svg)](https://www.buymeacoffee.com/danielvartan)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/434_status.svg)](https://github.com/ropensci/software-review/issues/434)
[![CRAN
status](https://www.r-pkg.org/badges/version/mctq)](https://CRAN.R-project.org/package=mctq)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/gipsousp/mctq/workflows/R-CMD-check/badge.svg)](https://github.com/gipsousp/mctq/actions)
[![codecov](https://codecov.io/gh/gipsousp/mctq/branch/main/graph/badge.svg?token=i5nG9mtxP8)](https://codecov.io/gh/gipsousp/mctq)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://gipsousp.github.io/mctq/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`mctq` is an R package that provides a complete and consistent toolkit
to process the Munich ChronoType Questionnaire (MCTQ), a quantitative
and validated method to assess peoples’ sleep behavior presented by Till
Roenneberg, Anna Wirz-Justice, and Martha Merrow in
[2003](https://doi.org/10.1177/0748730402239679). The aim of `mctq` is
to facilitate the work of sleep and chronobiology scientists with MCTQ
data while also helping with research reproducibility.

> This package is currently under a [software
> peer-review](https://github.com/ropensci/software-review/issues/434)
> by the [rOpenSci](https://ropensci.org/) initiative. We plan to submit
> it to [CRAN](https://cran.r-project.org/) soon after the review
> process ends.

Learn more about the MCTQ questionnaire at
<https://www.thewep.org/documentations/mctq>.

### Wait, an R package for a questionnaire?

Although it may look like a simple questionnaire, MCTQ requires a lot of
date/time manipulation. This poses a challenge for many scientists,
being that most people have difficulties with date/time data, especially
when dealing with an extensive set of data. The `mctq` package comes to
address this issue.

`mctq` can handle the processing tasks for the three MCTQ versions
(standard, micro, and shift) with few dependencies, relying much of its
applications on the [lubridate](https://lubridate.tidyverse.org/) and
[hms](https://hms.tidyverse.org/) packages from
[tidyverse](https://www.tidyverse.org/). We also designed `mctq` with
the user experience in mind, by creating an interface that resembles the
way the questionnaire data is shown in MCTQ publications, and by
providing extensive and detailed documentation about each computation
proposed by the MCTQ authors. The package also includes several utility
tools to deal with different time representations (e.g., decimal hours,
radians) and time arithmetic issues, along with fictional datasets for
testing and learning purposes.

## Prerequisites

You only need to have some familiarity with the [R programming
language](https://www.r-project.org/) to use the `mctq` main functions.

In case you don’t feel comfortable with R, we strongly recommend
checking Hadley Wickham and Garrett Grolemund free and online book [R
for Data Science](https://r4ds.had.co.nz/) and the Coursera course from
John Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

## Installation

The first stable `mctq` version is already out, we’re just waiting for
the [software
peer-review](https://github.com/ropensci/software-review/issues/434) to
promote it to the public. We hope that it will be available on
[CRAN](https://cran.r-project.org/) soon. Until that moment comes, you
can install it from GitHub with:

``` r
# install.packages("pak")
pak::pkg_install("gipsousp/mctq")
```

## Usage

`mctq` works with a set of object classes specially created to hold time
values. These classes can be found in the
[lubridate](https://lubridate.tidyverse.org/) and
[hms](https://hms.tidyverse.org/) packages from
[tidyverse](https://www.tidyverse.org/packages/). If your data do not
conform to the object classes required, you can use `mctq` `convert()`
function to convert it.

Here are some examples of how to convert your data using `convert()`:

``` r
# From decimal hours to `hms`
convert(6.5, "hms", input_unit = "H")
#> 06:30:00
# From radians to `Duration`
convert(1.308997, "Duration", input_unit = "rad")
#> [1] "18000s (~5 hours)"
# From radians to decimal minutes
convert(0.2617994, "numeric", input_unit = "rad", output_unit = "M")
#> [1] 60
# From `character` `HMS` to `Duration`
convert("19:55:17", "Duration", orders = "HMS")
#> [1] "71717s (~19.92 hours)"
# From `character` `HM AM/PM ` to `hms`
convert("10:00 PM", "hms", orders = "IMp")
#> 22:00:00
```

### Workdays and work-free days variables

After your data is set to start, just use the `mctq` functions below to
process it.

Note that the `mctq` functions uses a similar naming scheme to that used
in the MCTQ publications. That makes it easy to find and apply any
computation necessary.

-   `fd()`: compute MCTQ work-free days
-   `so()`: compute MCTQ local time of sleep onset
-   `gu()`: compute MCTQ local time of getting out of bed
-   `sd()`: compute MCTQ sleep duration
-   `tbt()`: compute MCTQ total time in bed
-   `ms()`: compute MCTQ local time of mid-sleep
-   `napd()`: compute MCTQ nap duration (only for MCTQ Shift)
-   `sd24()`: compute MCTQ 24 hours sleep duration (only for MCTQ Shift)

Example:

``` r
# Local time of preparing to sleep on workdays
sprep_w <- c(hms::parse_hms("23:45:00"), hms::parse_hms("02:15:00"))
# Sleep latency or time to fall asleep after preparing to sleep on workdays
slat_w <- c(lubridate::dminutes(30), lubridate::dminutes(90))
# Local time of sleep onset on workdays
so(sprep_w, slat_w)
#> 00:15:00
#> 03:45:00
```

### Combining workdays and work-free days variables

For computations combining workdays and work-free days, use:

-   `sd_week()`: compute MCTQ average weekly sleep duration
-   `sd_overall()`: compute MCTQ overall sleep duration (only for MCTQ
    Shift)
-   `sloss_week()`: compute MCTQ weekly sleep loss
-   `le_week()`: compute MCTQ average weekly light exposure
-   `msf_sc()`: compute MCTQ chronotype or corrected local time of
    mid-sleep on work-free days
-   `sjl_rel()` and `sjl()`: compute MCTQ social jet lag
-   `sjl_weighted()`: compute MCTQ absolute social jetlag across all
    shifts (only for MCTQ Shift)

Example:

``` r
# Local time of mid-sleep on workdays
msw <- c(hms::parse_hms("02:05:00"), hms::parse_hms("04:05:00"))
# Local time of mid-sleep on work-free days
msf <- c(hms::parse_hms("23:05:00"), hms::parse_hms("08:30:00"))
# Relative social jetlag
sjl_rel(msw, msf)
#> [1] "-10800s (~-3 hours)"  "15900s (~4.42 hours)"
```

See a quick tour of all MCTQ main functions
[here](https://gipsousp.github.io/mctq/articles/mctq.html).

### Utilities

In addition to `convert()`, `mctq` is also equipped with many other
utility functions. The package also provides fictional datasets of the
standard, micro, and shift MCTQ versions for testing and learning
purposes.

All functions are well documented, showing all the guidelines behind the
computations. Click
[here](https://gipsousp.github.io/mctq/reference/index.html) to see a
list of them.

## Citation

If you use `mctq` in your research, please consider citing it. We put a
lot of work to build and maintain a free and open-source R package. You
can find the `mctq` citation below.

``` r
citation("mctq")
#> 
#> To cite mctq in publications use:
#> 
#>   Vartanian, D., Benedito-Silva, A. A., Pedrazzoli, M. (2021). mctq: An
#>   R package for the Munich ChronoType Questionnaire. Retrieved from
#>   https://gipsousp.github.io/mctq/.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {mctq: An R package for the Munich ChronoType Questionnaire},
#>     author = {Daniel Vartanian and Ana Amelia Benedito-Silva and Mario Pedrazzoli},
#>     year = {2021},
#>     url = {https://gipsousp.github.io/mctq/},
#>     note = {Lifecycle: maturing},
#>   }
```

## Contributing

`mctq` is a community project, everyone is welcome to contribute. Take a
moment to review our [Guidelines for
Contributing](https://gipsousp.github.io/mctq/CONTRIBUTING.html).

Please note that `mctq` is released with a [Contributor Code of
Conduct](https://gipsousp.github.io/mctq/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Support `mctq`

[![“Buy Me A
Coffee”](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/danielvartan)

Working with science in Brazil is a daily challenge. There are few
funding opportunities available and their value is not enough to live
on. Added to this, every day Brazilian science suffers from deep cuts in
funding, which requires researchers to always look for other sources of
income.

If this package helps you in any way or you simply want to support the
author work, please consider donating or even creating a membership
subscription (if you can!). Your support will help with the author
scientific pursuit and with the package maintenance.

To make a donation, click on the buymeacoffee.com button above. Please
indicate the `mctq` package in your donation message.

Thank you!
