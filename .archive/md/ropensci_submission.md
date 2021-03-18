Submitting Author: Daniel Vartanian (@danielvartan)
Other Authors: Ana Amelia Benedito-Silva (@AnaAmeliaBeneditoSIlva) and Mario Pedrazzoli (@pedrazzo)
Repository: https://github.com/gipsousp/mctq
Version submitted: 0.0.0.9000
Editor: <!--editor--> TBD <!--end-editor-->
Reviewers: <!--reviewers-list--> TBD <!--end-reviewers-list-->
<!--due-dates-list--><!--end-due-dates-list-->
Archive: TBD
Version accepted: TBD

---



-   Paste the full DESCRIPTION file inside a code block below:

```
Package: mctq
Title: The R Package for the Munich ChronoType Questionnaire (MCTQ)
Version: 0.0.0.9000
Authors@R: 
    c(person(given = "Daniel",
             family = "Vartanian",
             role = c("aut", "cre", "cph"),
             email = "danvartan@gmail.com",
             comment = c(ORCID = "0000-0001-7782-759X")),
      person(given = "Ana Amelia",
             family = "Benedito-Silva",
             email = "aamelia@usp.br",
             role = c("aut", "sad"),
             comment = c(ORCID = "0000-0003-4976-2623")),
      person(given = "Mario",
             family = "Pedrazzoli",
             email = "pedrazzo@usp.br",
             role = c("aut", "sad"),
             comment = c(ORCID = "0000-0002-5257-591X")),
      person(given = "Interdisciplinary Sleep Research Group (GIPSO)",
             role = c("fnd", "cph")),
      person(given = "University of Sao Paulo (USP)",
             role = c("fnd")))
Maintainer: Daniel Vartanian <danvartan@gmail.com>
Description: A complete and consistent toolkit to process Munich ChronoType 
    Questionnaire (MCTQ) data in R for all of its three versions (standard, 
    micro, and shift).
License: MIT + file LICENSE
URL: https://gipsousp.github.io/mctq/, https://github.com/gipsousp/mctq/
BugReports: https://github.com/gipsousp/mctq/issues/
Encoding: UTF-8
LazyData: true
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.1.1
Depends:
    R (>= 3.6)
Imports: 
    checkmate,
    dplyr,
    hms,
    lifecycle,
    lubridate
Suggests:
    covr,
    crayon,
    datasets,
    ggplot2,
    grDevices,
    knitr,
    magrittr,
    mockr,
    readr,
    rmarkdown,
    spelling,
    stats,
    testthat (>= 2.0.0),
    utils
Config/testthat/edition: 2
VignetteBuilder: knitr
Language: en-US
```


## Scope

- Please indicate which category or categories from our [package fit policies](https://ropensci.github.io/dev_guide/policies.html#package-categories) this package falls under: (Please check an appropriate box below. If you are unsure, we suggest you make a pre-submission inquiry.):

	- [ ] data retrieval
	- [ ] data extraction
	- [x] data munging
	- [ ] data deposition
	- [ ] workflow automation
	- [ ] version control
	- [ ] citation management and bibliometrics
	- [ ] scientific software wrappers
	- [ ] field and lab reproducibility tools
	- [ ] database software bindings
	- [ ] geospatial data
	- [ ] text analysis

- Explain how and why the package falls under these categories (briefly, 1-2 sentences):

The Munich Chronotype Questionnaire (MCTQ) may look like a simple questionnaire, but it requires a lot of date/time manipulation. This can be really challenging, especially if you’re dealing with a large set of data. `mctq` provides reliable tools, thoroughly tested, to help scientists with that task.

-   Who is the target audience and what are scientific applications of this package?

`mctq` was designed for researchers in the field of sleep and chronobiology. The scientific applications of the package are the same as the scale it addresses, _i.e._ to assess peoples' sleep behavior and characteristics of circadian rhythms.

-   Are there other R packages that accomplish the same thing? If so, how does yours differ or meet [our criteria for best-in-category](https://ropensci.github.io/dev_guide/policies.html#overlap)?

No other packages do the same thing (to my knowledge).

-   (If applicable) Does your package comply with our [guidance around _Ethics, Data Privacy and Human Subjects Research_](https://devguide.ropensci.org/policies.html#ethics-data-privacy-and-human-subjects-research)?

I don't think this is applicable to our package.

-   If you made a pre-submission enquiry, please paste the link to the corresponding issue, forum post, or other discussion, or @tag the editor you contacted.

N/A

## Technical checks

Confirm each of the following by checking the box.

- [x] I have read the [guide for authors](https://devguide.ropensci.org/guide-for-authors.html) and [rOpenSci packaging guide](https://devguide.ropensci.org/building.html).

This package:

- [x] does not violate the Terms of Service of any service it interacts with.
- [x] has a CRAN and OSI accepted license.
- [x] contains a [README with instructions for installing the development version](https://ropensci.github.io/dev_guide/building.html#readme).
- [x] includes [documentation with examples for all functions, created with roxygen2](https://ropensci.github.io/dev_guide/building.html#documentation).
- [x] contains a vignette with examples of its essential functions and uses.
- [x] has a [test suite](https://ropensci.github.io/dev_guide/building.html#testing).
- [x] has [continuous integration](https://ropensci.github.io/dev_guide/ci.html), including reporting of test coverage using services such as Travis CI, Coveralls and/or CodeCov.

## Publication options

- [x] Do you intend for this package to go on CRAN?
- [ ] Do you intend for this package to go on Bioconductor?

- [ ] Do you wish to submit an Applications Article about your package to [Methods in Ecology and Evolution](http://besjournals.onlinelibrary.wiley.com/hub/journal/10.1111/(ISSN)2041-210X/)? If so:

<details>
<summary>MEE Options</summary>

- [ ] The package is novel and will be of interest to the broad readership of the journal.
- [ ] The manuscript describing the package is no longer than 3000 words.
- [ ] You intend to archive the code for the package in a long-term repository which meets the requirements of the journal (see [MEE's Policy on Publishing Code](http://besjournals.onlinelibrary.wiley.com/hub/journal/10.1111/(ISSN)2041-210X/journal-resources/policy-on-publishing-code.html))
- (*Scope: Do consider MEE's [Aims and Scope](http://besjournals.onlinelibrary.wiley.com/hub/journal/10.1111/(ISSN)2041-210X/aims-and-scope/read-full-aims-and-scope.html) for your manuscript. We make no guarantee that your manuscript will be within MEE scope.*)
- (*Although not required, we strongly recommend having a full manuscript prepared when you submit here.*)
- (*Please do not submit your package separately to Methods in Ecology and Evolution*)

</details>

## Code of conduct

- [x] I agree to abide by [rOpenSci's Code of Conduct](https://ropensci.github.io/dev_guide/policies.html#code-of-conduct) during the review process and in maintaining my package should it be accepted.
