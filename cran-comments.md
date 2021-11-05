## Resubmission

This is a resubmission. Please see the notes about it below 
(based on the last submission response).

> Thanks,
> 
> Please omit the redundant "An R Package for the" from the title of your
> package

Response: Done.

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> > authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking.
> (If you want to add a title as well please put it in quotes: "Title")

Response: I plan to write an article about the package, but, at the moment, 
          there isn't one. I added the reference for the original MCTQ article 
          in the description field. The vignette 'mctq' gives a quick tour 
          of `mctq` main functions.

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)
> Missing Rd-tags:
>       qplot_walk.Rd: \value

Response: Done.

> It is sufficient to wrap interactive examples only in if (interactive())
> {}. Please remove the additional \dontrun{}.
> e.g. qplot_walk.Rd

Response: Done.

> Please fix and resubmit.
> 
> Best,
> Julia Haider

Response: Thank you! :)

## Test environments

* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD check results

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Daniel Vartanian <danvartan@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    ChronoType (2:36, 34:70)
    MCTQ (2:62, 35:20, 36:5)
    Merrow (38:30)
    Roenneberg (37:59)
    Wirz (38:5)
    chronotypes (36:57)
  New submission

0 errors √ | 0 warnings √ | 1 note x

## Notes

* 'ChronoType', 'MCTQ', 'Merrow' (refers to Martha Merrow), 'Roenneberg' (refers
to Till Roenneberg), 'Wirz' (refers to Anna Wirz-Justice), and 'chronotypes' are
not misspelled words.
