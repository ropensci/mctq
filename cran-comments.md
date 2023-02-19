## Release summary

* This is a patch release in the 0.*.* series.
* As of this version, a decoupling process related to some `mctq` utility
functions will start. A deprecation warning was added to several functions.

## Test environments

* Local: Windows 11 install (v. 22H2), R-release (4.2.2)
* GitHub Actions: Windows (latest), R-release (4.2.2)
* GitHub Actions: macOS (latest), R-release (4.2.2)
* GitHub Actions: Ubuntu (latest), R-devel, R-release (4.2.2), R-oldrel (4.1.3)
* R-hub: Windows Server 2022 (64 bit), R-devel, R-release (4.2.2), 
         R-oldrel (4.1.3)
* R-hub: macOS 10.13.6 High Sierra, R-release (4.2.2)
* R-hub: Ubuntu Linux 20.04.1 LTS (GCC), R-devel, R-release (4.2.2)
* r-project.org: Win builder, R-devel, R-release (4.2.2), R-oldrel (4.1.3) 
* r-project.org: macOS builder, R-release (4.2.2)

## Local calls

```
devtools::check(remote = TRUE, manual = TRUE)
devtools::check_mac_release()
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
devtools::check_rhub(platforms = c(
    "windows-x86_64-devel", "windows-x86_64-release",
    "windows-x86_64-oldrel", "macos-highsierra-release-cran", 
    "ubuntu-gcc-devel", "ubuntu-gcc-release"
))
revdepcheck::revdep_check(num_workers = 4)
goodpractice::gp()
```

## R CMD check results

There were no ERRORs or WARNINGs.

There were 1 NOTEs:

* Note 1

```
Possibly mis-spelled words in DESCRIPTION:
  ChronoType (2:36, 46:55)
  MCTQ (2:62, 47:20, 48:5)
  Merrow (50:30)
  Roenneberg (49:59)
  Wirz (50:5)
  chronotypes (48:57)
```

These are not misspelled words.

* Note 2

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this 
could be due to a bug/crash in MiKTeX and can likely be ignored.

* Note 3

```
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1177/0748730402239679
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://www.thewep.org/documentations/mctq
    From: README.md
    Status: Error
    Message: schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
  URL: https://www.thewep.org/documentations/mctq/
    From: man/fd.Rd
          man/gu.Rd
          man/le_week.Rd
          man/micro_mctq.Rd
          man/msf_sc.Rd
          man/msl.Rd
          man/napd.Rd
          man/sd24.Rd
          man/sd_overall.Rd
          man/sd_week.Rd
          man/sdu.Rd
          man/shift_mctq.Rd
          man/sjl.Rd
          man/sjl_sc.Rd
          man/sjl_weighted.Rd
          man/sloss_week.Rd
          man/so.Rd
          man/std_mctq.Rd
          man/tbt.Rd
    Status: Error
    Message: schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
  URL: https://www.thewep.org/documentations/mctq/item/english-mctq-full
    From: inst/doc/mctq.html
          inst/doc/missing-sections.html
          inst/doc/sjl-computation.html
    Status: Error
    Message: schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
  URL: https://www.thewep.org/documentations/mctq/item/mctq-variables
    From: inst/doc/missing-sections.html
          inst/doc/sjl-computation.html
    Status: Error
    Message: schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.

Found the following (possibly) invalid DOIs:
  DOI: 10.1177/0748730402239679
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
```

These could be due to a temporary failure in the web servers and can likely be ignored. I checked all the links and DOIs and they are working normally.

## revdepcheck results (Windows 11 install (v. 22H2))

There were no reverse dependencies.

```
We checked 0 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
```

## goodpractice results (Windows 11 install (v. 22H2))

```
♥ Ole! Awesome package! Keep up the super work!
```

---

Thanks!

Daniel Vartanian
