## Release summary

* This is a patch release in the 0.*.* series.
* All existing problems at
https://cran.r-project.org/web/checks/check_results_mctq.html were fixed.

## Test environments

* Local: Windows 10 install (v. 21H2), R-devel, R 4.2.2
* GitHub Actions: Windows (latest), __R 4.2.2__
* GitHub Actions: macOS (latest), __R 4.2.2__
* GitHub Actions: Ubuntu 18.04, __R-devel__, __R 4.2.2__, __R 4.2.1__
* R-hub: Windows Server 2022 (64 bit), R-devel
* R-hub: Fedora Linux (clang, gfortran), R-devel
* R-hub: Ubuntu Linux 20.04.1 LTS (GCC), R 4.2.2

## Local calls

```
devtools::check(remote = TRUE, manual = TRUE)
devtools::check_rhub()
goodpractice::gp()
revdepcheck::revdep_check(num_workers = 4)
```

## R CMD check results

There were no ERRORs or WARNINGs.

There were 3 NOTEs:

* Note 1

```
checking CRAN incoming feasibility ... [73s] NOTE
  Maintainer: 'Daniel Vartanian <danvartan@gmail.com>'
  
  Found the following (possibly) invalid URLs:
    URL: https://www.thewep.org/documentations/mctq
      From: README.md
      Status: Error
      Message: libcurl error code 35:
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
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
      Message: libcurl error code 35:
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
    URL: https://www.thewep.org/documentations/mctq/item/english-mctq-full
      From: inst/doc/mctq.html
            inst/doc/missing-sections.html
            inst/doc/sjl-computation.html
      Status: Error
      Message: libcurl error code 35:
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
    URL: https://www.thewep.org/documentations/mctq/item/mctq-variables
      From: inst/doc/missing-sections.html
            inst/doc/sjl-computation.html
      Status: Error
      Message: libcurl error code 35:
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
```

These could be due to a temporary failure in the web servers and can likely be ignored.

* Note 2

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

* Note 3

```
Possibly mis-spelled words in DESCRIPTION:
  ChronoType (2:36, 34:55)
  MCTQ (2:62, 35:20, 36:5)
  Merrow (38:30)
  Roenneberg (37:59)
  Wirz (38:5)
  chronotypes (36:57)
```

These are not mis-spelled words.

## revdepcheck results

There were no reverse depensdencies.

```
We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
```

---

Thanks!

Daniel Vartanian
