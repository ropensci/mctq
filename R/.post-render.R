# library(beepr)
# library(cffr)
# library(codemetar)
# library(groomr) # https://github.com/danielvartan/groomr
# library(here)
# library(readr)
# library(rutils) # https://github.com/danielvartan/rutils
# library(stringr)

# Remove empty lines from `README.md` -----

groomr::remove_blank_line_dups(here::here("README.md"))

# Update package versions in `DESCRIPTION` -----

rutils::update_pkg_versions()

# Update package year -----

files <- c(
  here::here("LICENSE"),
  here::here("LICENSE.md"),
  here::here("inst", "CITATION")
)

for (i in files) {
  data <-
    i |>
    readr::read_lines() |>
    stringr::str_replace_all(
      pattern = "20\\d{2}",
      replacement = as.character(Sys.Date() |> lubridate::year())
    )

  data |> readr::write_lines(i)
}

# Update `cffr` and `codemeta` -----

cffr::cff_write()
codemetar::write_codemeta()

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
