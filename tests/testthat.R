library(testthat)
library(checkmate)
library(mctq)
library(lubridate)
library(hms)

test_check("mctq")

# # For development use only
# # (don't forget to comment the code after use (Ctrl + Shift + C))
# library(checkmate)
# library(circular)
# library(crayon)
# library(glue)
# library(hms)
# library(lifecycle)
# library(lubridate)
# library(magrittr)
# library(rlang)
# library(tidyverse)
# library(tcltk)
# library(validate)
#
# library(devtools)
# load_all()
# document()

# Helpers:
# normalizePath(readClipboard(), "/", mustWork = FALSE)

# Plan vignettes
# * About the MCTQ and it's variants
# * Converting values
# * Common MCTQ data issues
# * Social jet lag signal
# * Time arithmetic and summary measures
