fd <- function(wd) {

    assert_number(wd, lower = 0, upper = 7)

    as.integer(7 - wd)

}

# mctq_core %>% dplyr::select(wd) %>% dplyr::mutate(fd = fd(wd))

so <- function(s_prep, s_lat){

    s_prep <- convert_to_date_time(s_prep, "POSIXct")
    s_lat <- convert_to_date_time(s_lat, "Duration")

    hms::as_hms(s_prep + s_lat)

}

# mctq_core %>%
#     dplyr::select(s_prep_f, s_lat_f) %>%
#     dplyr::mutate(so_f = so(s_prep_f, s_lat_f))

gu <- function(se, si){

    se <- convert_to_date_time(se, "POSIXct")
    si <- convert_to_date_time(si, "Duration")

    hms::as_hms(se + si)

}

# mctq_core %>%
#     dplyr::select(se_f, si_f) %>%
#     dplyr::mutate(gu_f = gu(se_f, si_f))

sd <- function(se, so){

    se <- convert_to_date_time(se, "POSIXct")
    so <- convert_to_date_time(so, "POSIXct")

    lubridate::as.duration(se - so)

}

tbt <- function(gu, bt){

    # Add add_time_origin

    gu <- convert_to_date_time(gu, "POSIXct")
    bt <- convert_to_date_time(bt, "POSIXct")

    hms::as_hms(gu - bt)

}

ms <- function(so, sd){

    so <- convert_to_date_time(so, "POSIXct")
    sd <- convert_to_date_time(sd, "Duration")

    hms:as_hms(so + (sd / 2))

}

sd_week <- function(wd, sd_w, sd_f){

    assert_number(wd, lower = 0, upper = 7)

    sd_w <- convert_to_date_time(sd_w, "Duration")
    sd_f <- convert_to_date_time(sd_f, "Duration")

    hms::as_hms(((sd_w * wd) + (sd_f * fd(wd))) / 7)

}

msf_sc <- function(msf, sd_w, sd_f, sd_week){

    msf <- convert_to_date_time(msf, "POSIXct")
    sd_w <- convert_to_date_time(sd_w, "Duration")
    sd_f <- convert_to_date_time(sd_f, "Duration")
    sd_week <- convert_to_date_time(sd_week, "Duration")

    if (sd_f <= sd_w){
        # msf <- msf
    } else {
        msf <- msf - ((sd_f - sd_week) / 2)
    }

    hms::as_hms(msf)

}

chronotype <- function(msf, sd_w, sd_f, sd_week) {

    msf_sc(msf, sd_f, sd_w, sd_week)

}

sloss_week <- function(wd, sd_w, sd_f, sd_week){

    assert_number(wd, lower = 0, upper = 7)

    sd_w <- convert_to_date_time(sd_w, "Duration")
    sd_f <- convert_to_date_time(sd_f, "Duration")
    sd_week <- convert_to_date_time(sd_week, "Duration")

    if (sd_week > sd_w){
        (sd_week - sd_w) * wd
    } else { # sd_week <= sd_w
        (sd_week - sd_f) * fd(wd)
    }

}

sjl_rel <- function(msw, msf) {

    lubridate::as.duration(msf - msw)

}

sjl <- function(msw, msf){

    abs(sjl_rel(msw, msf))

}

le_week <- function(wd, le_w, le_f){

    if (any(stringr::str_detect(wd, "^\\d+$"))) {
        wd <- as.integer(wd)
    }

    assert_integerish(wd, lower = 0, upper = 7)

    le_w <- convert_to_date_time(le_w, "Duration")
    le_f <- convert_to_date_time(le_f, "Duration")

    if (le_w > lubridate::dhours(24) |
        le_f > lubridate::dhours(24))

    ((le_w * wd) +  (le_f * fd(wd))) / 7

}


# Adicionar verificador e conversor de valor (caractere, posixct, hms)

# Adicionar conversor de resultado: hms, posixct, horas decimais, radianos,
# fração de dia

# Adicionar conversor de valores de duração


mctq <- readRDS("C:/Midia/GitHub/genetica_do_sono/data/valid/mctq/mctq.RData")

library(tidyverse)
library(anytime)
library(hms)
library(testthat)

test_that("MSW is valid", {
    expect_equal(sjl(as.POSIXct("2020-01-01 02:00:00"),
                     as.POSIXct("2020-01-01 04:00:00")),
                 lubridate::as.duration("3 hours"))
})
