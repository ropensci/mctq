fd <- function(wd) {

    as.integer(7 - wd)

}

# mctq_core %>% dplyr::select(wd) %>% dplyr::mutate(fd = fd(wd))

so <- function(s_prep, s_lat){

    output <- convert_to_posixct(s_prep + s_lat)
    hms::as_hms(output)

}

# mctq_core %>%
#     dplyr::select(s_prep_f, s_lat_f) %>%
#     dplyr::mutate(so_f = so(s_prep_f, s_lat_f))

gu <- function(se, si){

    output <- convert_to_posixct(se + si)
    hms::as_hms(output)

}

mctq_core %>%
    dplyr::select(se_f, si_f) %>%
    dplyr::mutate(gu_f = gu(se_f, si_f))

sd <- function(se, so){

    output <- convert_to_posixct(se + si)
    lubridate::as.duration(se - so)

}

tbt <- function(gu, bt){

    gu - bt

}

ms <- function(so, sd){

    so + (sd / 2)

}

sd_week <- function(wd, sd_w, sd_f){

    ((sd_w * wd) + (sd_f * fd(wd))) / 7

}

msf_sc <- function(msf, sd_w, sd_f, sd_week){


    if (sd_f <= sd_w){
        output <- msf
    } else {
        output <- msf - ((sd_f - sd_week) / 2)
    }

    output

}

chronotype <- function(msf, sd_w, sd_f, sd_week) {

    msf_sc(msf, sd_f, sd_w, sd_week)

}

sloss_week <- function(wd, sd_w, sd_f, sd_week){


    if (sd_week > sd_w){
        output <- (sd_week - sd_w) * wd
    } else {
        output <- (sd_week - sd_f) * fd(wd)
    }

    output

}

sjl_rel <- function(msw, msf) {

    lubridate::as.duration(msf - msw)

}

sjl <- function(msw, msf){

    abs(sjl_rel(msw, msf))

}

le_week <- function(wd, le_w, le_f){

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
