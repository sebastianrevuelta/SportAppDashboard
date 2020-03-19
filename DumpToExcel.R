# 1. Load libraries ----
library("stringr")
library("plyr")
library("lubridate")
library("tidyquant")
library("readr")
library("leaflet")
library("XML")
library("methods")
library("xlsx")
library("DT")
library("purrr")

setwd("C:/Users/srevuelta/OneDrive/Documentos/BusinessIntelligence/SportApp/")
dfTrainings <- read_rds("dfTrainings.rds")

dfTrainings <- dfTrainings %>%
    filter(Year > 2000) %>%
    mutate(Date = str_sub(Date,1,10))
xlsx::write.xlsx(dfTrainings,"dfTrainings.xlsx")