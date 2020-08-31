# 1. Load libraries ----
library("shinydashboard")
library("shinyjs")
library("stringr")
library("plyr")
library("lubridate")
library("tidyquant")
library("readr")
library("shinyalert")
library("leaflet")
library("XML")
library("methods")
library("xlsx")
library("DT")
library("rjson")

setwd("C:/Users/srevuelta/OneDrive/Documentos/BusinessIntelligence/SportApp/")
source('AlgorithmTrainings.R')
source('ProcessXML.R')
source('Constants.R')


dfTrainings <- readFromStrava()
write_rds(dfTrainings,"dfTrainings.rds")


