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

#setwd(pathTrainings) 
#dfTrainings <- time4Training(pathTrainings)
dfTrainings <- readFromStrava()
#dfTrainings2 <- readFromStravaPurr()
write_rds(dfTrainings,"dfTrainings.rds")

fileName <- "info.json"
filter <- paste0("@",template)
options <- paste0("-H ","\"",
                  "Authorization: Bearer 20e1effa3a5126d4cb50a9564c902b86d1cd2096","\"")
url <- "https://www.strava.com/api/v3/athlete"
options <- paste(options,"--url",url)
system2("curl",args=options,stdout = fileName)

#####
fileName <- "info.json"
filter <- paste0("@",template)
options <- paste0("-H ","\"",
                  "Authorization: Bearer 20e1effa3a5126d4cb50a9564c902b86d1cd2096","\"")
url <- "https://www.strava.com/api/v3/athlete/activities?before='&'after='&'page='&'per_page="
options <- paste(options,"--url",url)
system2("curl",args=options,stdout = fileName)

