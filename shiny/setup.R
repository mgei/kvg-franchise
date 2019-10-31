library(shiny)
library(shinydashboard)
library(shinyjs)
library(dqshiny)

library(tidyverse)
library(plotly)
library(DT)


# data ----

kantone <- c("ZH","BE","LU","UR","SZ","OW","NW","GL","ZG","FR","SO","BS","BL","SH","AR","AI","SG","GR","AG","TG","TI","VD","VS","NE","GE","JU")
kantone <- kantone[order(kantone)]


premia <- readRDS("data/premia.RDS")
lut_hmo <- readRDS("data/lut_hmo.RDS")

weblinks <- readRDS("data/kk_web.RDS")

premia %<>% 
  left_join(weblinks, by = c("Versicherer" = "VersNr")) %>% 
  mutate(VersLink = paste0("<a href='", weblink, "'>", Versicherung, "</a>"))
  

# helper functions ----

unfalleinschluss <- function(unfall) {
  Unfalleinschluss <- case_when(unfall == "Nein|nein|ohne Unfall|o. Unfall" ~ "OHN-UNF",
                                isFALSE(unfall)       ~ "OHN-UNF",
                                unfall == "Ja|ja|mit Unfall|m. Unfall|Unfalleinschluss"     ~ "MIT-UNF",
                                isTRUE(unfall)        ~ "MIT-UNF",
                                T                     ~ "OHN-UNF")
  return(Unfalleinschluss)
}

tariftyp <- function(modell) {
  Tariftyp <- switch(modell,
                     "Basis|Standard" = "TAR-BASE",
                     "andere|Andere|alternative|Alternative|diverse|Diverse|div" = "TAR-DIV",
                     "Hausarzt|Hausarztmodell"          = "TAR-HAM",
                     "HMO"                              = "TAR-HMO",
                     "nur Standard Grundversicherungen" = "TAR-BASE",
                     "auch alternative Modelle"         = c("TAR-BASE", "TAR-DIV", "TAR-HMO", "TAR-HAM"),
                     "TAR-BASE")
  return(Tariftyp)
}



altersklasse <- function(alter) {
  Altersklasse <- case_when(alter == "Erwachsene" ~ "AKL-ERW",
                            alter == "Jugendliche" ~ "AKL-JUG",
                            alter == "Kinder"     ~ "AKL-KIN",
                            alter == "Erwachsene (26+)" ~ "AKL-ERW",
                            alter == "Junge Erwachsene (19-25)" ~ "AKL-JUG",
                            alter == "Kinder (0-18)" ~ "AKL-KIN",
                            T                     ~ "AKL-ERW")
  return(Altersklasse)
}