library(tidyverse)
library(dplyr)
library(readxl)
library(readr)

key <- read_csv("data/hkh-keyword.csv") %>% 
  ungroup() %>% 
  select(-1) %>% 
  mutate(id=seq(1:13115)) %>% 
  select(3,1,2)

#
#Group division-----
found <- c("cropdiversity", "livestock", "geneticdiversity", "landraces", 
  "paddylandraces", "ricelandraces", "traditionalcrops", "cropproduction",
  "cropimprovement", "germplasm")

func <- c("ecosystemservice", "ecosystemservices", "traditionalknowledge",
          "integratedfarming", "integratedapproach", "integratedfarmingsystems",
          "integratedapproach", "sustainableintensification", "conservation", 
          "conservationagriculture", "ecologicalagriculture", "	integratedpestmanagement",
          "soilnutrient", "watermanagement")

appli <- c("foodsecurity", "agribusiness", "foodsupply", "agritourism",
           "livelihood", "livelihoods", "farmincome", "nutrition")

engage <- c("sustainability", "sustainableagriculture", "adaptation",
            "resilience", "ecosystemresilience")

drive <- c("climatechange", "landusechange", "pesticide", "pesticides")

#Division-------

