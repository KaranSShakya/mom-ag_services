#Library
library(tidyverse)
library(readxl)

#Importing excel sheets
current <- read_excel("data/modified.xlsx", 
                      sheet = "ES-current")
demand <- read_excel("data/modified.xlsx", 
                     sheet = "ES-demand")
value <- read_excel("data/modified.xlsx", 
                    sheet = "ES-value")
benefit <- read_excel("data/modified.xlsx", 
                      sheet = "ES-Beneficiaries ")
div <- read_excel("data/modified.xlsx", 
                       sheet = "division", col_names = FALSE)
names(div)[1] <- "eco_ser"
names(div)[2] <- "eco_type"

#Changing data structure - long
current <- current %>% 
  gather(key="eco_ser", value="result", 5:104) %>% 
  select(-4)
names(current)[5] <- "current"

demand <- demand %>% 
  gather(key="eco_ser", value="result", 5:104) %>% 
  select(-4)
names(demand)[5] <- "demand"

value <- value %>% 
  gather(key="eco_ser", value="result", 5:104) %>% 
  select(-4)
names(value)[5] <- "value"

benefit <- benefit %>% 
  gather(key="eco_ser", value="result", 5:104) %>% 
  select(-4)
names(benefit)[5] <- "benefit"

#Merging into one dataset
survey <- merge(x=current, y=demand, by=c("Country", "site", "Type of farming", 
                                          "eco_ser"))
survey <- merge(x=survey, y=value, by=c("Country", "site", "Type of farming", 
                                          "eco_ser"))
survey <- merge(x=survey, y=benefit, by=c("Country", "site", "Type of farming", 
                                        "eco_ser"))
survey <- merge(x=survey, y=div, by="eco_ser")

write.csv(survey, file="data/survey.csv")






