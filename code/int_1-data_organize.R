library(tidyverse)
library(readxl)

#Import ----
base0 <- read_excel("data/Intervention_and_SDGs_target-influence_score.xlsx", 
                    sheet = "Edited")
list0 <- read_excel("data/Intervention_and_SDGs_target-influence_score.xlsx", 
                    sheet = "50 SDGs", col_types = c("text", "numeric", "text"))

base1 <- base0 %>% 
  gather(key="Sdg", value="Rank", 3:52)

data0 <- merge(base1, list0, by="Sdg") %>% 
  select(2,3,1,5,6,4)
data0$Rank <- as.factor(data0$Rank)
data0$Scale <- as.factor(data0$Scale)
data0$Sdg_abb <- as.factor(data0$Sdg_abb)

int_names <- data0 %>% 
  select(1,2)

sdg_names <- data0 %>% 
  select(3,4)

remove(base0, list0, base1)

