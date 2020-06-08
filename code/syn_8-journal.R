library(tidyverse)
library(readxl)

data0 <- read_excel("data/synthesis_HKH-final.xlsx")

data1 <- data0 %>% 
  select(5)
names(data1)[1] <- "Journal"

data.sum <- data1 %>% 
  group_by(Journal) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  arrange(desc(n)) %>% 
  head(17)
names(data.sum)[2] <- "Count"

remove(data0, data1)





