library(tidyverse)
library(dplyr)
library(readxl)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

data0 <- read_excel("data/synthesis_HKH-final.xlsx")

#Combining and Unique--------
names(data0)[2] <- "Auth_id"
names(data0)[18] <- "Auth_keywords"
names(data0)[19] <- "Index_keywords"

data1 <- data0 %>% 
  select(2, 4, 18, 19)
data1 <- as.data.frame(lapply(data1, tolower)) #all lower case
data1$Auth_keywords <- gsub(" ", "", data1$Auth_keywords)
data1$Index_keywords <- gsub(" ", "", data1$Index_keywords)
data1$Auth_id <- gsub(";", "", data1$Auth_id)

data2 <- data1 %>% 
  mutate(Key=seq(1:838)) %>% 
  select(-1) %>% 
  select(4,1,2,3)

data2.a <- data2 %>% 
  select(1,2,3) %>% 
  na.omit() %>% 
  separate_rows(Auth_keywords, sep=";")

data2.i <- data2 %>% 
  select(1,2,4) %>% 
  na.omit() %>% 
  separate_rows(Index_keywords, sep=";")

data2.merge <- merge(data2.a, data2.i, by=c("Key", "Year"), all=T)

sum(is.na(data2.merge$Auth_keywords)) #928 missing words
sum(is.na(data2.merge$Index_keywords)) #1631 missing words

data2.merge <- data2.merge %>% 
  mutate(identical = data2.merge$Auth_keywords==data2.merge$Index_keywords)

data2.merge$identical <- data2.merge$identical %>% 
  replace_na("NO")

data2.filter <- data2.merge %>% 
  filter(identical != "TRUE")

data3 <- data2.filter %>% 
  select(-5)

data3.a <- data3 %>% 
  select(1,2,3) %>%
  group_by(Key, Year) %>%
  distinct(Auth_keywords, .keep_all = T) %>% 
  ungroup() %>% 
  select(-1) %>% 
  group_by(Year) %>% 
  summarise(a_key = paste(Auth_keywords, collapse = ";")) %>% 
  ungroup()

data3.i <- data3 %>% 
  select(1,2,4) %>%
  group_by(Key, Year) %>%
  distinct(Index_keywords, .keep_all = T) %>% 
  ungroup() %>% 
  select(-1) %>% 
  group_by(Year) %>% 
  summarise(i_key = paste(Index_keywords, collapse = ";")) %>% 
  ungroup()

data4 <- merge(data3.a, data3.i, by="Year")  
data4$Combine <- with(data4, paste0(a_key, i_key))

data5 <- data4 %>% 
  select(-2,-3)

data5$Combine <- gsub("NA", "", data5$Combine)

data5.s <- data5 %>% 
  separate_rows(Combine, sep = ";")
data5.s <- data5.s %>% 
  separate_rows(Combine, sep = ",")

#Filtering by Year--------
data5.time <- data5.s %>% 
  filter(Year >= 1995 & Year <= 2020) %>% 
  select(2) 

a5.time <- Corpus(VectorSource(data5.time))

a5.time.matrix <- TermDocumentMatrix(a5.time)
a5.time.matrix1 <- as.matrix(a5.time.matrix)
a5.time.v <- sort(rowSums(a5.time.matrix1),decreasing=TRUE)
a5.time.d <- data.frame(word = names(a5.time.v),freq=a5.time.v) 

a5.time.d <-a5.time.d %>% 
  slice(-1,-2,-3,-5, -11, -15, -20, -21, -22, -30, -32, -39, -50)

a5.time.d$word <- gsub(",", "", a5.time.d$word)
a5.time.d$word <- gsub('"', '', a5.time.d$word)

set.seed(9284)
w1.plot <- wordcloud(words = a5.time.d$word, freq = a5.time.d$freq, min.freq = 1,
                              max.words=40, random.order=F, rot.per=0.25, 
                              colors=brewer.pal(8, "Dark2"), 
                              scale=c(2.00,0.10))

remove(data5.time, a5.time, a5.time.matrix, a5.time.matrix1, a5.time.v)

#write.csv(a5.time.d, file="output/keyword-16_20.csv")  

