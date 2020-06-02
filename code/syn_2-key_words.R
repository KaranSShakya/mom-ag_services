library(tidyverse)
library(readxl)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

data0 <- read_excel("data/synthesis_HKH-copy.xlsx") %>% 
  select(-35:-39)

#Author Keyword -------
data.a <- data0 %>% 
  select(4, 18)

sum(is.na(data.a$Auth_keywords)) #123 rows are NA
data.a  <- data.a %>%  
  na.omit()

data.a2 <- data.a %>% 
  separate_rows(Auth_keywords, sep = ";")  #seperating based on ;
remove(data.a)

data.a2$Auth_keywords <- gsub(" ", "", data.a2$Auth_keywords) #removing space
data.a2 <- as.data.frame(lapply(data.a2, tolower)) #all lower case
data.a2$Year <- as.numeric(as.character(data.a2$Year))

#Time Filer
data.a2.time <- data.a2 %>% 
  filter(Year >= 2016 & Year <= 2020) %>% 
  select(2) 

a2.time <- Corpus(VectorSource(data.a2.time))

a2.time.matrix <- TermDocumentMatrix(a2.time)
a2.time.matrix1 <- as.matrix(a2.time.matrix)
a2.time.v <- sort(rowSums(a2.time.matrix1),decreasing=TRUE)
a2.time.d <- data.frame(word = names(a2.time.v),freq=a2.time.v)

remove(data.a2.time, a2.time, a2.time.matrix, a2.time.matrix1, a2.time.v)

write.csv(a2.time.d, file="output/a-16_20.csv")

#Total
data.a.t <- data.a2 %>% 
  select(2) #4179 key words

a.t <- Corpus(VectorSource(data.a.t))

a.t.matrix <- TermDocumentMatrix(a.t)
a.t.matrix1 <- as.matrix(a.t.matrix)
a.t.v <- sort(rowSums(a.t.matrix1),decreasing=TRUE)
a.t.d <- data.frame(word = names(a.t.v),freq=a.t.v)

remove(data.a.t, a.t, a.t.matrix, a.t.matrix1, a.t.v)


#Index Keyword ----------
data.i <- data0 %>% 
  select(4, 19)

sum(is.na(data.i$Index_keywords)) #388 rows are NA

data.i  <- data.i %>%  
  na.omit()

data.i2 <- data.i %>% 
  separate_rows(Index_keywords, sep = ";")  #seperating based on ;
remove(data.i)

data.i2$Index_keywords <- gsub(" ", "", data.i2$Index_keywords) #removing space
data.i2 <- as.data.frame(lapply(data.i2, tolower)) #all lower case
data.i2$Year <- as.numeric(as.character(data.i2$Year))

#1996 - 2014 
data.i2.96_14 <- data.i2 %>% 
  filter(Year >= 1996 & Year <= 2015) %>% 
  select(2) #4287 key words

i2.96_14 <- Corpus(VectorSource(data.i2.96_14))

i2.96_14.matrix <- TermDocumentMatrix(i2.96_14)
i2.96_14.matrix1 <- as.matrix(i2.96_14.matrix)
i2.96_14.v <- sort(rowSums(i2.96_14.matrix1),decreasing=TRUE)
i2.96_14.d <- data.frame(word = names(i2.96_14.v),freq=i2.96_14.v)

remove(data.i2.96_14, i2.96_14, i2.96_14.matrix, i2.96_14.matrix1, i2.96_14.v)

#2015 - 2020 
data.i2.15_20 <- data.i2 %>% 
  filter(Year >= 2015 & Year <= 2020) %>% 
  select(2) #5497 key words

i2.15_20 <- Corpus(VectorSource(data.i2.15_20))

i2.15_20.matrix <- TermDocumentMatrix(i2.15_20)
i2.15_20.matrix1 <- as.matrix(i2.15_20.matrix)
i2.15_20.v <- sort(rowSums(i2.15_20.matrix1),decreasing=TRUE)
i2.15_20.d <- data.frame(word = names(i2.15_20.v),freq=i2.15_20.v)

remove(data.i2.15_20, i2.15_20, i2.15_20.matrix, i2.15_20.matrix1, i2.15_20.v)

# write.csv(i2.96_14.d, file="output/index-96_14.csv")
# write.csv(i2.15_20.d, file="output/index-15_20.csv")

#Total
data.i.t <- data.i2 %>% 
  select(2) #9106 key words

i.t <- Corpus(VectorSource(data.i.t))

i.t.matrix <- TermDocumentMatrix(i.t)
i.t.matrix1 <- as.matrix(i.t.matrix)
i.t.v <- sort(rowSums(i.t.matrix1),decreasing=TRUE)
i.t.d <- data.frame(word = names(i.t.v),freq=i.t.v)

remove(data.i.t, i.t, i.t.matrix, i.t.matrix1, i.t.v)

