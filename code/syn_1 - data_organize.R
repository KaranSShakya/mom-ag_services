#Library and Imports -------
library(tidyverse)
library(readxl)
library(readr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)


data0 <- read_excel("data/synthesis_HKH-final.xlsx")

#By year --------
data.year <- data0 %>% 
  select(4) %>%
  group_by(Year) %>% 
  tally() %>% 
  ungroup()

#By country ------
names(data0)[34] <- "Geog_unit"
unique(data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-NP", "Nepal", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-CN", "China", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-IN", "India", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-BD", "Bangladesh", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-BU", "Bhutan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-PK", "Pakistan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH_NP", "Nepal", data0$Geog_unit)
data0$Geog_unit <- gsub("NKH-NP", "Nepal", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH_BD", "Bangladesh", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-AF", "Afghanistan", data0$Geog_unit)
data0$Geog_unit <- gsub("Otheers", "Others", data0$Geog_unit)
data0$Geog_unit <- gsub("HKh-PK", "Pakistan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKh-IN", "India", data0$Geog_unit)
data0$Geog_unit <- gsub("HH-PK", "Pakistan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-MM", "Myanmar", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-MN", "Myanmar", data0$Geog_unit)
data0$Geog_unit <- gsub("HKh_CN", "China", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH_IN", "India", data0$Geog_unit)
data0$Geog_unit <- gsub("HK_PK", "Pakistan", data0$Geog_unit)
unique(data0$Geog_unit)

data.country <- data0 %>% 
  select(34) %>% 
  group_by(Geog_unit) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate('% of Total' = (n/905)*100)
names(data.country)[1] <- "Country"
names(data.country)[2] <- "Count"
data.country$`% of Total` <- round(data.country$`% of Total`, 1)

country <- c("Afghanistan", "Bangladesh", "Bhutan", "China", "India",
             "Myanmar", "Nepal", "Pakistan", "HKH", "Others")
publication <- c(3, 22, 9, 229, 243, 34, 78, 50, 88, 82)

country1 <- data.frame(name=country, value=publication)
country1$name <- as.factor(country1$name)
country1 <- country1 %>% 
  mutate(name_re = factor(name, levels = c("Afghanistan", "Bangladesh",
                                           "Bhutan", "China", "India", "Myanmar",
                                           "Nepal", "Pakistan", "HKH", "Others")))

country.plot <- ggplot(country1, aes(x=name_re, y=value))+
  geom_bar(stat="identity", fill="skyblue2", color="skyblue4")+
  theme_bw(base_size = 10)+
  labs(x='Country Name', y='Frequency')+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_text(aes(label=value), position=position_dodge(width=0.8), vjust=-0.25)

#Word Cloud (omitted) -------
# names(data0)[18] <- "Auth_keywords"
# data.word1 <- data0 %>% ()
#   select(18) %>% 
#   na.omit()
# 
# word1 <- paste(unlist(data.word1), collapse = "")
# 
# w1 <- Corpus(VectorSource(data.word1))
# 
# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# w1 <- tm_map(w1, toSpace, ";")
# w1 <- tm_map(w1, toSpace, ",")
# w1 <- tm_map(w1, toSpace, "\\|")
# w1 <- tm_map(w1, toSpace, "\n")
# w1 <- tm_map(w1, toSpace, "'")
# w1 <- tm_map(w1, removeWords, "and")
# 
# w1.edit <- tm_map(w1, content_transformer(tolower))
# w1.edit <- tm_map(w1.edit, removeNumbers)
# w1.edit <- tm_map(w1.edit, removePunctuation)
# 
# w1.table <- TermDocumentMatrix(w1.edit)
# w1.m <- as.matrix(w1.table)
# w1.v <- sort(rowSums(w1.m),decreasing=TRUE)
# w1.d <- data.frame(word = names(w1.v),freq=w1.v)
# w1.table <- head(w1.d, 10)
# 
# # w1.plot <- wordcloud(words = w1.d$word, freq = w1.d$freq, min.freq = 1,
# #           max.words=100, random.order=F, rot.per=0.35, 
# #           colors=brewer.pal(8, "Dark2"), 
# #           scale=c(2.00,0.25))
# 
# 
# data.word2 <- data0 %>% 
#   select(19) %>% 
#   na.omit()
# 
# word2 <- paste(unlist(data.word1), collapse = "")
# 
# w2 <- Corpus(VectorSource(data.word2))
# 
# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# w2 <- tm_map(w2, toSpace, ";")
# w2 <- tm_map(w2, toSpace, ",")
# w2 <- tm_map(w2, toSpace, "\\|")
# w2 <- tm_map(w2, toSpace, "\n")
# w2 <- tm_map(w2, toSpace, "'")
# w2 <- tm_map(w2, removeWords, "and")
# 
# w2.edit <- tm_map(w2, content_transformer(tolower))
# w2.edit <- tm_map(w2.edit, removeNumbers)
# w2.edit <- tm_map(w2.edit, removePunctuation)
# 
# w2.table <- TermDocumentMatrix(w2.edit)
# w2.m <- as.matrix(w2.table)
# w2.v <- sort(rowSums(w2.m),decreasing=TRUE)
# w2.d <- data.frame(word = names(w2.v),freq=w2.v)
# w2.table <- head(w2.d, 10)
# 
# # w2.plot <- wordcloud(words = w2.d$word, freq = w2.d$freq, min.freq = 1,
# #           max.words=100, random.order=F, rot.per=0.35, 
# #           colors=brewer.pal(8, "Dark2"), 
# #           scale=c(2.00,0.35))
