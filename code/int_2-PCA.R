library(tidyverse)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(factoextra)

abb0 <- read_csv("data/intervention-sdg-abb.csv")
abb.t <- read_csv("data/intervention-sdg-abb.csv")

#
#Sdg_abb reorder----------
# abb1$Sdg_abb <- factor(abb1$Sdg_abb, levels = c("1.1",
#                                                 "1.2",
#                                                 "2a",
#                                                 "2.2",
#                                                 "2.3",
#                                                 "2.4",
#                                                 "5 c",
#                                                 "5.5",
#                                                 "6 b",
#                                                 "6.4",
#                                                 "6.5",
#                                                 "7.1",
#                                                 "8.1",
#                                                 "8.2",
#                                                 "8.3",
#                                                 "8.5",
#                                                 "8.8",
#                                                 "8.9",
#                                                 "9.1",
#                                                 "9.3",
#                                                 "9.4",
#                                                 "10.1",
#                                                 "10.2",
#                                                 "11.4",
#                                                 "11.5",
#                                                 "12.2",
#                                                 "12.3",
#                                                 "12.8",
#                                                 "13.1",
#                                                 "13.2",
#                                                 "15 a",
#                                                 "15.1",
#                                                 "15.2",
#                                                 "15.3",
#                                                 "15.4",
#                                                 "15.5",
#                                                 "15.6",
#                                                 "15.7",
#                                                 "15.8",
#                                                 "15.9",
#                                                 "16.6",
#                                                 "16.7",
#                                                 "17.1",
#                                                 "17.16",
#                                                 "17.17",
#                                                 "17.18",
#                                                 "17.6",
#                                                 "17.7",
#                                                 "17.8",
#                                                 "17.9"))
# 
# #
#Attempt 1 ------
df <- iris[c(1,2,3,4)]

abb.t$Int_abb <- as.factor(abb.t$Int_abb)

df.abb <- abb.t[c(2:51)]
# autoplot(prcomp(df.abb))
# 
# autoplot(prcomp(df.abb), data=abb.t, label=T, color='Int_abb')+
#   theme_bw()+
#   labs(x="Principle Component 1", y="Principle Component 2")

#Attempt 2------
test2 <- abb.t %>% 
  remove_rownames() %>% 
  column_to_rownames(var="Int_abb")

yes.label <- autoplot(prcomp(test2), data=abb.t, label=T)+
  theme_bw()+
  labs(x="Principle Component 1", y="Principle Component 2")

no.label <- autoplot(prcomp(test2), data=abb.t, label=F)+
  theme_bw()+
  labs(x="Principle Component 1", y="Principle Component 2")

res.test <- prcomp(test2, scale=T)
fviz_pca_ind(res.test, repel = T)
