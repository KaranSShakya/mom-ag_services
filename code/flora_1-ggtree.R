# Library ----
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggtree)
library(TreeSummarizedExperiment)
library(stringr)

# library(ape)
# library(data.tree)
# library(treemap)

# Data ----
data.0 <- read_excel("data/flora-raw.xlsx", 
                        sheet = "final revised")
data.1 <- data.0 %>% 
  select(1,2,3) 

data.2 <- data.0 %>%
  select(1,2,3,4) %>% 
  group_by(order) %>% 
  tally() %>% 
  ungroup()

data.label <- merge(data.1, data.2, by="order")
data.label$label <- str_c(data.label$order," ", "[", data.label$n, "]")
data.label2 <- data.label %>% 
  select(2,3,5)

# ggtree:Order ----
data.tree <- toTree(data = data.1)
data.tree2 <- toTree(data = data.label2)

# ggtree(data.tree, layout = "circular")+
#   geom_text2(aes(label=label), color="red", vjust=1,
#              position = position_dodge(width=0.9),
#              check_overlap = TRUE)+
#   geom_nodepoint()

fig1 <- ggtree(data.tree, layout = "circular")+
  geom_nodepoint()+
  geom_tiplab(size=2, aes(angle=angle))

fig2 <- ggtree(data.tree2, layout = "circular")+
  geom_nodepoint()+
  geom_tiplab(size=2, aes(angle=angle))


# ggtree:Family ----
# data.tree2 <- toTree(data = data.2dub)
# 
# fig2 <- ggtree(data.tree2, layout = "circular")+
#   geom_tiplab(size=1, aes(angle=angle))

# Examples ----
## Example 1 ----
    # data(GNI2014)
    # GNI2014$pathString <- paste("world", 
    # 
    #                             GNI2014$continent, 
    #                             GNI2014$country, 
    #                             sep = "/")
    # 
    # population <- as.Node(GNI2014)
    # print(population, "iso3", "population", "GNI", limit = 20)

## Example 2 ----
    # taxTab <- data.frame(R1 = rep("A", 5),
    #                      R2 = c("B1", rep("B2", 4)),
    #                      R3 = paste0("C", 1:5))
    # 
    # tree <- toTree(data = taxTab)
    # 
    # ggtree(tree) + 
    #   geom_text2(aes(label = label), color = "red", vjust = 1) + 
    #   geom_nodepoint()








