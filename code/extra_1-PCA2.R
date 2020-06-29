library(tidyverse)
library(readxl)
library(factoextra)

pca0 <- read_excel("data/pca2.xlsx")

pca1 <- pca0 %>% 
  remove_rownames() %>% 
  column_to_rownames(var="Var")

res.pca <- prcomp(pca1, scale=T)

ind.label <- fviz_pca_ind(res.pca, repel = T)
ind.nolabel <- fviz_pca_ind(res.pca, repel = T, label=F)

biplot.label <- fviz_pca_biplot(res.pca, repel = T)

# Stat explain ----
fviz_pca_biplot(res.pca, repel = T)

mrPr <- prcomp(pca0[,-1], scale=T)
biplot(mrPr, scale=0)

#ggsave(filename = "biplot_label.png", plot=biplot.label, device = "png", dpi=300)
