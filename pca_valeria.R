rm(list = ls())
library(psych)
library(PerformanceAnalytics)
library(GGally)
library(lattice)
library(cptcity) #paleta de colores
pacman :: p_load(tidyverse, 
                 lattice, 
                 psych, 
                 PerformanceAnalytics, 
                 GGally, 
                 cptcity)

fires <- read_csv("fires_prueba.csv") %>% 
  print()

fires %>% select(-c("X", "Y", "day"))

fires_pca <- select(fires, 5:13) %>% 
  print()

#describiendo los datos
describe(fires_pca)

#esttandarizando
fires_pca_esc <- scale(fires_pca) %>% as_tibble() %>% 
  print()

#matriz de correlación
correlation <- cor(fires_pca_esc) %>% 
  print()

#matriz de varianza-covarianza
matrix_cov <- cov(fires_pca_esc) %>% print()

#suma de la diagonal o "varianza total"
diag(matrix_cov) %>% sum() 

chart.Correlation(fires_pca_esc, histogram = T, pch = 20)

levelplot(
  correlation,
  col.regions = 
    cpt(
      pal = "cb_div_RdBu_11", n = 100
    )
)

#calculando eigenvectors y eigenvalues

eigen <- eigen(matrix_cov) %>% print()

#calculando el pca
library(ade4)
pca <- dudi.pca(fires_pca_esc, 
                scale = F, 
                scannf = F,
                nf = ncol(fires_pca_esc)
)
summary(pca)

pca$eig

sum(pca$eig)

#autovectores
pca$c1

library(factoextra)
fviz_eig(pca,addlabels = T)

levelplot(
  as.matrix(pca$co),
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = F
    )
)

contrib <- as.matrix(pca$co ^ 2)

library(corrplot)

corrplot(contrib, is.corr = F)


as.tibble(fires_pca_esc)
head(pca$li) #ese li 
dim(pca$li)

output <- as.tibble(pca$li) %>% 
  dplyr::select(sprintf("Axis%1$s", 1:3)) %>% 
  print()