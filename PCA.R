rm(list = ls())

pacman :: p_load(tidyverse, 
                 lattice, 
                 psych, 
                 PerformanceAnalytics, 
                 GGally, 
                 cptcity,
                 corrplot)

#DATA ------------------------------
fires <- read_csv("fires_area.csv") %>% 
  print()


fires_pca <- select(fires, 5:12) %>% 
  print()

#describiendo los datos
describe(fires_pca)

#ploteo


#esttandarizando
fires_pca_esc <- scale(fires_pca) %>% as_tibble() %>% 
  print()


#matriz de correlación
correlation <- cor(fires_pca_esc) %>% 
  print()

corrplot(correlation, 
         method = "number", 
         type = "full",
         diag = TRUE,
         tl.col = "black", 
         bg = "white", 
         title = "Correlation Matrix", 
         col = NULL)

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

#Scree plot 

library(factoextra)
fviz_eig(pca,addlabels = T)

#correlacion entre las variable soriginales y las componetes
levelplot(
  as.matrix(pca$co),
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = F
    )
)

#Varianza explicada
contrib <- as.matrix(pca$co ^ 2)


corrplot(contrib,
         method = "circle",
         tl.col = "black", 
         bg = "white", 
         title = "Contributions", 
         is.corr = F)


as.tibble(fires_pca_esc)
head(pca$li) 
dim(pca$li)

#df de componentes principales
pca_data <- as.tibble(pca$li) %>% 
  dplyr::select(sprintf("Axis%1$s", 1:4)) %>% 
  print()



library(FactoMineR)


fviz_pca_biplot(pca, col.var  = "cos2", 
             geom.var = "arrow", 
             labelsize = 3, 
             repel = FALSE)

fviz_pca_var(pca, col.var  = "cos2", 
                geom.var = "arrow",
                labelsize = 3,
                repel = FALSE)

biplot(pca, scale = 0, cex = 0.5, col = c("blue", "pink"))

library(rgl)


plot3d( 
  x=output$`Axis1`, y=output$`Axis2`, z=output$`Axis3`, 
  col = c("blue", "orange", "red"), 
  type = 's', 
  radius = .15,
  xlab="Comp 1", ylab="Comp 2", zlab="Comp 3")

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

data.class <- fires_pca_esc[,1:8]
data_pca <- prcomp(fires_pca_esc, scale. = F)

g <- ggbiplot(data_pca, obs.scale = 1, var.scale = 1,ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

# library(Rtsne)
# library(ggplot2)
# tsne <- Rtsne(X = fires_pca_esc, is_distance = FALSE, dims = 3, perplexity = 10,
#               theta = 0.5, max_iter = 500)
# head(tsne$Y)
# 
# resultados <- as.data.frame(tsne$Y)
# colnames(resultados) <- c("dim_1", "dim_2")
# ggplot(data = resultados, aes(x = dim_1, y = dim_2)) +
#   geom_point(aes(color = blue)) + 
#   theme_bw()

#CLUSTER ------------------------------------

#Cluster jerárquico

#matriz de distancias
distancia<- dist(pca_data, method = "euclidean")
as.matrix(distancia) %>% 
  dim()

#clusterizando con el metodo ward
hmodel<- hclust(distancia, method = "ward.D") #función rbase

#dendograma
plot(hmodel)

#obteniendo las alturas
hmodel$height #hay 28 alturas

#ploteando 
plot(hmodel$height, type = "p")
lines(hmodel$height)
(hmodel$height)[30]

plot(hmodel)
clust<- cutree(hmodel, k = 4) #solo conservaremos 3 clusters
length(clust)
table(clust) 

#otra forma de clusterizar
cluster::daisy(fires_pca_esc, metric = "euclidean", stand = F)
cluster::diana(fires_pca_esc, metric = "euclidean", stand = F)

library(rgl)

#convertimos nuestra data a amtriz
distancia2<- as.matrix(distancia)
heatmap(distancia2)

          
autoplot(kmeans(fires_pca_esc, 3), data = fires_pca_esc)

library(cluster)
