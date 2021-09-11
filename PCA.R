rm(list = ls())

pacman :: p_load(tidyverse, 
                 lattice, 
                 psych, 
                 PerformanceAnalytics, 
                 GGally, 
                 cptcity,
                 corrplot)

#DATA ------------------------------

#cargando los datos 

fires <- read_csv("forestfiresproj.csv") %>% 
  print()

#describiendo los datos
describe(fires)

#ploteo


#esttandarizando los datos
fires_esc <- scale(fires) %>% as_tibble() %>% 
  print()


#matriz de correlación
correlation <- cor(fires_esc) %>% 
  print()

#obteniendo la matríz de correlación
corrplot(correlation, 
         method = "number", 
         type = "full",
         diag = TRUE,
         tl.col = "black", 
         bg = "white", 
         title = "Correlation Matrix", 
         col = NULL)

#matriz de varianza-covarianza
matrix_cov <- cov(fires_esc) %>% print()


#suma de la diagonal o "varianza total"
diag(matrix_cov) %>% sum() 

chart.Correlation(fires_esc, histogram = T, pch = 20)

# levelplot(
#   correlation,
#   col.regions = 
#     cpt(
#       pal = "cb_div_RdBu_11", n = 100
#     )
# )

#calculando eigenvectors y eigenvalues

eigen <- eigen(matrix_cov) %>% print()

#calculando el pca
library(ade4)

pca <- dudi.pca(fires_esc, 
                scale = F, 
                scannf = F,
                nf = ncol(fires_esc)
)
summary(pca)

#viendo los autovalores
pca$eig

#verificando que la suma de los autovalores sea 7
sum(pca$eig)

#viendo los autovectores
pca$c1

#Scree plot 

library(factoextra)

fviz_screeplot(pca, 
               addlabels = TRUE,
               barfill = "lightblue1",
               barcolor = "lightblue1",
               linecolor= "indianred2",
               ggtheme = theme_grey())

#correlacion entre las variable soriginales y las componetes
levelplot(
  as.matrix(pca$co),
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = F
    ),
  xlab = "Variables",
  ylab = "Componentes"
)

#Varianza explicada o contribución de las variables
contrib <- as.matrix(pca$co ^ 2)

#Gráfico de contribución 

corrplot(contrib,
         method = "circle",
         tl.col = "black", 
         bg = "white", 
         title = "Contributions", 
         is.corr = F)


as.tibble(fires_esc)
head(pca$li) 
dim(pca$li)

#df de componentes principales
pca_data <- as_tibble(pca$li) %>% 
  dplyr::select(sprintf("Axis%1$s", 1:3)) %>% 
  print()



library(FactoMineR)


fviz_pca_ind(pca, col.var  = "cos2", 
             geom.ind = "point",
             col.ind = "royalblue3",
             labelsize = 3,
             repel = FALSE,
             ggtheme = theme_grey())

fviz_pca_biplot(pca, col.var  = "cos2", 
             geom.var = "arrow",
             geom.ind = "point",
             repel = FALSE,
             addEllipses = TRUE,
             col.ind = "tomato2",
             ggtheme = theme_grey(),
             title = "PCA BIPLOT")

library(rgl)


plot3d( 
  x=pca_data$`Axis1`, y=pca_data$`Axis2`, z=pca_data$`Axis3`, 
  col = c("royalblue", "sienna2", "red"), 
  type = 'p', 
  size = 8,
  xlab="Comp 1", ylab="Comp 2", zlab="Comp 3")

#Visualización en 3d 
library(pca3d)
prc <- prcomp(fires_esc, scale.=F)

pca3d(prc, components = 1:3, 
      col = "royalblue1", 
      bg = "gray31",
      axes.color = "sandybrown",
      new = TRUE)

#distancias
pca3d(prc, components = 1:3, 
      col = "royalblue1", 
      bg = "black",
      axes.color = "sandybrown",
      show.shadows = TRUE,
      new = TRUE)

#elipse
pca3d(prc, components = 1:3, 
      col = "royalblue1", 
      bg = "white",
      axes.color = "sandybrown",
      show.ellipses=TRUE,
      ellipse.ci=0.9, show.plane=FALSE,
      new = TRUE)


#CLUSTER ------------------------------------

#Cluster jerárquico---------------

#matriz de distancias
distancia<- dist(fires_esc, method = "euclidean")
d <- as.matrix(distancia)

#Heatmap
heatmap(d)

fviz_dist(dist.obj = distancia, lab_size = 10, 
          gradient = list(low = "#FC4E07", mid = "white", high = "#00AFBB"))


#clusterizando con el metodo ward
hmodel<- hclust(distancia, method = "ward.D") #función rbase

# # Compute cophentic distance
# res.coph <- cophenetic(hmodel)
# 
# # Correlation between cophenetic distance and
# # the original distance
# cor(distancia, res.coph)
# res.hc2 <- hclust(distancia, method = "average")
# cor(distancia, cophenetic(res.hc2))

#dendograma
plot(hmodel)

#obteniendo las alturas
hmodel$height #hay 28 alturas

#ploteando 
plot(hmodel$height, type = "p")
lines(hmodel$height)
(hmodel$height)[30]

plot(hmodel)
clust<- cutree(hmodel, k = 3)
head(clust, n=3)#solo conservaremos 3 clusters
length(clust)
table(clust)

#coloring
# Cut in 4 groups and color by groups
fviz_dend(hmodel, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE,# Add rectangle around groups
          show_labels = FALSE
)

fviz_cluster(list(data = pca_data, cluster = clust),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = TRUE, ggtheme = theme_minimal())



a <- rect.hclust(hmodel, k=4, border="red")

#otra forma de clusterizar
cluster::daisy(fires_pca_esc, metric = "euclidean", stand = F)
cluster::diana(fires_pca_esc, metric = "euclidean", stand = F)


#convertimos nuestra data a amtriz
library(rgl)
library(viridis)
library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
distancia2<- as.matrix(distancia)
heatmap(distancia2, color = col)


col <- colorRampPalette(brewer.pal(50, "RdYlBu"))(256)
heatmap(distancia2)

library(cluster)

#boxplot
#boxplot, caracterizacion de clusters
col <- c("#E69F00","#56B4E9","#009E73","#F0E442")
data2 <- dplyr::bind_cols(scale(pca_data), cluster = clust)
boxplot(
  data2$Axis1 ~ data2$cluster,
  col = col
)

#KMEANS ------------------

# Indicadores para K óptimo

# Valor de silueta

set.seed(2021)
fviz_nbclust(
  pca_data, kmeans, method = "silhouette",
  k.max = 20
)

# Valor de suma de cuadrados totales

set.seed(2021)
fviz_nbclust(
  pca_data, kmeans, method = "wss",
  k.max = 15
)

# NbClust for determining the best number of clusters

library(NbClust)

nb <- NbClust::NbClust(pca_data,
                       distance = "euclidean",
                       min.nc = 2, max.nc = 20,
                       method = "kmeans",
                       index = "all")
fviz_nbclust(nb)

autoplot(kmeans(fires_pca_esc, 3), data = fires_pca_esc)

smoothScatter(fires$Y ~ fires$X,
              colramp = colorRampPalette(palette),
              ylim = c(9, 2))

set.seed(2021)
c4 = kmeans(pca_data, centers = 3, iter.max = 100,
            nstart = 100)

# Silueta por cada cluster

silueta <- cluster::silhouette(c4$cluster, dist(pca_data))

fviz_silhouette(silueta)
#Hkmeans -----------------
# Compute hierarchical k-means clustering
library(factoextra)

set.seed(2021)
fviz_nbclust(
  pca_data, kmeans, method = "silhouette",
  k.max = 20
)
hkmodel <-hkmeans(pca_data, 3)
hkmodel$centers
# Elements returned by hkmeans()
names(hkmodel)

# Visualize the tree
fviz_dend(hkmodel, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(hkmodel, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

length(hkmodel$cluster)
table(hkmodel$cluster)


#CLARA -------------
clara_clus <- clara(pca_data, 5, metric = "euclidean", stand = TRUE,
      samples = 5, pamLike = FALSE)

library(cluster)
library(factoextra)

fviz_nbclust(pca_data, clara, method = "silhouette")+
  theme_classic()

fviz_cluster(clara_clus,
             palette = c("#999999","#E69F00","#56B4E9","#009E73"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)

library(cluster)
autoplot(clara(pca_data, 4))

