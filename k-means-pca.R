library(tidyverse)
library(factoextra)

pca_data

# Indicadores para K óptimo

# Valor de silueta

set.seed(2021)
fviz_nbclust(
  pca_data, kmeans, method = "silhouette",
  k.max = 15
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

nb$Best.nc # Valores máximos de cada indicador

# K-means

set.seed(2021)
c3 = kmeans(pca_data, centers = 3, iter.max = 100,
            nstart = 100)

c3$cluster
c3$centers

# Silueta por cada cluster

silueta <- cluster::silhouette(c3$cluster, dist(pca_data))

fviz_silhouette(silueta)

#Gráfico de clusters basados en sus centroides

centroides_c3 = data.frame(c3$centers)

centroides_c3$kmeans3 = 1:3

library(reshape)
centroides_km3_2 = reshape::melt(centroides_c3, id=c("kmeans3"))

str(centroides_km3_2)

library(ggplot2)
ggplot(centroides_km3_2, aes(x=kmeans3, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")

# Cluster con PCA

cluster::clusplot(pca_data, c3$cluster, color=T, labels=2) 

# * grafico en 3D ----

pc = princomp(fires_pca_esc, cor = T, scores = T)
ls(pc)
head(pc$scores)
class(head(pc$scores))
pca_data

summary(pc)

biplot(pc)

#install.packages('rgl')
library(rgl)

plot3d(pc$scores[, 1:3], col = c4$cluster, size = 10)
rglwidget() # grafico aparece en Viewer

c3_cluster_f = as.factor(c3$cluster) #En factor para graficar con scatter
c3_cluster_f

library(car)
scatter3d(x = pc$scores[,1], y = pc$scores[,2], z = pc$scores[,3],
          xlab = "PC1", ylab = "PC2" , zlab = "PC3",
          groups = c3_cluster_f, surface=FALSE, ellipsoid= TRUE)
rglwidget() # grafico aparece en Viewer

# Validación
library(clusterSim)

# Indice Davies - Boulding

val <- c3$cluster

index <- index.DB(pca_data, val, centrotypes = "centroids")
index$DB

library(clValid)
dunn(distance = NULL, Data = pca_data, clusters = val)

