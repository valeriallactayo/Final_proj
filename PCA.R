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


pc <- plot_ly(pca_data, x = ~Axis1, y = ~Axis2, z = ~Axis3, color = "blue")
print(pc)

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

names <- "height"
heights <- as.data.frame(hmodel$height) #hay 28 alturas

#ploteando las alturas
plot(hmodel$height, type = "p")+ lines(hmodel$height)

ggplot(heights, aes(x = index(heights), y = hmodel$height)) +
  geom_line(color = "royalblue2",    # Color de la línea
            lwd = 1,      # Ancho de la línea
            linetype = 1) +
  geom_vline(xintercept=30, color = "orange") +
  geom_point(color = "royalblue4")+ labs( x = "Observaciones", y = "Altura",
          title ="Visualización de alturas")+
  theme(plot.title = element_text(hjust = 0.5))

#Obteniendo el valor de la altura

(hmodel$height)[30]

#cortando el dendograma

clust<- cutree(hmodel, k = 3)
length(clust)
#numero de elementos en cada cluster
table(clust)

#visualizando los grupo en el dendograma

fviz_dend(hmodel, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800"),
          rect_fill = TRUE)

#visualizando los grupos

fviz_cluster(list(data = pca_data, cluster = clust),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = TRUE, ggtheme = theme_minimal()
)



library(cluster)

#boxplot
#boxplot, caracterizacion de clusters
col <- c("#2E9FDF", "#00AFBB", "#E7B800")
data2 <- dplyr::bind_cols(pca_data, cluster = as.numeric(clust))

#visualizando en 3d
library(plotly)
fig <- plot_ly(data2, x = ~Axis1, y = ~Axis2, z = ~Axis3,
               marker = list(color = ~cluster, colorscale = c('#FFE1A1', '#683531'), showscale = FALSE))
print(fig)

#Hkmeans -----------------
# Compute hierarchical k-means clustering
library(factoextra)

hkmodel <-hkmeans(pca_data, 3)
hkmodel$centers
# Elements returned by hkmeans()
names(hkmodel)

# Visualize the tree
fviz_dend(hkmodel, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters

fviz_cluster(hkmodel,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = TRUE, 
             ggtheme = theme_minimal()
)

length(hkmodel$cluster)
head(hkmodel$data)
table(hkmodel$cluster)

library(plotly)
datahk <- dplyr::bind_cols(hkmodel$data, cluster = as.numeric(clust))

fig <- plot_ly(datahk, x = ~Axis1, y = ~Axis2, z = ~Axis3,
               marker = list(color = ~cluster, colorscale = c('#FFE1A1', '#683531'), showscale = FALSE))
print(fig)



