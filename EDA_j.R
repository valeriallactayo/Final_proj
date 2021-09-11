library(tidyverse)
library(VIM)
library(skimr)
library(psych)
library(GGally)
library(visdat)
library(plotly)

# Dataset

fires

# Resumenes estadísticos

summary(fires)
str(fires)
describe(fires)

# Missing Values

na_g1 <- VIM::aggr(fires, numbers = T, sortVars = T)

# Distribución espacial

palette <- hcl.colors(30, palette = "inferno")

smoothScatter(fires$Y ~ fires$X,
              colramp = colorRampPalette(palette),
              ylim = c(9, 2))

# Distribución de cantidad de incendios por mes

incendios_g <- ggplot(fires, aes(x =month, fill = month)) +
  geom_bar() +
  geom_label(stat='count', aes(x = month, label = ..count..),
             vjust = -0.1, size = 3, color = "white") +
  ggtitle("Distribución de áreas quemadas por mes") +
  labs(x = "Meses", y = "Cantidad de incendios") +
  theme_modern_rc() +
  theme(legend.position = "none")

# Pieplot de cantidad de incendios por mes

library(ggrepel)

fires_mes <- 
  fires %>% 
  dplyr::group_by(
    month
  ) %>% 
  dplyr::summarize(
    value = n(),
    porc = round((n()/517)*100,1)
  )

df <- fires_mes %>% 
  mutate(csum = rev(cumsum(rev(porc))), 
         pos = porc/2 + lead(csum, 1),
         pos = if_else(is.na(pos), porc/2, pos))

pie <- ggplot(df, aes(x = "" , y = porc, fill = fct_inorder(month))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  geom_label_repel(data = df,
                   aes(y = pos, label = paste0(porc, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Mes")) +
  theme_void()

# Distribución de variables por boxplot

library(reshape2)

fires_long2 <- melt(fires_ars %>% 
                      dplyr::select(
                        -c(X, Y)
                      ))

g1 <- ggplot(fires_long2, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = variable)) +
  geom_jitter(color="red", size = 0.2, alpha=0.2) +
  theme_bw() +
  labs(
    title = "Distribución de variables por mes",
    caption = "Elaboración propia") +
  facet_wrap(~month, scales = "free_y", 
             strip.position = "top") + 
  theme(legend.position = "left")
# axis.ticks.x=element_blank() en theme()
ggplotly(g1)

# Distribución de variables por densidad

library(ggridges)

ggplot(fires_long2, aes(x = value, y = variable)) +
  geom_density_ridges() +
  facet_wrap(~month, scales = "free")

ggplot(fires_long2, aes(x = value, y = variable, fill = stat(quantile))) +
  stat_density_ridges(quantile_lines = FALSE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient") +
  scale_fill_brewer(name = "") +
  facet_wrap(~month, scales = "free")

# Boxplot áreas por mes

library(viridis)
library(hrbrthemes)


ggplot(fires, aes(x = month, y = area)) +
  geom_boxplot(aes(fill = month, alpha = 0.5)) +
  geom_jitter(color="red", size=1.5, alpha=0.5, width = 0.1) +
  theme_ipsum_tw() +
  facet_wrap(~month, scales = "free") +
  labs(
    title = "Áreas quemadas por mes",
    subtitle = "Áreas en hectáreas",
    caption = "Elaboración propia"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position = "none")

ggplotly(g1)

# Boxplot áreas != 0.0 ha

fires_ars <-
  fires %>% 
  dplyr::filter(
    area >= 1
  ) %>% 
  dplyr::select(-rain)

write.csv(fires_ars, "datasets/fires_ars.csv")


summary(fires_ars)
describe(fires_ars)

ggplot(fires_ars, aes(x = month, y = area)) +
  geom_boxplot(aes(fill = month, alpha = 0.5)) +
  geom_jitter(color="red", size=1.5, alpha=0.5, width = 0.1) +
  theme_ipsum_tw() +
  facet_wrap(~month, scales = "free") +
  labs(
    title = "Áreas quemadas significativas por mes",
    subtitle = "Áreas mayores a 0 hectáreas",
    caption = "Elaboración propia"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position = "none")

# Boxplot temperatura por mes

ggplot(fires, aes(x = month, y = temp)) +
  geom_boxplot(aes(fill = month, alpha = 0.5)) +
  geom_jitter(color="red", size=1.5, alpha=0.3, width = 0.1) +
  theme_ipsum_tw() +
  facet_wrap(~month, scales = "free") +
  labs(
    title = "Temperatura por mes",
    subtitle = "Temperatura en °C",
    caption = "Elaboración propia"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position = "none")

ggplot(fires_ars, aes(x = month, y = area)) +
  geom_boxplot(aes(fill = month, alpha = 0.5)) +
  geom_jitter(color="red", size=1.5, alpha=0.5, width = 0.1) +
  theme_ipsum_tw() +
  facet_wrap(~month, scales = "free") +
  labs(
    title = "Áreas quemadas significativas por mes",
    subtitle = "Áreas mayores a 0 hectáreas",
    caption = "Elaboración propia"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position = "none")

# Boxplot temperatura por mes

ggplot(fires_arss, aes(x = month, y = temp)) +
  geom_boxplot(aes(fill = month, alpha = 0.5)) +
  geom_jitter(color="red", size=1.5, alpha=0.3, width = 0.1) +
  theme_ipsum_tw() +
  facet_wrap(~month, scales = "free") +
  labs(
    title = "Temperatura por mes",
    subtitle = "Temperatura en °C",
    caption = "Elaboración propia"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position = "none")


library(ggridges)

g5 <- ggplot(fires, aes(x = temp, y = month, fill = stat(x))) +
  geom_density_ridges_gradient(quantile_lines = TRUE, alpha = 0.75,
                               quantiles = c(0.05, 0.5, 0.95)) +
  scale_fill_viridis_c(name = "T (°C)", option = "C") +
  theme_ipsum() +
  labs(x = "Temperatura", y = "Mes")


# Data filtrada por el periodo de mayor ocurrencia de incendios

fires_per # Se consideran áreas de 0 ha

fires_pa <- 
  fires_per %>% 
  dplyr::filter(
    area > 0.00
  )

g3 <- ggplot(fires_pa, aes(x = "Aug - Sep", y = area)) +
  geom_boxplot() +
  geom_jitter(color="red", size=1.5, alpha=0.5) +
  theme_ipsum_tw() +
  labs(
    title = "Áreas quemadas significativas para el periodo de mayor ocurrencia de incendios por año",
    subtitle = "Áreas mayores a 0 hectáreas",
    caption = "Elaboración propia"
  ) +
  theme(legend.position = "none")

ggplotly(g3)

fires_pap <- # Sin contar areas > 600 ha
  fires_pa %>% 
  dplyr::filter(
    area < 600
  )

g4 <- ggplot(fires_pap, aes(x = "Aug - Sep", y = area)) +
  geom_boxplot() +
  geom_jitter(color="red", size=1.5, alpha=0.5) +
  theme_ipsum_tw() +
  labs(
    title = "Áreas para el periodo Aug - Sep",
    subtitle = "Áreas mayores a 0 hectáreas sin valores atípicos",
    caption = "Elaboración propia"
  ) +
  theme(legend.position = "none")

# Data para procesamiento

fires_area <- 
  fires_per %>% 
  group_by(grid) %>% 
  dplyr::filter(area == max(area)) %>% 
  arrange(grid) %>% 
  dplyr::filter(X != 3 | Y != 6 | wind != 1.3) %>% 
  dplyr::select(-rain)

write.csv(fires_area, "datasets/fires_area.csv")


  # Matriz de correlación

ggpairs(fires_area[,-c(1:3, 12)])

cor(fires_area[, -c(1:3, 12)])
corrplot::corrplot(cor(fires_area[, -c(1:3, 12)]))

library(lattice)
library(cptcity)

levelplot(
  cor(fires_area[, -c(1:3, 12)]),
  order = "hclust",
  col.regions = 
    cpt(
      pal = "cb_div_RdBu_11", n = 100
    )
)

# Matriz de correlación

cor_fires <- cor(fires_area[-c(1:3, 12)])
cor.plot(cor_fires, order = "hclust")

library(corrplot)
corrplot(cor(fires_area[-c(1:3, 12)]), order = "hclust")


fires_pca
