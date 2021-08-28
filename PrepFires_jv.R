library(tidyverse)

# Preprocesamiento >>> Elección de variables

plot(fires_port_2$X, fires_port_2$Y, pch = 20)
axis(1, at = c(1:9))
axis(2, at = c(1:9))
grid(nx = 9, ny = 9, lty = 2, col = "gray", lwd = 2)

summary(fires_port_2)
str(fires_port_2)
table(as.numeric(fires_port_2$month))
hist(as.numeric(fires_port_2$month))
class(fires_port_2$month)

table(as.numeric(fires_port_2$month %in% c(8,9)))/sum(table(as.numeric(fires_port_2$month)))

fires <- 
  fires_port_2 %>% 
  dplyr::filter(
    month %in% c(8, 9)
  ) %>% 
  mutate(
    grid = as.factor(paste(X, Y, sep = "-"))
  )%>% 
  dplyr::select(
    -4
  ) 

length(levels(fires$grid))

ggplot(fires, aes(x = grid, fill = month)) +
  geom_bar(position = "dodge2")

ggplot(fires, aes(x = X, y = Y)) +
  geom_hex(col = "white") +
  scale_fill_viridis_c()

palette <- hcl.colors(30, palette = "inferno")
smoothScatter(fires$Y ~ fires$X,
              colramp = colorRampPalette(palette),
              ylim = c(9, 2))

?smoothScatter

axis(1, at = c(1:9))
axis(2, at = c(0:9))
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)


fires_prueba <- 
  fires %>% 
  group_by(grid) %>%
  filter(temp == max(temp)) %>%
  arrange(grid)

write.csv(fires_prueba, file = "datasets/fires_prueba.csv")

library(skimr)
skim(fires_prueba)

library(VIM)
aggr(fires_prueba)

library(GGally)
ggpairs(fires_prueba)

cor(fires_prueba[, -1])
