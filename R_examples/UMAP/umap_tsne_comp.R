

# install.packages("plot3D")
library(plot3D)
library(MLmetrics)
library(pracma)
library(plotly)

umap_cost <- function(X, Y){
  z <- exp(-X**2)*log(exp(-X**2) * (1 + Y**2) + 1) + (1 - exp(-X**2))*log(1 + ((1-exp(-X**2)) * (1 + Y**2) / (Y**2+0.01)))
  z
}

tsne_cost <- function(X, Y){
  z <- exp(-X**2) * (log(1 + exp(-X**2) * (1 + Y**2)))
  z
}

X <- seq(0, 3, 0.01)
Y <- seq(0, 3, 0.01)
new_xy <- meshgrid(X, Y)

Z1 <- umap_cost(new_xy$X, new_xy$Y)
Z2 <- tsne_cost(new_xy$X, new_xy$Y)

p_umap <- plot_ly(colors = colorRamps::matlab.like2(20)) %>% add_surface(x = ~X, y = ~Y, z = ~Z1) %>%
  layout(
    scene = list(
      xaxis = list(title = "Distance in original space"),
      yaxis = list(title = "Distance in reduced space"),
      zaxis = list(title = "Cost function (CE)")
    )
  ) %>% 
  colorbar(title = "CE")

p_tsne <- plot_ly(colors = colorRamps::matlab.like2(20)) %>% add_surface(x = ~X, y = ~Y, z = ~Z2) %>%
  layout(
    scene = list(
      xaxis = list(title = "Distance in original space"),
      yaxis = list(title = "Distance in reduced space"),
      zaxis = list(title = "Cost function (KL)")
    )
  ) %>% 
  colorbar(title = "KL")

p_umap
p_tsne
