#!/usr/bin/env Rscript

library(magrittr)
library(umap)
library(pheatmap)
library(data.table)
library(ggplot2)

#  devtools::install_github("thomasp85/ggforce")
library(ggforce)

# Define breaks for heatmap (this controls the gradient for colours in pheatmap)
defineBreaks <- function(col_pal, lb = 0, ub = 1) {
  palette_length <- length(col_pal)

  mid_point <- mean(c(lb, ub))

  breaks <- c(
    seq(lb, mid_point, length.out = ceiling(palette_length / 2) + 1),
    seq(mid_point + 1 / palette_length, ub, length.out = floor(palette_length / 2))
  )
}

# Read in the data
my_data <- readRDS("./R_examples/Visualisation/Anna_example/dataset.RDS")

# Create a colour palette (white and blue) and colour breaks for pheatmap
col_pal <- c("#FFFFFF", grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100))
breaks <- defineBreaks(col_pal, lb = 0, ub = 1)

# These extensions and whatnot are for the non-binary data (slightly strange
# format as I want to keep [0, 1] in blues so the p-values of interest are easy
# to see)
ext_col_pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Oranges"))(100)
ext_breaks <- c(breaks, defineBreaks(ext_col_pal, lb = 1.01, ub = 13.4))
full_col_pal <- c(col_pal, ext_col_pal)

#
my_second_data <- my_data
my_second_data$P <- -log10(my_second_data$P)

# Heatmap of the entire dataset allowing clustering (this pheatmap contains
# -log10 transformed p-vlaues, possibly the same should be the case in the
# following 2)
ph0 <- pheatmap(my_second_data,
  show_colnames = F,
  show_rownames = F,
  color = full_col_pal,
  breaks = ext_breaks
)


# Create heatmap of the data less the p-values to see if there's strong
# clustering structure in the predictors. We hide row names and column names for
# aesthetic purposes here, do change this if you like
ph1 <- pheatmap(my_data[, -1],
  show_colnames = F,
  show_rownames = F,
  color = col_pal,
  breaks = breaks
)

# Extract the row and column ordering so we can re-do the heatmap with the
# p-values
row_order <- ph1$tree_row$order
col_order <- ph1$tree_col$order + 1 # as column 1 is p-values

# Heatmap as above but the first column is the p-values
ph2 <- pheatmap(my_data[row_order, c(1, col_order)],
  cluster_rows = F,
  cluster_cols = F,
  show_colnames = F,
  show_rownames = F,
  color = col_pal,
  breaks = breaks
)

# Apply UMAP to the predictors
data_umap <- umap(my_data[, -1])

# Create a data.frame of the projected co-ordinates and the associated -log10
# p-values
my_ggplot_data <- data.frame(
  X = data_umap$layout[, 1],
  Y = data_umap$layout[, 2],
  P = -log10(my_data[, 1])
)

# Brief inspection of data
summary(my_ggplot_data)
hist(my_ggplot_data$P)

# Consider removing the tails to ease visualisation
rows2keep <- my_ggplot_data$P < 5

# Base ggplot of data into the UMAP lower dimensions
p <- ggplot(my_ggplot_data[which(rows2keep), ], aes(x = X, y = Y, colour = P, alpha = 5 - P)) +
  geom_point()

# p <- ggplot(my_ggplot_data, aes(x = X, y = Y, colour = P)) +
# geom_point()

# Zooming into different clusters
p + facet_zoom(x = X > 5 & X < 10, y = Y > -15 & Y < -10)

p + facet_zoom(x = X > 0 & X < 15, y = Y > 2.5 & Y < 12)

p + facet_zoom(x = X > 17.5 & X < 25, y = Y > -2.5 & Y < 0)

p + facet_zoom(x = X > -17.5 & X < -5, y = Y > -10 & Y < 5)

p + facet_zoom(x = X > -17.5 & X < -5, y = Y > -10 & Y < -1.125)

p + facet_zoom(x = X > -17.5 & X < -5, y = Y > -2.5 & Y < 5)
