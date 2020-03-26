#!/usr/bin/env Rscript

library(mdiHelpR)
library(ggplot2)

set.seed(1)
setMyTheme()

# Parameters defining generated data
K <- 5
n <- 100
p <- 20

# Generate a dataset
my_data <- generateSimulationDataset(K = K, p = p, n = n)

# PCA
pc1 <- prcomp(my_data$data)

# My different flavours of pca plots
# Items over components as a series
p1 <- pcaPlot(pc1$x, labels = my_data$cluster_IDs)

# Explicitly add lines and a median value
p2 <- pcaSeriesPlot(pc1$x,
  labels = my_data$cluster_IDs,
  include_area = F,
  n_comp = 6
)

# Include a ribbon about the inter-quantile range
p3 <- pcaSeriesPlot(pc1$x,
  labels = my_data$cluster_IDs,
  include_area = T,
  n_comp = 6
)

# View the results
p1 + labs(title = "PCA plot")
p2 + labs(title = "PCA plot including medians")
p3 + labs(title = "PCA plot including medians and inter-quartile range")

# Let's add a facet wrap!
p3 +
  facet_wrap(~Cluster) +
  labs(title = "PCA plot including medians and inter-quartile range")

# New facet label names for cluster variable
cluster_labs <- c(paste0("Cluster ", 1:5))
names(cluster_labs) <- 1:5

# Create the plot
p3 +
  facet_wrap(~Cluster, labeller = labeller(Cluster = cluster_labs)) +
  labs(title = "PCA plot including medians and inter-quartile range") + 
  theme(legend.position="none")

