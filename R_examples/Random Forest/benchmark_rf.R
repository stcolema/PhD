#!/usr/env/bin/Rscript

# Curious how random forest scales with N and P

# Various libraries to generate data, run random forest, benchmark and visualise
library(randomForest)
library(mdiHelpR) # devtools::install_github("stcolema/mdiHelpR")
library(ggplot2)
library(magrittr)
library(microbenchmark)

# Generate data (too large for local computer)
if(! interactive()){
small_p <- generateSimulationDataset(10, 1e5, 5, delta_m=0.3)
small_n <- generateSimulationDataset(10, 100, 1e4, delta_m=0.3)

annotatedHeatmap(small_p$data, small_p$cluster_IDs)
annotatedHeatmap(small_n$data, small_n$cluster_IDs)

mbm <- microbenchmark(
  "large_n" = randomForest(x = small_p$data, y=small_p$cluster_IDs),
  "large_p" = randomForest(small_n$data,y=small_n$cluster_IDs),
  times = 5
  )

autoplot(mbm) +
  labs(
    title = "Random forest",
    subtitle = "microbenchmark run 10 times for (1e5 x 5) and (1e2x 1e4) datasets"
  )
}

# Slightly smaller and more reasonable data (with only two classes)
small_p_2 <- generateSimulationDataset(2, 1000, 50, delta_m=0.5)
small_n_2 <- generateSimulationDataset(2, 50, 1000, delta_m=0.3)

# Visualise the data to see how separable the classes are
annotatedHeatmap(small_p_2$data, small_p_2$cluster_IDs)
annotatedHeatmap(small_n_2$data, small_n_2$cluster_IDs)

# Bench mark
mbm_2 <- microbenchmark(
  "large_n" = randomForest(x = small_p_2$data, y=small_p_2$cluster_IDs),
  "large_p" = randomForest(small_n_2$data,y=small_n_2$cluster_IDs),
  times = 10
  )

# Visualise
autoplot(mbm_2) +
  labs(
    title = "Random forest",
    subtitle = "10 runs for (1000 x 50) and (50 x 1000) datasets"
  )
