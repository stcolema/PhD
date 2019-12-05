#!/usr/bin/env Rscript


if (!requireNamespace("ttriche/bayesCC", quietly = TRUE)) {
  BiocManager::install("ttriche/bayesCC")
}

library(bayesCC)
library(dplyr)
library(parallel)
library(ggplot2)

set.seed(1)

# Centers of clusters
means <- c(-1.5,1.5)

groupCounts <- c(50, 10, 40, 60)

# Helper function to generate test data
testData_all <- clusternomics::generateTestData_2D(groupCounts, means)
datasets <- testData_all$data
testData <- datasets %>% lapply(t)

# Context 1
qplot(datasets[[1]][, 1], datasets[[1]][, 2], col = factor(testData_all$groups)) +
  geom_point(size = 3) +
  ggtitle("Context 1") + xlab("x") + ylab("y") +
  scale_color_discrete(name = "Cluster")

# Context 2
qplot(datasets[[2]][, 1], datasets[[2]][, 2], col = factor(testData_all$groups)) +
  geom_point(size = 3) +
  ggtitle("Context 2") + xlab("x") + ylab("y") +
  scale_color_discrete(name = "Cluster")

# can take a while...
data(BRCAData)

n_iter <- 10000

# try a few
Ks <- 2:10
names(Ks) <- paste0("K", Ks)

runK <- function(k) bayesCC(BRCAData, K=k, IndivAlpha=T, maxiter=n_iter)
Results <- mclapply(Ks, runK)

# ?alphaStar
alphaStarDist <- data.frame(lapply(Results, alphaStar))
boxplot(alphaStarDist, main="Mean-adjusted adherence by K")
