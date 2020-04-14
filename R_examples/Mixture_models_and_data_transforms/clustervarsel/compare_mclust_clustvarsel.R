#!/usr/bin/env Rscript

# For ``arandi()`` a function that compares two clusterings
library(mcclust)     # install.packages("mcclust")

# For the pipe (``%>%``)
library(magrittr)    # install.packages("magrittr")

# For mixture models
library(mclust)

# For mixture models with variable selection
library(clustvarsel) # install.packages("clustvarsel")

# For parallel aspects of ``clustvarsel``
library(parallel)    # install.packages("parallel")
library(doParallel)  # install.packages("doParallel")

# For some visualisation
library(pheatmap)

# To install the ``mdiHelpR`` package from github
# install.packages("devtools")

# For data generation and some visualisation methods
library(mdiHelpR)    # devtools::install.github("stcolema/mdiHelpR")

# For reproducibility
set.seed(1)

# First generate some nice data with 0 irrelevant features (i.e. none that do not 
# help with our clustering!)
# type ``?generateSimulationDataset`` into the command line to see what this is
# This generates a dataset of 200 points with 5 clusters and 20 variables
# The cluster means are 1.0 apart within each variable
nice_data <- generateSimulationDataset(5, 200, 20, p_n = 0, delta_mu = 1)

# Fit a mixture model as we've seen already
mclust_1 <- Mclust(nice_data$data, G=2:15)

# Compare the predicted clustering to the true clustering (0 = no better than random, 1 = perfect)
arandi(mclust_1$classification, nice_data$cluster_IDs)

# Plot the data, annotated by its true label
annotatedHeatmap(nice_data$data, nice_data$cluster_IDs, 
  cluster_cols = F,
  main = "Tidy data (true labels)"
)

# Plot the data annotated by the predicted label
annotatedHeatmap(nice_data$data, mclust_1$classification, 
  cluster_cols = F,
  main = "Tidy data (predicted labels)"
)

# It's the same! (we knew this from the call to arandi, but nice to see visually)

# Now generate some data where there's twice as many irrelevant features as 
# there is relevant features
noisy_data <- generateSimulationDataset(5, 200, 20, p_n = 40, delta_mu = 1)

# Let's fit a Mclust model to this!
mclust_2 <- Mclust(noisy_data$data, G=2:15)

# And compare the predicted and true labels
arandi(mclust_2$classification, noisy_data$cluster_IDs)

# That's pretty bad :(

# Plot the data, annotated by its true label
annotatedHeatmap(noisy_data$data, noisy_data$cluster_IDs,
  cluster_cols = F,
  main = "Noisy data (true labels)"
)

# Plot the data annotated by the predicted label - it's put everything in one 
# cluster pretty much! But there is no cluster structure in most of the 
# variables, so I guess this isn't strictly wrong? Most of the features follow
# a continuum with no subpopulation structure.
annotatedHeatmap(noisy_data$data, mclust_2$classification,
  cluster_cols = F,
  main = "Noisy data (predicted labels)"
)

# Let's try clustervarsel; we set ``parallel = TRUE`` as it is super slow and 
# needs to use all of our cores (this makes running things elsewhere slow as 
# ``clustvarsel`` is now using all of the computing power it can access).

#### WARNING: This took about 2 hours to run on my laptop!
clustervarsel_mod_1 <- clustvarsel(noisy_data$data, G = 2:15, parallel = T)

# That's a lot slower! I hope the results are better.

# Let's see what variables are used:
vars_used <- clustervarsel_mod_1$subset
vars_used

# vars_used:
# Gene_11 Gene_16 Gene_20  Gene_2  Gene_4  Gene_9 Gene_14  Gene_8 Gene_10  Gene_6 Gene_17  Gene_5 Gene_12 Gene_15  Gene_1 
# 11      16      20       2       4       9      14       8      10       6      17       5      12      15       1 
# Gene_13  Gene_7 Gene_18  Gene_3 Gene_19 
# 13       7      18       3      19

# If you like use
# vars_used <- c(1:20) %>% set_names(paste0("Gene_", 1:20))

# And which variables are correlated with them in case some variables are 
# excluded as the contribute no original signal due to high correlation with
# another variable that is included. I think it might still make sense to exclude,
# but let's see if any are! if none are then we don't have to worry about this :)
cor_mat <- cor(noisy_data$data)
cor_mat[vars_used, ] %>% 
  pheatmap()
   
# Looking at this we see that no variables are excluded that we would hope are 
# not

# compare the predicted and true labels
arandi(clustervarsel_mod_1$model$classification, noisy_data$cluster_IDs)

# A perfect score!

# Plot the data, annotated by its true label
annotatedHeatmap(noisy_data$data, noisy_data$cluster_IDs,
  cluster_cols = F,
  main = "Noisy data (true labels)"
)

# Plot the data annotated by the predicted label
annotatedHeatmap(noisy_data$data, clustervarsel_mod_1$model$classification,
  cluster_cols = F,
  main = "Noisy data (predicted labels)"
)
