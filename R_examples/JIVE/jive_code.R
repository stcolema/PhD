#!/usr/bin/env Rscript

# install.packages("r.jive")
library(r.jive)
library(magrittr)

# set a random seed
set.seed(1)

# === Package toy example ======================================================

# Load data that were simulated as in Section 2.4 of Lock et al., 2013,
# with rank 1 joint structure, and rank 1 individual structure for each dataset
data(SimData)
# Using default method ("perm")
sim_results <- jive(SimData)
summary(sim_results)

# Using BIC rank selection
# BIC_result <- jive(SimData, method="bic")
# summary(BIC_result)

# Visualize results
showVarExplained(sim_results)
# showVarExplained is also called by the "jive" S3 class default plot method

# show heatmaps
showHeatmaps(sim_results)

# show PCA plots
showPCA(Results, 1, c(1, 1))

# === Vignette =================================================================
# https://cran.r-project.org/web/packages/r.jive/vignettes/BRCA_Example.html

# Data on breast cancer (BRCA) tumor samples from The Cancer Genome Atlas
data(BRCA_data)

# Implment JIVE on this list of data matrices (of common width)
# Two algorithms to select the ranks are included as options. One is a
# permutation-based approach described in E. Lock et al. (2013), the other is a
# BIC-motivated approach. We find that the accuracy of permutation estimated
# ranks are generally better, but BIC is less computationally intensive. By
# default the ranks are selected via permutation, with row-orthogonality
# enforced between the joint and invidual estimates and also between each
# individual estimate. Previously orthogonality was only enforced between joint
# and individual estimates, but we find that also enforcing the individual
# estimates to be orthogonal to each other improves convergence and robustness
# of the results to rank mispecification.
BRCA_results <- jive(Data)
summary(BRCA_results)

# Visualisation
# Display a barchart of the amount of variation explained by joint and
# individual estimates in each data source.
showVarExplained(BRCA_results)

# Note that the variation explained by joint structure is higher than that for
# individual structure for methylation data, despite the much higher rank of
# individual structure (rank 26 individual vs. rank 2 joint for methylation).

# Display the JIVE estimates in the form of low-rank matrix approximations
# the rows and columns of all matrices are ordered by complete linkage
# clustering of the joint structure.
showHeatmaps(BRCA_results)

# In addition, the showHeatmaps function includes options to specify how to
# order rows and columns, and which matrices to display. For example, we can
# order by the individual methylation structure (data source 2) and show only
# this heatmap.
showHeatmaps(BRCA_results, order_by = 2, show_all = FALSE)
# One factor appears to be a mean effect, distinguishing those samples with
# relatively high methylation genome-wide from those with relatively low
# methylation.

# To further examine the biological relevance of the estimated joint structure,
# we consider the “point cloud” view provided by the showPCA function. This
# shows the patterns in the column space that maximize variability of joint or
# individual structure, analogous to principal components.
Colors <- rep("black", 348)
Colors[clusts == 2] <- "green"
Colors[clusts == 3] <- "purple"
showPCA(BRCA_results, n_joint = 2, Colors = Colors)

# We see that the estimated joint corresponds well to the three previously
# identified clusters. Specifically, one pattern distinguishes Basal-like tumor
# samples (cluster 1) from other samples; among the remaining samples a subgroup
# of Luminal A tumors with a low fraction of genomic alteration and improved
# clinical prognosis (cluster 2) is distinguished.

# For a broader view, we show the first component of joint structure with the
# first component of each of the three individual structures.
showPCA(BRCA_results, n_joint = 1, n_indiv = c(1, 1, 1), Colors = Colors)
# A clustering effect is not apparant in the individual components shown,
# besides a slight distinction between clusters 2 and 3 in the expression
# individual component. This suggests that the coordinated expression,
# methylation, and miRNA activity in BRCA tumors is primarily driven by the
# cluster effects mentioned above, whereas the activity specific to each data
# source is driven by other biological components.

# == Yeast data ================================================================

# Read in data
time_course_data <- read.csv("./Data/Yeast/Granovskaia_timecourse_normalised_reduced.csv",
  row.names = 1
)
harbison_data <- read.csv("./Data/Yeast/harbison_marina.csv", row.names = 1)
ppi_data <- read.csv("./Data/Yeast/ppi.csv", row.names = 1)

# Ensure object order is the same in all datasets
harbison_data_order <- match(row.names(harbison_data), row.names(time_course_data))
ppi_data_order <- match(row.names(ppi_data), row.names(time_course_data))

harbison_data_ordered <- harbison_data[harbison_data_order, ]
ppi_data_ordered <- ppi_data[ppi_data_order, ]

# Test on a subset of the data
small_d1 <- time_course_data[1:100, ]
small_d2 <- harbison_data_ordered[1:100, 1:60]
small_d3 <- ppi_data_ordered[1:100, 1:100]

# Implement JIVE and visualise the output
my_small_data <- list(
  t(small_d1),
  t(small_d2),
  t(small_d3)
)
jive_small_out <- jive(my_small_data)
showPCA(jive_small_out, 1, c(1, 1, 0))
showHeatmaps(jive_small_out)
showVarExplained(jive_small_out)

# The full set of data
my_data <- list(
  t(time_course_data),
  # t(harbison_data_ordered),
  t(ppi_data_ordered)
)

jive_out <- jive(my_data)

showPCA(jive_out, 2, c(1,2))
showHeatmaps(jive_out)
showVarExplained(jive_out)


my_full_data <- list(
  t(time_course_data),
  t(harbison_data_ordered),
  t(ppi_data_ordered)
)

full_jive_out <- jive(my_full_data)

showPCA(full_jive_out, )
showHeatmaps(full_jive_out)
showVarExplained(full_jive_out)