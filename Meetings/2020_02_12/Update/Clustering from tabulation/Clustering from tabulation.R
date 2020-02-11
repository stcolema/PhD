#!/usr/env/bin/R
# Clustering of categorical data

# === Load libraries ===========================================================

# For visualisation
library(pheatmap)

# To create a coclustering matrix
library(mcclust)

# Set a seed for reporducibility
set.seed(1)

# Colour palette for heatmaps
col_pal <- colorRampPalette(c("white", "#146EB4"))(100)
my_breaks <- c(
  seq(0, 1, length.out = ceiling(length(col_pal)) + 1)
)

# == Data generation ===========================================================
# Generate some categorical data
# The number of samples / people considered
n <- 50

# The number of variables in our example
n_var <- 4

# The probability of a 1 in each category (some random value between 0.1 and 1.0)
prob_vec <- sample(5:95, n_var, replace = F) / 100

# Create a list of categorical vectors
my_data_lst <- lapply(prob_vec, function(x) {
  sample(0:1, n, replace = T, prob = c(1 - x, x))
})

# Convert this list to a data.frame
my_df <- data.frame(
  t(
    matrix(
      unlist(my_data_lst),
      nrow = length(my_data_lst),
      byrow = T
    )
  )
)

# Set row names (for annotation of labels in heatmaps)
row.names(my_df) <- paste0("Person_", 1:n)

# Visualise the data
pheatmap(my_df, cluster_cols = F, color = col_pal, breaks = my_breaks)

# === Correlation ==============================================================
# Sometimes correlation doesn't work (for instance if a row is all 0's or all 1's),
# but if it does than this method is the easiest to understand.

# We are interested in the variables with a correlation of 1 (i.e. the same
# category in each variable), so let's look at the correlation matrix!
my_clustering_mat <- cor(t(my_df))
pheatmap(my_clustering_mat, color = col_pal, breaks = my_breaks)

# This is the clustering of interest, but we want only 0's and 1's
# (arguably not true, but that's the assumption for now!). As the 1's
# indicate people with the same category in each variables, we are only
# interested in preserving these. This means we can set all values less than 1
# to 0 to acquire our clustering:
my_clustering_mat[my_clustering_mat < 1.0] <- 0
pheatmap(my_clustering_mat, color = col_pal, breaks = my_breaks)

# === Non-correlation ==========================================================
# IF we cannot use the correlation matrix due to the nature of the data, than we
# can use hclust as we know there is 0 distance between points with the same
# categorisation in each variable (and thus we can define the number of labels
# by taking our cut at an arbitrary small point).

# We want the points in the same categorisation, take the hclust using "binary" distance
hc <- hclust(dist(my_df, method = "binary"))
plot(hc) # check that our choice of cut makes sense

# Create labels by cutting the tree
test_labels <- cutree(hc, h = 0.05)
labels <- cutree(hc, h = 1e-5)

# Small test
if (any(test_labels != labels)) {
  stop("check our hclust plot that the cut in the tree makes sense")
}

# Create a data.frame of the clusters to annotate the pheatmap with
annotation_row <- data.frame(labels = as.factor(labels))
row.names(annotation_row) <- row.names(my_df)

# Visualise the data with the label annotation
pheatmap(my_df, annotation_row = annotation_row)

# Create coclustering matrix (technically cltoSim creates a posterior similarity
# matrix from a clustering vector, but the effect in *this* case is the same)
coclustering_matrix <- mcclust::cltoSim(labels)
row.names(coclustering_matrix) <- colnames(coclustering_matrix) <- row.names(my_df)
pheatmap(coclustering_matrix,
  annotation_row = annotation_row,
  annotation_col = annotation_row,
  color = col_pal,
  breaks = my_breaks
)

# This is a check I had been using to make sure that the correlation method and
# non-correlation methods did agree.
if (!any(is.na(my_clustering_mat))) {
  if (any(coclustering_matrix != my_clustering_mat)) {
    stop(paste0(c(
      "Correlation and non-correlation methods in disagreement.",
      "\nCheck tree-cutting and if ``cor`` function worked.\n"
    )))
  }
}
