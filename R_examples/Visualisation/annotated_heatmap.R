#!/usr/bin/Rscript

# Example of an annotated heatmap for Eckart

# Install MDI Help R, an R package that includes the annotated heatmap function
# it's not on CRAN so it has to be installed using devtools!

if (!require(devtools)) {
  install.packages("devtools")
}


if (!require(mdiHelpR)) {
  devtools::install_github("stcolema/mdiHelpR")
}

# Load libraries
library(mdiHelpR)
library(magrittr)
library(pheatmap)
library(viridis)

# === Data preparation =========================================================

# I generate some data to show what's happening
K <- 2
N <- 100
P <- 40

my_data <- generateSimulationDataset(K, N, P)

# This step is unnecessary, it's just closer to what you probably have,
# I imagine you have a data.frame with the immune cell data and a vector
# with the control/patient labelling
act_data <- my_data$data
control_ids <- my_data$cluster_IDs

# If the control patient labelling is a column (say column 1) in your data.frame
# do something like:
# act_data <- eck_data[, -1]
# control_ids <- eck_data[, 1]


# Make sure your data has row names! If it doesn't make some up like Person 1:N
row.names(act_data) <- NULL

if (is.null(row.names(act_data))) {
  row.names(act_data) <- paste0("Person", 1:N)
}

# === Annnotated heatmap =======================================================

# Factors of in annotation
my_factor <- c("Patient", "Control")

# Create the annotation data.frame for the rows
anno_row <- data.frame(Control = factor(my_factor[control_ids])) %>%
  magrittr::set_rownames(rownames(act_data))

# Create the annotation colours - I like viridis as it is colour-blind friendly
ann_colours <- list(Control = viridis(length(my_factor)) %>%
  magrittr::set_names(my_factor))

# Colour scheme for heatmap
col_pal <- colorRampPalette(c("#146EB4", "white", "#FF9900"))(100)

# Breaks (i.e. vector of values that the colour vector ``col_pal'' is paired with)
my_breaks <- defineDataBreaks(act_data, col_pal, mid_point = 0)

# Create the heatmap
pheatmap(act_data,
  color = col_pal,
  breaks = my_breaks,
  annotation_row = anno_row,
  annotation_colors = ann_colours,
  main = "My annotated data",
  show_rownames = TRUE,
  show_colnames = TRUE
)
