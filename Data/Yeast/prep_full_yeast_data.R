#!/usr/bin/env Rscript

# BiocManager::install("STRINGdb")
library(STRINGdb)

# Standardise the yeast cell-cycle data
my_data <- read.csv("./Data/Yeast/marina_alpha_orfs.tsv", sep = "\t", row.names = 1)
scaled_data <- scale(my_data)
write.csv(scaled_data, file = "./Data/Yeast/scaled_full_yeast_timepoint_data.csv")

# PPI data
ppi_data <- read.table("./Data/Yeast/string_ppi.txt", header = T)
str(ppi_data)


get_proteins()
