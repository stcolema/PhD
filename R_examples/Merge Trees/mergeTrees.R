
library(mergeTrees)

ge_data <- read.csv("~/PhD/Year_1/Consensus_inference/ra_chris_wallace/Data/BCC_data/MDI_data/MDI_Expression_data.csv",
                    row.names = 1)
meth_data <- read.csv("~/PhD/Year_1/Consensus_inference/ra_chris_wallace/Data/BCC_data/MDI_data/MDI_Methylation_data.csv",
                      row.names = 1)

miRNA_data <- read.csv("~/PhD/Year_1/Consensus_inference/ra_chris_wallace/Data/BCC_data/MDI_data/MDI_miRNA_data.csv",
         row.names = 1)
protein_data <- read.csv("~/PhD/Year_1/Consensus_inference/ra_chris_wallace/Data/BCC_data/MDI_data/MDI_Protein_data.csv",
         row.names = 1)

my_data <- list(ge_data,
                meth_data,
                miRNA_data,
                protein_data)


ge_dist <- dist(ge_data)
meth_dist <- dist(meth_data)
miRNA_dist <- dist(miRNA_data)
protein_dist <- dist(protein_data)

dist_data <- lapply(my_data, dist)

dist_data[[1]][1:3, 1:3]

hc_list <- lapply(dist_data, hclust)

merge_tree <- mergeTrees(hc.list = hc_list, standardize = F)


plot(merge_tree)

abline(h = 41)

pred <- cutree(merge_tree, k =4)



ccm <- mcclust::cltoSim(pred)
pheatmap::pheatmap(ccm)
