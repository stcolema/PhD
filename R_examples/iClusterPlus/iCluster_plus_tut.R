#!/usr/bin/env Rscript

# Tutorial from iCLuster+ package (see iClusterPlus::iManual())

library(iClusterPlus)
library(GenomicRanges)
library(cluster) # for adaptive thresholding
library(gplots)
library(lattice)
library(dplyr)

# The glioblastoma data from TGCNA
data(gbm)
dim(gbm.mut)

summary(gbm.mut)

mut.rate <- apply(gbm.mut, 2, mean)
gbm.mut2 <- gbm.mut[, which(mut.rate > 0.02)]
gbm.mut2[1:10, 1:8]

# For gene expression data, we recommend using the top variable genes for
# integrative clustering analysis, which can be obtained by variance filtering.
# For example, we use the top 1740 genes for our iCluster analysis.

# the rows and columns corresponding to the samples and genes respectively
gbm.exp[1:3, 1:8]

# It is a challenge to incorporate raw or normalized copy number data for
# iCluster analysis considering the high dimensionality and spatial correlation
# of the data. Based on our experience, we think it is more feasible to use the
# segmentation results produced by the DNAcopy package.
dim(gbm.seg)
gbm.seg[1:3, ]

# gbm.cn is the segmentation results produced by DNAcopy

# We reduce the GBM copy number regions to 5K by removing the redundant regions
# using function CNregions.
data(variation.hg18.v10.nov.2010)
gbm.cn <- CNregions(
  seg = gbm.seg,
  epsilon = 0,
  adaptive = FALSE,
  rmCNV = TRUE,
  cnv = variation.hg18.v10.nov.2010[, 3:5],
  frac.overlap = 0.5,
  rmSmallseg = TRUE, nProbes = 5
)

gbm.cn <- gbm.cn[order(rownames(gbm.cn)), ]

gbm.cn.adapat <- CNregions(
  seg = gbm.seg,
  epsilon = 0,
  adaptive = TRUE,
  rmCNV = TRUE,
  cnv = variation.hg18.v10.nov.2010[, 3:5],
  frac.overlap = 0.5,
  rmSmallseg = TRUE,
  nProbes = 5
)

# check if all the samples are in the same order for the three data sets
all(rownames(gbm.cn) == rownames(gbm.exp))
all(rownames(gbm.cn) == rownames(gbm.mut2))



n_clusters <- 3
n_iter <- 10
data_types <- c("binomial", "gaussian", "gaussian")
lambdas <- c(0.04, 0.61, 0.90)

fit.single <- iClusterPlus(
  dt1 = gbm.mut2,
  dt2 = gbm.cn,
  dt3 = gbm.exp,
  type = data_types,
  lambda = lambdas,
  K = n_clusters - 1,
  maxiter = n_iter
)

set.seed(1)
date()
for(k in 1:5){
  cv.fit = tune.iClusterPlus(
    cpus=3,
    dt1=gbm.mut2,
    dt2=gbm.cn,
    dt3=gbm.exp,
    type=c("binomial","gaussian","gaussian"),
    K=k,
    n.lambda=185, 
    scale.lambda=c(1,1,1),
    maxiter=20
  )
  save(cv.fit, file=paste("cv.fit.k",k,".Rdata",sep=""))
}
date()

output=alist()
files=grep("cv.fit",dir())
for(i in 1:length(files)){
  load(dir()[files[i]])
  output[[i]]=cv.fit
}
nLambda = nrow(output[[1]]$lambda)
nK = length(output)
BIC = getBIC(output)
devR = getDevR(output)


minBICid <- apply(BIC, 2, which.min)
devRatMinBIC <- rep(NA, nK)
for (i in 1:K) {
  devRatMinBIC[i] <- devR[minBICid[i], i]
}

plot(1:(K + 1),
     c(0, devRatMinBIC),
     type = "b",
     xlab = "Number of clusters (K+1)",
     ylab = "% Explained Variation"
)

clusters <- getClusters(output)
rownames(clusters) <- rownames(gbm.exp)
colnames(clusters) <- paste("K=", 2:(length(output) + 1), sep = "")
# write.table(clusters, file="clusterMembership.txt",sep='\t',quote=F)
k <- 2
best.cluster <- clusters[, k]
best.fit <- output[[k]]$fit[[which.min(BIC[, k])]]