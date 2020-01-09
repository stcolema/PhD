#!/usr/bin/env Rscript

# http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/seriation/seriation.pdf

# First, we load the package seriation and the iris data set. We remove the 
# species classification and reorder the objects randomly since they are already
# sorted by species in the data set. Then we calculate the Euclidean distances 
# between objects.

library("seriation")

data("iris") # the infamous Iris data

x <- as.matrix(iris[-5])
x <- x[sample(seq_len(nrow(x))), ]
d <- dist(x)

# To seriate the objects given the dissimilarities, we just call seriate() with 
# the default settings.
order <- seriate(d)
order

#> object of class 'ser_permutation', 'list'
#> contains permutation vectors for 1-mode data
#> 
#> vector length seriation method
#> 1           150         Spectral

# To visually inspect the effect of seriation on the distance matrix, we use 
# matrix shading with pimage() (the result is shown in Figure 2).
pimage(d, main = "Random")
pimage(d, order, main = "Reordered")

# Finally, we can also compare the improvement for different loss functions 
# using criterion().
cbind(random = criterion(d), reordered = criterion(d, order)) 
# reordered better under all criteria

# To use the result of the seriation for the original two-mode data, we have to 
# add a permutation vector to the ser_permutation object. To leave the columns 
# in the original order, we add an identity permutation vector to the
# permutations object using the combine function c().
pimage(x, main = "Random")
order_2mode <- c(order, ser_permutation(seq_len(ncol(x))))
order_2mode
pimage(x, order_2mode, main = "Reordered")
