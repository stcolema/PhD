#!/usr/bin/env Rscript

# Timing of functions
library(microbenchmark)

library(ggplot2)
library(magrittr)

# Reproducibility
set.seed(1)

# Functions
# Used in benchmarking
check_results <- function(values, tol = sqrt(.Machine$double.eps)) {
  n_values <- length(values)
  equality_check <- c()
  for (i in 1:(n_values - 1)) {
    for (j in (i + 1):n_values) {
      equality_check <- c(equality_check, sum(values[[i]] - values[[j]]))
    }
  }
  sum(equality_check) == 0
}

# === Question 1 ===============================================================

# Floating point accuracy
eps <- 1
x <- 1
while (x + eps != x) eps <- eps / 2
eps / x
x
x + eps

.Machine$double.eps

eps_vec <- c()

for (x in c(1 / 8, 1 / 4, 1 / 2, 1, 2, 4, 8)) {
  eps <- 1
  while (x + eps != x) eps <- eps / 2
  eps_vec <- c(eps_vec, eps)
}

print(eps_vec)

# == Question 2 ================================================================
# Avoiding for loops
X <- matrix(runif(100000), 1000, 100)

z <- rep(0, 1000)
for (i in 1:1000) {
  for (j in 1:100) z[i] <- z[i] + X[i, j]
}

z_1 <- apply(X, 1, sum)
z - z_1

z_2 <- rowSums(X)
z - z_2

# Benchmark competitors
# Define X outside of benchmarking as it is a random sample
X <- matrix(runif(100000),1000,100)

mbm <- microbenchmark(
  "for_loop" = {
    z <- rep(0, 1000)
    for (i in 1:1000) {
      for (j in 1:100) z[i] <- z[i] + X[i, j]
    }
    z
  },
  "apply" = {
    z <- apply(X, 1, sum)
    z
  },
  "rowSums" = {
    z <- rowSums(X)
    z
  },
  check = check_results
)

autoplot(mbm) + 
  labs(
    title = "Benchmark against for loops",
    subtitle = "Summing across rows"
  )

n <- 100000
z <- rnorm(n)
zneg <- 0
j <- 1
for (i in 1:n) {
  if (z[i] < 0) {
    zneg[j] <- z[i]
    j <- j + 1
  }
}


n <- 100000
z <- rnorm(n)
zneg <- 0
j <- 1
for (i in 1:n) {
  if (z[i] < 0) {
    zneg[j] <- z[i]
    j <- j + 1
  }
}

zneg_2 <- z[z < 0]

mbm_2 <- microbenchmark(
  "for_loop" = {
    zneg <- 0
    j <- 1
    for (i in 1:n) {
      if (z[i] < 0) {
        zneg[j] <- z[i]
        j <- j + 1
      }
    }
    zneg
  },
  "vectorised" = {
    zneg <- z[z < 0]
    zneg
  },
  check = check_results
)

autoplot(mbm_2) + 
  labs(
    title = "Benchmark against for loops",
    subtitle = "Recording negative numbers"
  )

# === Question 3 ===============================================================
n <- 1000
A <- matrix(runif(n*n),n,n)
x <- runif(n)

t(x) %*% A %*% x

W <- diag(x)

sum(diag(t(A) %*% W %*% A))


# === Question 4 ===============================================================

# set.seed(0)
n <- 1000
A <- matrix(runif(n*n),n,n)
x.true <- runif(n)
y <- A%*%x.true
A_inv <- solve(A)
x1 <- A_inv %*% y
all.equal(matrix(x.true), x1)
mean(matrix(x.true) - x1)

x2 <- solve(A, y)
all.equal(matrix(x.true), x2)
mean(matrix(x.true) - x2)


mbm_3 <- microbenchmark(
  "explicit" = {
    A_inv <- solve(A)
    x1 <- A_inv %*% y
  },
  "solve" = {
    x2 <- solve(A, y)
  }
  # ,
  # check = check_results
)

autoplot(mbm_3)+ 
  labs(
    title = "Benchmarking",
    subtitle = "Comparison of solve"
  )

# === Question 5 ===============================================================

# Write an R function which takes an un-ordered vector of observations x and 
# returns the values of the empirical c.d.f. for each value, in the order 
# corresponding to the original x vector. (See ?sort.int.)

?sort.int
x <- rnorm(3)
sort(x)
sort.int(x)
order(x)
sort(x) / cumsum(1:length(x))

empirical_cdf <- function(x, plot.cdf = F){
  n <- length(x)
  cdf <- (order(x) - 1) / n
  
  if(plot.cdf){
    plot(sort(x), sort(cdf), xlab = "x", ylab = "cdf", type = "l")
  }
  cdf
}

empirical_cdf(x, plot.cdf = T)

x1 <- rnorm(1000)
empirical_cdf(x1, plot.cdf = T)
x2 <- runif(1000)
empirical_cdf(x2, plot.cdf = T)

# === Question 6 ===============================================================

rb <- function(x, y){
  100 * ((y - x ** 2) ** 2) + (1 - x) ** 2
}


x <- seq(-1.5, 1.5, 0.001)
n <- length(x) - 1
z <- seq(-0.5, 1.5, 2/n)
y <- sapply(x, rb, z)

contour(x, z, z = y)
contour(x, z, z = y, levels = "log10")
