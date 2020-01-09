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

# Run different blocks
run_prep_material <- F

# === Prep material ============================================================

# === Question 1 ===============================================================

if (run_prep_material) {
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
  X <- matrix(runif(100000), 1000, 100)

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
  A <- matrix(runif(n * n), n, n)
  x <- runif(n)

  t(x) %*% A %*% x

  W <- diag(x)

  sum(diag(t(A) %*% W %*% A))


  # === Question 4 ===============================================================

  # set.seed(0)
  n <- 1000
  A <- matrix(runif(n * n), n, n)
  x.true <- runif(n)
  y <- A %*% x.true
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

  autoplot(mbm_3) +
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

  empirical_cdf <- function(x, plot.cdf = F) {
    n <- length(x)
    cdf <- (order(x) - 1) / n

    if (plot.cdf) {
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

  rb <- function(x, y) {
    100 * ((y - x**2)**2) + (1 - x)**2
  }


  rb.grad <- function(x, y) {
    df_dx <- 200 * (2 * x**3 - 2 * x * y) + 2 * (x - 1)
    df_dy <- 200 * (y - x**2)
    c(df_dx, df_dy)
  }

  approx.rb.grad <- function(x, y, delta = 1e-7) {
    df_dx <- (rb(x + delta, y) - rb(x, y)) / delta
    df_dy <- (rb(x, y + delta) - rb(x, y)) / delta
    c(df_dx, df_dy)
  }

  approx.rb.hessian <- function(x, y, delta = 1e-7) {
    df_dx <- (rb.grad(x + delta, y) - rb.grad(x, y)) / delta
    df_dy <- (rb.grad(x, y + delta) - rb.grad(x, y)) / delta
    matrix(c(df_dx, df_dy), nrow = 2)
  }

  rb.hessian <- function(x, y) {
    d2f_dx2 <- 400 * (3 * x**2 - y) + 2
    d2f_dy2 <- 200
    d2f_dxdy <- -400 * x
    matrix(c(d2f_dx2, d2f_dxdy, d2f_dxdy, d2f_dy2), nrow = 2)
  }

  rb.taylor.expansion <- function(x, y) {
    rb(x, y) + sum(rb.grad(x, y), 0.5 * rb.hessian(x, y))
  }

  x <- seq(-1.5, 1.5, 0.01)
  n <- length(x) - 1
  z <- seq(-0.5, 1.5, 2 / n)
  y <- sapply(x, rb, z)

  plt_1 <- contour(x, z, z = y)
  new_point <- rb.taylor.expansion(0.6, 0.6)
  contour(new_point, add = T)

  # contour(x, z, z = y, levels = "log10")

  z_lim <- range(y, finite = T)
  pretty(z_lim, 10)
  max(y)

  contour(log10(y))


  rb.hessian(4, 4)
  approx.rb.hessian(4, 4)
}

# === Computer Lab 1 ===========================================================

# === Question 1: SVD ==========================================================

pcScoresEig <- function(X) {
  eig <- eigen(var(X), symmetric = TRUE)
  Xc <- sweep(as.matrix(X), 2, colMeans(X))
  Xc %*% eig$vectors
}

# We can test it on the infamous iris data, as follows.
Xi <- iris[, -5]
scores <- pcScoresEig(Xi)
plot(scores[, 1], scores[, 2], col = iris[, 5], pch = 19)

# Equivalent to princomp function in R
head(scores, 3)
head(princomp(Xi)$scores, 3)

svd(Xi)

pcScoresSvd <- function(X, mean_centre = T, scale = F) {

  # Mean centre the data (enable scaling)
  X_new <- scale(X, center = mean_centre, scale = scale)

  # Singular value decomposition
  svd_out <- svd(X_new)

  # Signular values of X in a diagonal matrix
  D <- diag(svd_out$d)

  # Left singular vectors of X
  U <- svd_out$u

  out <- list()

  # The product of these is the PCA scores of X
  out$scores <- U %*% D

  # The variance explained is the normalised squared singular values
  out$var <- D**2 / sum(D**2)

  out
}

my_scores <- pcScoresSvd(Xi)
my_prcomp <- prcomp(Xi)

n_row <- 5000
n_col <- 1000
test_data <- runif(n_row * n_col, min = -1000, max = 1000) %>%
  matrix(nrow = n_row, ncol = n_col)


mbm_pca <- microbenchmark(
  # "prcomp" = {
  #   prcomp(test_data)$x
  # },
  "eigen_decomposition" = {
    pcScoresEig(test_data)
  },
  # "princomp" = {
  #   princomp(test_data)$scores
  # },
  "svd" = {
    pcScoresSvd(test_data)$scores
  },
  times = 3
)

autoplot(mbm_pca) +
  labs(
    title = "PCA methods"
  )


# === 2. Ridge regression ======================================================

# y = XB + e
# L(B) = ||e||^2 + l||B||^2
#
# Subsitute for e and differentiate
# Second, substitute in X = UDV^T
# Realise that I = V^TV = VV^T due to orthogonal nature of V

# Ridge regression using SVD
ridge <- function(y, X, lambda) {

  # Mean centre X to avoid penalising the intercept
  X_centred <- scale(X, center = T, scale = F)

  # Apply SVD
  svd_out <- svd(X_centred)

  # Calculate the values of (D**2 - lambda I)**-1 %*% D for each lambda
  d_l <- sapply(svd_out$d, function(x, lambda) {
    x / (x**2 + lambda)
  }, lambda)

  # Calculate the Betas
  my_values <- apply(d_l, 1, function(d, V, U, y) {
    V %*% diag(d) %*% t(U) %*% y
  }, svd_out$v, svd_out$u, y) %>%
    t()

  my_values
}

treed_ridge <- ridge(trees[, 3], trees[, 1:2], c(0, exp(0:5)))
lm(Volume ~ Girth + Height, trees)


# === Question 3: ==============================================================

## First, simulate the data
set.seed(1)
x <- sort(runif(100)) + 100
y <- .2 * (x - 100 - .5) + (x - 100 - .5)^2 + rnorm(100) * .1

## lm manages to estimate the regression
plot(x, y)
b <- lm(y ~ x + I(x^2))
lines(x, fitted(b), lwd = 2)

## direct solution of the normal equations doesn't work
X <- model.matrix(b)
beta.hat <- solve(t(X) %*% X, t(X) %*% y)

## What if the x-values were even further away from zero?
## In theory, this should still be able to represent the quadratic function.
x1 <- x + 1000
plot(x, y)
b1 <- lm(y ~ x1 + I(x1^2))
lines(x, fitted(b1), lwd = 2)
## lm fails without warning!

kappa(x1)
kappa(X)
