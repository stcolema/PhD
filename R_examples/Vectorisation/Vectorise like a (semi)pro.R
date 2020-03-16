#!/usr/env/bin/Rscript

# Tutorial in funcitonal programming from TLP
# https://www.data-imaginist.com/2020/vectorizing-like-a-semi-pro/

set.seed(1)

library(ambient) # devtools::install_github('thomasp85/ambient')
library(dplyr)

z <- long_grid(1:100, 1:100) %>% 
  mutate(val = gen_simplex(x, y, frequency = 0.02)) %>% 
  as.matrix(val)

image(z, useRaster = TRUE)

# Find extrema in data
extrema <- function(z, neighbors = 2) {
  ind <- seq_along(z)
  rows <- row(z)
  cols <- col(z)
  n_rows <- nrow(z)
  n_cols <- ncol(z)
  window_offsets <- seq(-neighbors, neighbors)
  window <- outer(window_offsets, window_offsets * n_rows, `+`)
  window_row <- rep(window_offsets, length(window_offsets))
  window_col <- rep(window_offsets, each = length(window_offsets))
  windows <- mapply(function(i, row, col) {
    row <- rows + row
    col <- cols + col
    new_ind <- ind + i
    new_ind[row < 1 | row > n_rows | col < 1 | col > n_cols] <- NA
    z[new_ind]
  }, i = window, row = window_row, col = window_col, SIMPLIFY = FALSE)
  windows <- c(windows, list(na.rm = TRUE))
  minima <- do.call(pmin, windows) == z
  maxima <- do.call(pmax, windows) == z
  extremes <- matrix(0, ncol = n_cols, nrow = n_rows)
  extremes[minima] <- -1
  extremes[maxima] <- 1
  extremes
}