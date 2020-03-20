
# For generic helper functions
library(mdiHelpR)

# For visualisation
library(pheatmap)

# To construct block diagonal matrices
library(Matrix)

# Piping
library(magrittr)

#
makeBlockSymmetrixMatrix <- function(n_block){
  
  n_cum <- cumsum(n_block)
  
  my_mat <- lapply(n_clust, function(x){
    matrix(1, nrow = x, ncol = x)
  }) %>%
    bdiag() %>% 
    as.matrix()
}

# Consistency
set.seed(1)

# Generic descriptions of the data
n             <- 100              # The number of chains
n_entries     <- n * (n-1) / 2    # The number of entries in a triangular matrix
n_clust       <- c(30, 50, 20)    # The number of chains trapped in each mode
n_clust_cum   <- cumsum(n_clust)  # The cumulative sum of chains across modes
K_init        <- 15               # The number of clusters initialised in each chain
n_samples     <- 1000             # The number of samples being clustered

# Arguments for ``pheatmap''
col_pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100)
my_breaks <- defineBreaks(col_pal, lb = 0, ub = 1)

# === Zeroth iteration =========================================================

# Generate n random clusterings and compute the ARI between them
iter_0_mat <- lapply(1:n, function(x){sample(1:K_init, size = n_samples, replace = T)}) %>% 
  unlist() %>% 
  matrix(ncol = n, byrow = T) 

# Save the ARI between each chain
iter_0_ari <- matrix(0, nrow = n, ncol = n)
for(i in 1:(n- 1)){
  for(j in (i+1):n){
    iter_0_ari[i, j] <- iter_0_ari[j, i] <- mcclust::arandi(iter_0_mat[i, ], iter_0_mat[j, ])
  }
}

# To make breaks as nearly all values are << 1
max_ari <- max(iter_0_ari)

# The diagonal should be one!
diag(iter_0_ari) <- 1

min_ari <- min(iter_0_ari)

# Visualise
pheatmap(
  iter_0_ari,
  color = col_pal,
  breaks = my_breaks,
  main = "ARI in zeroth iteration for 100 chains"
)

iter_0_breaks <- defineBreaks(col_pal, lb = min_ari, ub = 1.1 * max_ari)

pheatmap(
  iter_0_ari,
  color = col_pal,
  breaks = iter_0_breaks,
  main = "ARI in zeroth iteration for 100 chains (restricted scales)"
)


# === Final iteration ==========================================================

# In the final iteration we hope to see strong blocks of chains trapped in each 
# of our three modes
iter_n_mat <- bdiag(
  matrix(
    1, nrow = n_clust[1], ncol = n_clust[1]),
    matrix(1, nrow = n_clust[2], ncol = n_clust[2]),
    matrix(1, nrow = n_clust[3], ncol = n_clust[3])
  ) %>%
  as.matrix() 



# Symmetry
iter_n_mat[(n_clust_cum[1]+1):n_clust_cum[2], 1:n_clust_cum[1]] <- iter_n_mat[1:n_clust_cum[1], (n_clust_cum[1]+1):n_clust_cum[2]] <- 0.8
iter_n_mat[(n_clust_cum[2]+1):n_clust_cum[3], n_clust_cum[1]:n_clust_cum[2]] <- iter_n_mat[n_clust_cum[1]:n_clust_cum[2], (n_clust_cum[2]+1):n_clust_cum[3]] <- 0.7
iter_n_mat[(n_clust_cum[2]+1):n_clust_cum[3], 1:n_clust_cum[1]] <- iter_n_mat[1:n_clust_cum[1], (n_clust_cum[2]+1):n_clust_cum[3]] <- 0.5

# Visualise
pheatmap(
  iter_n_mat,
  color = col_pal,
  breaks = my_breaks,
  main = "ARI in final iteration for 100 chains"
)
