
if(! requireNamespace("mcclust.ext")){
  devtools::install_github("sarawade/mcclust.ext")
}

library(mcclust.ext)
library(data.table)
library(magrittr)
library(ggplot2)

data(galaxy.fit)
x <- data.frame(x = galaxy.fit$x)
data(galaxy.pred)
data(galaxy.draw)

# Find representative partition of posterior
# Variation of Information (minimizes lower bound to VI)
psm <- comp.psm(galaxy.draw)
galaxy.VI <- minVI(psm, galaxy.draw, method = ("all"), include.greedy = TRUE)
summary(galaxy.VI)

plot(galaxy.VI,
  data = x,
  dx = galaxy.fit$fx,
  xgrid = galaxy.pred$x,
  dxgrid = galaxy.pred$fx
)

# Compute Variation of Information
VI(galaxy.VI$cl, galaxy.draw)

# Binder
galaxy.B <- minbinder.ext(psm, galaxy.draw, method = ("all"), include.greedy = TRUE)
summary(galaxy.B)

plot(galaxy.B,
  data = x,
  dx = galaxy.fit$fx,
  xgrid = galaxy.pred$x,
  dxgrid = galaxy.pred$fx
)

# Uncertainty in partition estimate
galaxy.cb <- credibleball(galaxy.VI$cl[1, ], galaxy.draw)
summary(galaxy.cb)

plot(galaxy.cb,
  data = x,
  dx = galaxy.fit$fx,
  xgrid = galaxy.pred$x,
  dxgrid = galaxy.pred$fx
)

# Compare with uncertainty in heat map of posterior similarity matrix
plotpsm(psm)


mdi_data <- fread("~/Desktop/MDI_more_clusters/out_seed_1.csv")

N <- 551
p_rand <- 6

d1 <- mdi_data[, (p_rand + 1) : (p_rand + N) ] %>% 
  as.matrix()

d2 <- mdi_data[, (p_rand + N + 1) : (p_rand + 2 * N) ] %>% 
  as.matrix()

d3 <- mdi_data[, (p_rand + 2 * N + 1) : (p_rand + 3 * N) ] %>% 
  as.matrix()

n_samples <- d3 %>% nrow()

psm <- comp.psm(d1)
d1_VI <- minVI(psm, d1, method = ("all"), include.greedy = TRUE)
d1_cb <- credibleball(d1_VI$cl[1, ], d1)

# cb_df <- data.frame(Upper_vert = d1_cb$c.uppervert, Lower_vert = d1_cb$c.lowervert, MCMC_sample = 1:n_samples)
# cb_df$
# 
# ggplot(data = cb_df, aes(x = MCMC_sample)) +
#   geom_line(aes(y = Upper_vert)) +
#   geom_line(aes(y = Lower_vert))
# 
