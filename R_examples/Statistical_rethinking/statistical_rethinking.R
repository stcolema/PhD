
library(tidyverse)
library(brms)
library(mvtnorm)
library(loo)

# devtools::install_github("rmcelreath/rethinking",ref="Experimental")
library(rethinking)

# === Data paths and counting ==================================================

# Consider drawing 4 marbles from a bag. The bag is split into navy and white 
# marbles. The possible combinations are captured in the tibble:
d <- tibble(p_1 = 0,
         p_2 = rep(1:0, times = c(1, 3)),
         p_3 = rep(1:0, times = c(2, 2)),
         p_4 = rep(1:0, times = c(3, 1)),
         p_5 = 1)

# We can plot these possible combinations:
d %>% 
  gather() %>% 
  mutate(x = rep(1:4, times = 5),
         possibility = rep(1:5, each = 4)) %>% 
  
  ggplot(aes(x = x, y = possibility, 
             fill = value %>% as.character())) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(.75, 4.25),
                  ylim = c(.75, 5.25)) +
  theme(legend.position = "none")

# Consider the case of 4 marbles present, 1 Navy and 3 white and sampling with
# replacement
tibble(draw            = 1:3,
       marbles         = 4) %>% 
  mutate(possibilities = marbles ^ draw) %>% 
  knitr::kable()

# Describe the possible different paths in a tibble.
d <- tibble(position = c((1:4^1) / 4^0, 
                      (1:4^2) / 4^1, 
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
         fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
           rep(., times = c(4^0 + 4^1 + 4^2)))

# Describe the lines connecting draw 1 to draw 2 for each position
lines_1 <- tibble(x    = rep((1:4), each = 4),
         xend = ((1:4^2) / 4),
         y    = 1,
         yend = 2)

# Describe the lines connecting draw 2 to draw 3 for each position
lines_2 <- tibble(x    = rep(((1:4^2) / 4), each = 4),
         xend = (1:4^3) / (4^2),
         y    = 2,
         yend = 3)

# Plot the possible sampling paths
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values  = c("navy", "white")) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "none")

# In this case the paths are all right-aligned. To centre-align we need to 
# subtract (0.5)/4^(draw - 1) from the position
d <- d %>% 
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position    = position - denominator)

# Similarly correct lines_1 and lines_2
lines_1 <- lines_1 %>% 
  mutate(x    = x - .5,
         xend = xend - .5 / 4^1)

lines_2 <- lines_2 %>% 
  mutate(x    = x - .5 / 4^1,
         xend = xend - .5 / 4^2)

# This looks a little nicer.
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values  = c("navy", "white")) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "none")

# Converting to polar coordinates
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 4) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  theme(panel.grid      = element_blank(),
        legend.position = "none") +
  coord_polar()

# Let us describe the possible paths given we sample "navy", "white", "navy":
lines_1 <- lines_1 %>% 
  mutate(remain = c(rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 3)))

lines_2 <- lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 12 * 4)))

d <- d %>% 
  mutate(remain = c(rep(1:0, times = c(1, 3)),
                    rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 12 * 4))) 

# finally, the plot:
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size  = 1/3) +
  geom_point(aes(fill = fill, alpha = remain %>% as.character()),
             shape = 21, size = 4) +
  # it's the alpha parameter that makes elements semitransparent
  scale_alpha_manual(values = c(1/10, 1)) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  theme(panel.grid      = element_blank(),
        legend.position = "none") +
  coord_polar()

# === Bayesian updating ========================================================

# Create a tibble of tosses of water and land (not sure what this is about)
d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w"))

# Update the tibble to contain the number of observations at each point and the 
# cumulative number of successes
d <-  d %>% 
  mutate(n_trials  = 1:9,
         n_success = cumsum(toss == "w"))




sequence_length <- 50

# Create a plot depicting the change in likelihood as we make more observations.
# The dashed curves are normalized prior densities.
# The solid ones are normalized likelihoods.
d %>% 
  expand(nesting(n_trials, toss, n_success), 
         p_water = seq(from = 0, to = 1, length.out = sequence_length)) %>% 
  group_by(p_water) %>% 
  # you can learn more about lagging here: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lag or here: https://dplyr.tidyverse.org/reference/lead-lag.html
  mutate(lagged_n_trials  = lag(n_trials,  k = 1),
         lagged_n_success = lag(n_success, k = 1)) %>% 
  ungroup() %>% 
  mutate(prior      = ifelse(n_trials == 1, .5,
                             dbinom(x    = lagged_n_success, 
                                    size = lagged_n_trials, 
                                    prob = p_water)),
         likelihood = dbinom(x    = n_success, 
                             size = n_trials, 
                             prob = p_water),
         strip      = str_c("n = ", n_trials)
  ) %>% 
  # the next three lines allow us to normalize the prior and the likelihood, 
  # putting them both in a probability metric 
  group_by(n_trials) %>% 
  mutate(prior      = prior      / sum(prior),
         likelihood = likelihood / sum(likelihood)) %>%   
  
  # plot!
  ggplot(aes(x = p_water)) +
  geom_line(aes(y = prior), linetype = 2) +
  geom_line(aes(y = likelihood)) +
  scale_x_continuous("proportion water", breaks = c(0, .5, 1)) +
  scale_y_continuous("plausibility", breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~strip, scales = "free_y")


# Given 6 out of 9 observations coming up water, the most likely probability is 
tibble(prob = seq(from = 0, to = 1, by = .01)) %>% 
  ggplot(aes(x = prob,
             y = dbinom(x = 6, size = 9, prob = prob))) +
  geom_line() +
  labs(x = "probability",
       y = "binomial likelihood") +
  theme(panel.grid = element_blank())
