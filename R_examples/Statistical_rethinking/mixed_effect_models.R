

# install.packages(c("devtools","mvtnorm","loo","coda"),dependencies=TRUE)
# devtools::install_github("rmcelreath/rethinking",ref="Experimental")

library(mvtnorm)
library(loo)
library(coda)
library(rethinking)
library(tidyverse)
library(ggthemes) 

# Reed in data
data(reedfrogs)
d <- reedfrogs %>% 
  mutate(tank = 1:nrow(.))

rm(reedfrogs)
detach(package:rethinking, unload = T)
library(brms)


# Go ahead and acquaint yourself with the reedfrogs.
d %>%
  glimpse()

# Here’s the formula for the un-pooled model in which each tank gets its own intercept:
  # survi∼Binomial(ni,pi)logit(pi)=αtankiαtank∼Normal(0,5),
# where ni is indexed by the density column. It’s values are distributed like so:
  
d %>% 
  count(density)

# Visualise this for ease sake
d %>% 
  ggplot(aes(x = as.factor(density))) +
  geom_bar()

# Fit a simple model
b12.1 <- brm(data = d,
      family = binomial,
      surv | trials(density) ~ 0 + factor(tank),
      prior(normal(0, 5), class = b),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 12,
      file = "./R_examples/Statistical_rethinking/fits/b12.01"
      )


print(b12.1)


tibble(estimate = fixef(b12.1)[, 1]) %>% 
  mutate(p = inv_logit_scaled(estimate)) %>% 
  gather() %>% 
  mutate(key = if_else(key == "p", "expected survival probability", "expected survival log-odds")) %>% 
  ggplot(aes(x = value, fill = key)) +
  geom_density(size = 0) +
  scale_fill_manual(values = c("orange1", "orange4")) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Tank-level intercepts from the no-pooling model",
       subtitle = "Notice now inspecting the distributions of the posterior means can offer insights you\nmight not get if you looked at them one at a time.") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~key, scales = "free")
