########## Load Libraries ##########
library(coda)
library(rjags)
# library(devtools)
# install_github("https://github.com/stan-dev/bayesplot") # if want latest development version of bayesplot)
library(ggplot2)
library(tidyr)
library(bayesplot)
library(tidybayes)
library(dplyr)

######### Load MCMC Object #########

out <- readRDS("Results/dfus_msmc.rds")

out$summary

samples <- out$samples

######### Check MCMC ########

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(samples, regex_pars = c("sd"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regex_pars = c("mean_psi"))
p + facet_text(size = 15)

# p <- mcmc_trace(samples, regex_pars = c("alpha1"))
# p + facet_text(size = 15)

p <- mcmc_trace(samples, regex_pars = c("b"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, pars = c("a0_p", "a1", "a2"))
p + facet_text(size = 15)

# p <- mcmc_trace(samples, regex_pars = c("Z_sum")) # too big I think
# p + facet_text(size = 15)


posterior <- as.matrix(out$samples)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior, regex_pars = c("Z_sum"),
           prob = 0.8) + plot_title

posterior_Z <- select(as.data.frame(posterior), starts_with("Z_sum"))

# pivot_longer and make specific years then plot
Z_long <- as.data.frame(posterior) %>%
  select(starts_with("Z_sum")) 

colnames(Z_long) <- 2001:2018

# broke this
Z_long <- Z_long %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "year",
               values_to = "Z")

ggplot(data = Z_long, aes(year, Z, fill = "blue")) + geom_violin()
ggplot(data = Z_long, aes(year, Z)) + geom_boxplot()
