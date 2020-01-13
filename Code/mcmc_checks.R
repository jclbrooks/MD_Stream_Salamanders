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
library(readr)

######### Load MCMC Object #########

out <- readRDS("Results/prub_mcmc.rds")

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

#------------- elon ----------------

out <- readRDS("Results/elon_mcmc.rds")

out$summary

samples <- out$samples

p <- mcmc_trace(samples, regex_pars = c("sd"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regex_pars = c("mean_psi"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regex_pars = c("b"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, pars = c("a0_p", "a1", "a2"))
p + facet_text(size = 15)


#------------- prub ----------------

out <- readRDS("Results/prub_mcmc.rds")

out$summary

samples <- out$samples

p <- mcmc_trace(samples, regex_pars = c("sd"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regex_pars = c("mean_psi"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regex_pars = c("b"))
p + facet_text(size = 15)

p <- mcmc_trace(samples, pars = c("a0_p", "a1", "a2"))
p + facet_text(size = 15)



#------------------ potential figures ------------
posterior <- as.matrix(out$samples)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior, regex_pars = c("Z_sum"),
           prob = 0.8) + plot_title







# https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html
# Summarize posteriors with intervals or areas
posterior_1 <- dplyr::select(as.data.frame(posterior), -starts_with("Z"))

plot_title <- ggtitle("Northern Red Salamander (PRUB)")

mcmc_intervals(x = posterior_1, pars = c("mu_b0", "b1", "b2", "b3", "b5")) +
  xlab("Estimate of Effect") +
  ylab("Covariate")+
  ggplot2::scale_y_discrete(labels = c("Autologistic", "Mean Air Temperature", "Slope", "Forest Cover", "Mean Intercept")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=20, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=20, color = "black"),
        axis.text.x = element_text(size = 20, face = "plain", color = "black"),
        axis.text.y = element_text(size = 20, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm")) + plot_title + xlim(-4,4)

mcmc_intervals(x = posterior_1, pars = c("a0_p", "a1", "a2")) +
  xlab("Estimate of Effect") +
  ylab("Covariate")+
  ggplot2::scale_y_discrete(labels = c("Mean Intercept", "Total Precipitation (Previous 7 Days)", "Multi-pass Removal Sampling")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=20, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=20, color = "black"),
        axis.text.x = element_text(size = 20, face = "plain", color = "black"),
        axis.text.y = element_text(size = 20, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm")) + plot_title + xlim(-4,4)

mcmc_areas(posterior_1, pars = c("mu_b0", "b1", "b2", "b3", "b5"), 
           prob = 0.8) +
  xlab("Estimate of Effect") +
  ylab("Covariate")+
  ggplot2::scale_y_discrete(labels = c("Autologistic", "Mean Air Temperature", "Slope", "Forest Cover", "Mean Intercept")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=20, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=20, color = "black"),
        axis.text.x = element_text(size = 20, face = "plain", color = "black"),
        axis.text.y = element_text(size = 20, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm")) + plot_title + xlim(-4,4)

mcmc_areas(posterior_1, pars = c("a0_p", "a1", "a2"),
           prob = 0.8)  +
  xlab("Estimate of Effect") +
  ylab("Covariate")+
  ggplot2::scale_y_discrete(labels = c("Mean Intercept", "Total Precipitation (Previous 7 Days)", "Multi-pass Removal Sampling")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=20, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=20, color = "black"),
        axis.text.x = element_text(size = 20, face = "plain", color = "black"),
        axis.text.y = element_text(size = 20, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm")) + plot_title + xlim(-4,4)


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

ggplot(data = Z_long, aes(year, Z)) + geom_violin(fill = "darkblue") +
  xlab("Year") +
  ylab("Estimated Total Number of Occupied Transects") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=20, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=20, color = "black"),
        axis.text.x = element_text(size = 15, face = "plain", color = "black"),
        axis.text.y = element_text(size = 20, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm")) + plot_title


ggplot(data = Z_long, aes(year, Z)) + geom_boxplot()


#############################

results <- read_csv("Data/Derived/z_sum_turnover_results.csv")
# colnames(results) <- c("covariate", "Northern Red", "Northern Spring", "Long-tailed", "Three-lined", "Northern Two-lined", "Allegheny Mountain Dusky", "Seal", "Northern Dusky")
summary(results)

#Z_sum plot

zsum <- results[1:18,] %>%
  pivot_longer(cols = c('Northern Red', "Northern Spring", "Northern Two-lined", "Long-tailed", "Three-lined", "Allegheny Mountain Dusky", "Seal", "Northern Dusky"), names_to = "Species") %>%
  mutate(covariate = ifelse(covariate == "Z_sum[1]", "2001", covariate),
         covariate = ifelse(covariate == "Z_sum[2]", "2002", covariate),
         covariate = ifelse(covariate == "Z_sum[3]", "2003", covariate),
         covariate = ifelse(covariate == "Z_sum[4]", "2004", covariate),
         covariate = ifelse(covariate == "Z_sum[5]", "2005", covariate),
         covariate = ifelse(covariate == "Z_sum[6]", "2006", covariate),
         covariate = ifelse(covariate == "Z_sum[7]", "2007", covariate),
         covariate = ifelse(covariate == "Z_sum[8]", "2008", covariate),
         covariate = ifelse(covariate == "Z_sum[9]", "2009", covariate),
         covariate = ifelse(covariate == "Z_sum[10]", "2010", covariate),
         covariate = ifelse(covariate == "Z_sum[11]", "2011", covariate),
         covariate = ifelse(covariate == "Z_sum[12]", "2012", covariate),
         covariate = ifelse(covariate == "Z_sum[13]", "2013", covariate),
         covariate = ifelse(covariate == "Z_sum[14]", "2014", covariate),
         covariate = ifelse(covariate == "Z_sum[15]", "2015", covariate),
         covariate = ifelse(covariate == "Z_sum[16]", "2016", covariate),
         covariate = ifelse(covariate == "Z_sum[17]", "2017", covariate),
         covariate = ifelse(covariate == "Z_sum[18]", "2018", covariate) )
zsum$covariate <- factor(zsum$covariate,levels = c("2001", "2002","2003", "2004", "2005", "2006", "2007", "2008",  "2009", "2010", "2011",  "2012",  "2013",  "2014",  "2015",  "2016", "2017",  "2018"))



ggplot(data = zsum, aes(covariate, value, group = Species, fill = Species))  + geom_point(aes(shape = Species, color = Species), size = 4) + geom_line(aes(color = Species)) +
  xlab("Year") + scale_shape_manual(values = c(7,9,10,21,22,23,24,25)) +
  ylab("Estimated Total Number of Occupied Transects") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=17, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=17, color = "black"),
        axis.text.x = element_text(size = 15, face = "plain", color = "black", angle = 90),
        axis.text.y = element_text(size = 15, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "right",
        legend.key.size = unit(2, "line"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) + ylim(0, 200)

#Turnover

turn <- results[19:35,] %>%
  pivot_longer(cols = c('Northern Red', "Northern Spring", "Northern Two-lined", "Long-tailed", "Three-lined", "Allegheny Mountain Dusky", "Seal", "Northern Dusky"), names_to = "Species") %>%
  mutate(value = value *100,
         covariate = ifelse(covariate == "turnover[2]", "2002", covariate),
         covariate = ifelse(covariate == "turnover[3]", "2003", covariate),
         covariate = ifelse(covariate == "turnover[4]", "2004", covariate),
         covariate = ifelse(covariate == "turnover[5]", "2005", covariate),
         covariate = ifelse(covariate == "turnover[6]", "2006", covariate),
         covariate = ifelse(covariate == "turnover[7]", "2007", covariate),
         covariate = ifelse(covariate == "turnover[8]", "2008", covariate),
         covariate = ifelse(covariate == "turnover[9]", "2009", covariate),
         covariate = ifelse(covariate == "turnover[10]", "2010", covariate),
         covariate = ifelse(covariate == "turnover[11]", "2011", covariate),
         covariate = ifelse(covariate == "turnover[12]", "2012", covariate),
         covariate = ifelse(covariate == "turnover[13]", "2013", covariate),
         covariate = ifelse(covariate == "turnover[14]", "2014", covariate),
         covariate = ifelse(covariate == "turnover[15]", "2015", covariate),
         covariate = ifelse(covariate == "turnover[16]", "2016", covariate),
         covariate = ifelse(covariate == "turnover[17]", "2017", covariate),
         covariate = ifelse(covariate == "turnover[18]", "2018", covariate) )
turn$covariate <- factor(turn$covariate,levels = c("2001", "2002","2003", "2004", "2005", "2006", "2007", "2008",  "2009", "2010", "2011",  "2012",  "2013",  "2014",  "2015",  "2016", "2017",  "2018"))



ggplot(data = turn, aes(covariate, value, group = Species, fill = Species))  + geom_point(aes(color = Species, size = 2)) + geom_line(aes(color = Species)) +
  xlab("Year") +
  ylab("Estimated Turnover (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0), color = "black"),
        axis.title.x = element_text(size=17, margin=margin(7,0,0,0), color = "black"),
        axis.title.y = element_text(size=17, color = "black"),
        axis.text.x = element_text(size = 15, face = "plain", color = "black", angle = 90),
        axis.text.y = element_text(size = 15, face = "plain", color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "right",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +ylim(0,25)





