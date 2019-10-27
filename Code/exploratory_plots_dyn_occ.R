###############################################################
# Exploratory plots for Mid-Atlantic dynamic occupancy model


library(bayesplot)
library(ggplot2)


dfus <- readRDS("Results/Dan_results/dfus_mcmc.rds")
dmon <- readRDS("Results/Dan_results/dmon_mcmc.rds")
doch <- readRDS("Results/Dan_results/doch_mcmc.rds")
ebis <- readRDS("Results/Dan_results/ebis_mcmc.rds")
#egut <- readRDS("Results/gpor_mcmc.rds")
#elon <- readRDS("Results/gpor_mcmc.rds")
#prub <- readRDS("Results/gpor_mcmc.rds")
gpor <- readRDS("Results/gpor_mcmc.rds")

dim(dfus)
dfus_array <- as.array(dfus)
dim(dfus_array)
mcmc_intervals(dfus_array, pars = c("b1", "b2", "b3", "b4", "b5"))
















