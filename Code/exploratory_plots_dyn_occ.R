###############################################################
# Exploratory plots for Mid-Atlantic dynamic occupancy model


library(bayesplot)
library(ggplot2)
library(coda)
library(MCMCvis)
library(dplyr)


dfus <- readRDS("Results/Dan_results/dfus_mcmc.rds")
dmon <- readRDS("Results/Dan_results/dmon_mcmc.rds")
doch <- readRDS("Results/Dan_results/doch_mcmc.rds")
ebis <- readRDS("Results/Dan_results/ebis_mcmc.rds")
#egut <- readRDS("Results/gpor_mcmc.rds")
#elon <- readRDS("Results/gpor_mcmc.rds")
#prub <- readRDS("Results/gpor_mcmc.rds")
gpor <- readRDS("Results/gpor_mcmc.rds")

class(dfus)
dfus_list <- as.mcmc.list(dfus$samples)
vars <- c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b4", "b5", "b6", "Z_sum[1]", "Z_sum[2]", "Z_sum[3]", "Z_sum[4]", "Z_sum[5]", "Z_sum[6]", "Z_sum[7]", "Z_sum[8]", "Z_sum[9]", "Z_sum[10]", "Z_sum[11]"," Z_sum[12]", "Z_sum[13]",  "Z_sum[14]", "Z_sum[15]", "Z_sum[16]", "Z_sum[17]", "Z_sum[18]", "turnover[2]", "turnover[3]", "turnover[4]", "turnover[5]", "turnover[6]", "turnover[7]", "turnover[8]", "turnover[9]", "turnover[10]", "turnover[11]"," turnover[12]", "turnover[13]",  "turnover[14]", "turnover[15]", "turnover[16]", "turnover[17]", "turnover[18]", "deviance")
dfus_list <- dfus_list %>%
  filter(row.names("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b4", "b5", "b6", "Z_sum[1]", "Z_sum[2]", "Z_sum[3]", "Z_sum[4]", "Z_sum[5]", "Z_sum[6]", "Z_sum[7]", "Z_sum[8]", "Z_sum[9]", "Z_sum[10]", "Z_sum[11]"," Z_sum[12]", "Z_sum[13]",  "Z_sum[14]", "Z_sum[15]", "Z_sum[16]", "Z_sum[17]", "Z_sum[18]", "turnover[2]", "turnover[3]", "turnover[4]", "turnover[5]", "turnover[6]", "turnover[7]", "turnover[8]", "turnover[9]", "turnover[10]", "turnover[11]"," turnover[12]", "turnover[13]",  "turnover[14]", "turnover[15]", "turnover[16]", "turnover[17]", "turnover[18]", "deviance"))
#summary <- MCMCsummary(dfus_list, round = 3)

dfus_sum_params <- MCMCsummary(dfus_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b4", "b5", "b6", "Z_sum", "turnover", "deviance"))

# "Z_sum[1]", "Z_sum[2]", "Z_sum[3]", "Z_sum[4]", "Z_sum[5]", "Z_sum[6]", "Z_sum[7]", "Z_sum[8]", "Z_sum[9]", "Z_sum[10]", "Z_sum[11]"," Z_sum[12]", "Z_sum[13]",  "Z_sum[14]", "Z_sum[15]", "Z_sum[16]", "Z_sum[17]", "Z_sum[18]", "turnover[2]", "turnover[3]", "turnover[4]", "turnover[5]", "turnover[6]", "turnover[7]", "turnover[8]", "turnover[9]", "turnover[10]", "turnover[11]"," turnover[12]", "turnover[13]",  "turnover[14]", "turnover[15]", "turnover[16]", "turnover[17]", "turnover[18]"


#htmlTable(summary_params, file = "Results/parameter_summaries.html")


mcmc_intervals(dfus_sum_params, pars = c("b1", "b2", "b3", "b4", "b5"))
#"b1", "b2", "b3", "b4", "b5"















