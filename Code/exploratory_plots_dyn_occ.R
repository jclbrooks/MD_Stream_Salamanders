###############################################################
# Exploratory plots for Mid-Atlantic dynamic occupancy model


library(bayesplot)
library(ggplot2)
library(coda)
library(MCMCvis)
library(dplyr)
library(htmlTable)



#DFUS
#class(dfus)
dfus <- readRDS("Results/dfus_mcmc.rds")
dfus_list <- as.mcmc.list(dfus$samples)
# vars <- c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b4", "b5", "b6", "Z_sum[1]", "Z_sum[2]", "Z_sum[3]", "Z_sum[4]", "Z_sum[5]", "Z_sum[6]", "Z_sum[7]", "Z_sum[8]", "Z_sum[9]", "Z_sum[10]", "Z_sum[11]"," Z_sum[12]", "Z_sum[13]",  "Z_sum[14]", "Z_sum[15]", "Z_sum[16]", "Z_sum[17]", "Z_sum[18]", "turnover[2]", "turnover[3]", "turnover[4]", "turnover[5]", "turnover[6]", "turnover[7]", "turnover[8]", "turnover[9]", "turnover[10]", "turnover[11]"," turnover[12]", "turnover[13]",  "turnover[14]", "turnover[15]", "turnover[16]", "turnover[17]", "turnover[18]", "deviance")
# dfus_list <- dfus_list %>%
#   filter(row.names("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b4", "b5", "b6", "Z_sum[1]", "Z_sum[2]", "Z_sum[3]", "Z_sum[4]", "Z_sum[5]", "Z_sum[6]", "Z_sum[7]", "Z_sum[8]", "Z_sum[9]", "Z_sum[10]", "Z_sum[11]"," Z_sum[12]", "Z_sum[13]",  "Z_sum[14]", "Z_sum[15]", "Z_sum[16]", "Z_sum[17]", "Z_sum[18]", "turnover[2]", "turnover[3]", "turnover[4]", "turnover[5]", "turnover[6]", "turnover[7]", "turnover[8]", "turnover[9]", "turnover[10]", "turnover[11]"," turnover[12]", "turnover[13]",  "turnover[14]", "turnover[15]", "turnover[16]", "turnover[17]", "turnover[18]", "deviance"))
#summary <- MCMCsummary(dfus_list, round = 3)

dfus_sum_params <- MCMCsummary(dfus_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5","Z_sum", "turnover", "deviance"))

htmlTable(dfus_sum_params, file = "Results/dfus_parameter_summaries.html")
rm(dfus, dfus_list, dfus_sum_params)



#dmon
dmon <- readRDS("Results/dmon_mcmc.rds")

dmon_list <- as.mcmc.list(dmon$samples)

dmon_sum_params <- MCMCsummary(dmon_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(dmon_sum_params, file = "Results/dmon_parameter_summaries.html")
rm(dmon, dmon_list, dmon_sum_params)



#doch
doch <- readRDS("Results/doch_mcmc.rds")

doch_list <- as.mcmc.list(doch$samples)

doch_sum_params <- MCMCsummary(doch_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(doch_sum_params, file = "Results/doch_parameter_summaries.html")
rm(doch, doch_list, doch_sum_params)



#ebis
ebis <- readRDS("Results/ebis_mcmc.rds")

ebis_list <- as.mcmc.list(ebis$samples)

ebis_sum_params <- MCMCsummary(ebis_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(ebis_sum_params, file = "Results/ebis_parameter_summaries.html")
rm(ebis, ebis_list, ebis_sum_params)



#egut
egut <- readRDS("Results/egut_mcmc.rds")

egut_list <- as.mcmc.list(egut$samples)

egut_sum_params <- MCMCsummary(egut_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(egut_sum_params, file = "Results/egut_parameter_summaries.html")
rm(egut, egut_list, egut_sum_params)



#elon
elon <- readRDS("Results/elon_mcmc.rds")

elon_list <- as.mcmc.list(elon$samples)

elon_sum_params <- MCMCsummary(elon_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(elon_sum_params, file = "Results/elon_parameter_summaries.html")
rm(elon, elon_list, elon_sum_params)



#prub
prub <- readRDS("Results/prub_mcmc.rds")

prub_list <- as.mcmc.list(prub$samples)

prub_sum_params <- MCMCsummary(prub_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(prub_sum_params, file = "Results/prub_parameter_summaries.html")
rm(prub, prub_list, prub_sum_params)



#gpor
gpor <- readRDS("Results/gpor_mcmc.rds")

gpor_list <- as.mcmc.list(gpor$samples)

gpor_sum_params <- MCMCsummary(gpor_list, round = 3, params = c("mean_psi", "mean_p", "a0_p", "a1", "a2", "mu_b0", "sd_b0", "b1", "b2", "b3", "b5", "Z_sum", "turnover", "deviance"))

htmlTable(gpor_sum_params, file = "Results/gpor_parameter_summaries.html")
rm(gpor, gpor_list, gpor_sum_params)










