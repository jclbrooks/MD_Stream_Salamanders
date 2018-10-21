

library(dplyr)
library(tidyr)
library(ggplot2)
library(jagsUI)

sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)

doch_adult <- sal %>%
  group_by(stream, transect, visit) %>%
  select(stream, transect, visit, DOCHA) %>%
  spread(visit, value = DOCHA)

summary(doch_adult)

upstream <- sal %>%
  group_by(stream, transect) %>%
  select(stream, transect, up_down, type) %>%
  distinct() %>%
  mutate(up = ifelse(type == "res" & up_down == "UP", 1, 0))

downstream <- sal %>%
  group_by(stream, transect) %>%
  select(stream, transect, up_down, type) %>%
  distinct() %>%
  mutate(down = ifelse(type == "res" & up_down == "DOWN", 1, 0))

testing <- TRUE
if(testing) {
  nb = 5000
  ni = 5000 + nb
  nt = 1
} else {
  nb = 10000
  ni = 40000
  nt = 10
}

n_transects <- nrow(doch_adult[ , 3:6])
n_streams <- length(unique(sal$stream))
n_visits <- 4
  
doch_adult_data <- list(Count = as.matrix(doch_adult[ , 3:6]), 
                     n_transects = n_transects, 
                     n_visits = n_visits,
                     n_streams = n_streams, 
                     stream = as.numeric(as.factor(doch_adult$stream)), 
                     up = upstream$up, 
                     down = downstream$down)

Nst <- apply(doch_adult[ , 3:6], 1, function(x) max(x, na.rm = TRUE)) + 1

##### min model deterministic p #####
inits <- function(){
  list(N = Nst,
       a0 = rnorm(1, 0, 1),
       a1 = rnorm(1, -1, 0.5),
       a2 = rnorm(1, 0, 0.5),
       b0 = rnorm(1, 0, 0.5),
       Z_N = rnorm(9, 0, 1),
       sigma_stream = runif(1, 0, 1),
       sigma_p = runif(1, 0, 1))
}

params <- c( "a0", 
             "a1", 
             "a2",
             "sigma_stream",
             "b0",
             "N",
             "p",
             "Z_N")

out <- jags(data = doch_adult_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/min.txt",
            n.chains = 3,
            # n.adapt = 100,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = 3)

# Results
out
plot(out, parameters = c("a0", 
                         "a1", 
                         "a2",
                         "sigma_stream",
                         "b0"))

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS", recursive = TRUE)
save(out, file = "Results/JAGS/doch_adult_mcmc.RData")
# saveRDS(pjor_od, file = "Results/JAGS/pjor_mcmc_out.rds")


##### Min model with overdispersion in p #####
inits <- function(){
  list(N = Nst,
       a0 = rnorm(1, 0, 1),
       a1 = rnorm(1, -1, 0.5),
       a2 = rnorm(1, 0, 0.5),
       b0 = rnorm(1, 0, 0.5),
       Z_N = rnorm(9, 0, 1),
       sigma_stream = runif(1, 0, 1),
       sigma_p = runif(1, 0, 1))
}

params <- c( "a0", 
             "a1", 
             "a2",
             "sigma_stream",
             "b0",
             "sigma_p",
             "N",
             "p",
             "Z_N",
             "Z_p")

out <- jags(data = doch_adult_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/min_od.txt",
            n.chains = 3,
            # n.adapt = 100,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = 3)

# Results
out
plot(out, parameters = c("a0", 
                         "a1", 
                         "a2",
                         "sigma_stream",
                         "b0",
                         "sigma_p"))

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS", recursive = TRUE)
save(out, file = "Results/JAGS/doch_adult_mcmc.RData")
# saveRDS(pjor_od, file = "Results/JAGS/pjor_mcmc_out.rds")



