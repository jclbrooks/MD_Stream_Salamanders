#######################################
### Multi-species Occupancy Model ####
#####################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(jagsUI)

# Read in data set and look at data 
sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)
str(sal)  # structure of the data
dim(sal)  # dimensions of the data
# head(sal) # first couple of lines of data with header

# Rearrange data into long format
df <- sal %>%
  mutate(trans = paste0(stream, "_", transect)) %>%
  group_by(trans, stream, transect, visit) %>%
  tidyr::gather(sp_stage, count, -date, -trans, - stream, -transect, -type, -up_down, -dist, -visit, -time_min, -air, -water, -pH, -DO, -EC, -TDS, -observers) %>%
  tidyr::separate(sp_stage, into = c("species", "stage"), sep = 4) %>%
  filter(species != "tota")
str(df)

# Combine across life stages?

# Convert counts to binary (detection/nondetection)
df$obs <- df$count
df[df$obs > 1 & !is.na(df$obs), "obs"] <- 1
summary(df)

# Reorganize so data in 4D array: site (j) x visit (k) x species (i) x stage (l)
occ <- df %>%
  ungroup() %>%
  group_by(trans, species, stage) %>%
  select(trans, visit, species, stage, obs) %>%
  mutate(visit = paste0("v", visit)) %>%
  spread(visit, obs) # not working
str(occ)
summary(occ)

n_trans <- length(unique(occ$trans))
n_species <- length(unique(df$species)) # I
n_streams <- length(unique(sal$transect)) # J
n_visits <- length(unique(sal$visit)) # K
n_stages <- length(unique(occ$stage)) # L

# Order everything the same way!!!!!!!!!
occ <- occ %>%
  arrange(stage, species, trans)
str(occ)

# need to figure out how to do this
occ_array <- array(NA, dim = c(n_trans, n_visits, n_species, n_stages))

species_observed <- unique(df$species)
transects <- unique(df$trans)
stages <- unique(df$stage)

foo <- occ[which(occ$species == species_observed[1] & occ$stage == stages[1]), c("v1", "v2", "v3", "v4")]

# for(j in 1:n_sites) {
  # for (k in 1:n_visits) {
    for (i in 1:n_species) {
      for (l in 1:n_stages) {
        occ_array[1:n_trans, 1:n_visits, i, l] <- as.matrix(occ[which(occ$species == species_observed[i] & occ$stage == stages[l]), c("v1", "v2", "v3", "v4")])
      }
    }
#   }
# }
str(occ_array)

# print(xtabs(obs ~ trans + visit + species + stage, data = df, na.action = na.pass, addNA = TRUE), na.print = "NA") # doesn't seem to be handling visits and NA correctly
# 
# library(vcd)
# structable(obs ~ trans + visit + species + stage, data = df) # nope

# Read in covariate only data file
cov<-read.csv("RubyToiyabeCovariates_15feb13.csv", header=TRUE) #covariate only file; make sure you first alphabetize by site A-Z in .csv 
head(cov)
str(cov)


# Create species (n=34) and site (n=72) lists
(species.list <- levels(nevada$species))
nspec<-length(species.list)
nspec

(site.list<-levels(nevada$site)) #this is site list of collapsed transects
nsite<-length(site.list)
nsite


# Get covariates and standardize them; note use of second covariate data file
# Site elevation

elev <- cov$elev_GPS
elev
(mean.elev <- mean(elev, na.rm = TRUE))
(sd.elev <- sd(elev, na.rm = TRUE))
elev <- (elev - mean.elev) / sd.elev
elev
elev2<-elev*elev

#Site era; historical = 0; modern = 1
era <-as.factor(cov$era)
era

#Site mtn; Ruby = 0; Toiyabe = 1
mtn <-as.factor(cov$mtn)
mtn

#Trap number and trend vary by sites and rep
trap <-cbind(cov$traps_day1, cov$traps_day2, cov$traps_day3, 
             cov$traps_day4, cov$traps_day5)
trap


trend <-cbind(cov$trend1, cov$trend2, cov$trend3, 
              cov$trend4, cov$trend5)
trend

# Select count data for 5 reps; does not work with NA's; use zero instead
OCC <-cbind(nevada$rep1, nevada$rep2, nevada$rep3, nevada$rep4, nevada$rep5)
OCC

#Convert counts to binary (detection/nondetection)
OCC1 <- OCC # Make a copy 
OCC1[OCC>1] <- 1 #convert counts to binary
OCC1

# Put detection data into 3D array: site (j) x rep (k) x species (i)
A <- array(NA, dim = c(72, 5, 34))
for(i in 1:34){
  A[,,i] <- OCC1[((i-1)*72+1):(i*72),]
}
dimnames(A) <- list(site.list, NULL, species.list)
nsite <- dim(A)[1]
A   #A is now our three dimentional array of observed data



# Number of occupied sites by this species among the 71 sites
tmp <- apply(A, c(1,3), max, na.rm = TRUE)
tmp
(obs.occ <- apply(tmp, 2, sum, na.rm = TRUE))

#convert 3D array to 2D to look at a naive occupancy matrix
NaiveOcc <- matrix(NA, 72, 34)
for(i in 1:34){
  NaiveOcc[,i] <- apply(A[,,i], MARGIN=1, max, na.rm = TRUE) 
}
NaiveOcc


# Get observed species richness; Naive number of occurring species at each site
obs.rich <- apply(NaiveOcc, 1, sum, na.rm = TRUE)
obs.rich
names(obs.rich) <- dimnames(A)[[1]]
(sort(obs.rich))

#BUILD IN TO CHECK THERE ARE 34 SPECIES AT EACH SITE

##################
## CODE FOR JAGS ##
#################

#Bundle data (Note you may need to change nsite, nrep, nspec)
win.data = list(X=A, nsite=72, nrep=5, nspec=34,
                elev=elev, elev2=elev2, era=era, trap=trap, trend=trend, mtn=mtn)


## Write model code (based off of Morgan's Ecology paper and USGS workshop pg. 30)
sink("modelRubyToiyabeMultiSP_10jun14.txt")
cat("
    model {
    
    #PRIORS
    
    
    # Set species loop
    for(i in 1:nspec){
    
    b0[i] ~ dnorm(mu.b0, tau.b0) #Hyperparams
    b1[i] ~ dnorm(mu.b1, tau.b1)
    b2[i] ~ dnorm(mu.b2, tau.b2)
    b3[i] ~ dnorm(mu.b3, tau.b3)
    b4[i] ~ dnorm(mu.b4, tau.b4)
    b5[i] ~ dnorm(mu.b5, tau.b5)
    b6[i] ~ dnorm(mu.b6, tau.b6)
    a0[i] ~ dnorm(mu.a0, tau.a0)
    a1[i] ~ dnorm(mu.a1, tau.a1)
    a2[i] ~ dnorm(mu.a2, tau.a2)
    a3[i] ~ dnorm(mu.a3, tau.a3)
    
    
    }
    
    #HYPERPRIORS
    mu.b0 ~ dnorm(0,0.001)     #Narrower prior to avoid WinBUGS 'undef real result'
    tau.b0 ~ dgamma(0.1, 0.1)
    
    mu.b1 ~ dnorm(0,0.001)
    tau.b1 ~ dgamma(0.1, 0.1)
    
    mu.b2 ~ dnorm(0,0.001)
    tau.b2 ~ dgamma(0.1, 0.1)
    
    mu.b3 ~ dnorm(0,0.001)
    tau.b3 ~ dgamma(0.1, 0.1)
    
    mu.b4 ~ dnorm(0,0.001)
    tau.b4 ~ dgamma(0.1, 0.1)
    
    mu.b5 ~ dnorm(0,0.001)
    tau.b5 ~ dgamma(0.1, 0.1)
    
    mu.b6 ~ dnorm(0,0.001)
    tau.b6 ~ dgamma(0.1, 0.1)
    
    mu.a0 ~ dnorm(0,0.001)
    tau.a0 ~ dgamma(0.1, 0.1)
    
    mu.a1 ~ dnorm(0,0.001)
    tau.a1 ~ dgamma(0.1, 0.1)
    
    mu.a2 ~ dnorm(0,0.001)
    tau.a2 ~ dgamma(0.1, 0.1)
    
    mu.a3 ~ dnorm(0,0.001)
    tau.a3 ~ dgamma(0.1, 0.1)
    
    
    # Estimation of Z matrix (true occurrence for species i at site j)	
    for(i in 1:nspec){	
    for (j in 1:nsite){ 
    logit(psi[j,i]) <-   b0[i]+
    b1[i]*elev[j]+
    b2[i]*elev2[j]+
    b3[i]*era[j]+
    b4[i]*era[j]*elev[j]+
    b5[i]*era[j]*elev2[j]+
    b6[i]*mtn[j]
    Z[j,i] ~ dbern(psi[j,i])
    
    
    # Estimate detection for species i at site j during sampling period k
    for (k in 1:nrep) {
    logit(p[j,k,i]) <- a0[i]+
    a1[i]*era[j]+
    a2[i]*trend[j,k]+
    a3[i]*trap[j,k]
    
    
    
    mu.p[j,k,i] <- p[j,k,i]*Z[j,i]
    X[j,k,i] ~ dbern(mu.p[j,k,i])
    
    }
    }
    }
    
    
    ######Derived quantities #######
    
    
    for(i in 1:nspec){
    occ.sp[i]<-sum(Z[,i])        #Number of occupied sites by this species among the 72
    }
    for(j in 1:nsite){
    occ.site[j]<-sum(Z[j,])      #Number of occurring species at each site
    }                    
    
    }
    
    ",fill=TRUE)
sink()

#INITIAL VALUES
zst<-apply(A, c(1,3), max) #observed occurrence as starting values for Z
zst[is.na(zst)]<-1
inits<-function() 
  list(Z=zst)


#Parameters monitored
params <-c('p', 'Z', 'psi', 'a0','a1', 'a2', 'a3', 'b0','b1', 'b2', 'b3', 'b4', 'b5','b6', 'occ.sp', 'occ.site')


# MCMC settings
saved.per.chain <- 1000
nc <- 3         #num. chains (this is a standard number of chains)
nb <- 200000    #burn-in; draws from Markov chain that are discarded
nt <- 100        #thin chains to save diskspace / reduce autocorrelation among repeated draws
ni <- saved.per.chain*nt + nb  #iterations (draws from posterior dist.)

###################################################################
#Call JAGS from R using rjags and setup to run on multiple cores

#?jags.model

#cl <- makeCluster(3)                       # Request 3 cores
#clusterExport(cl, c("win.data", "inits", "params")) # Make these available
#clusterSetRNGStream(cl = cl, 6572)


#system.time({ # no status bar (% complete) when run in parallel
#  out <- clusterEvalQ(cl, {
#   library(rjags)
#  jm <- jags.model("modelRubyToiyabeMultiSP.txt", win.data, inits, n.adapt="nb", n.chains="nc") # Compile model and run burnin; run burnin = adapt
# ps <- coda.samples(jm, params, n.iter="ni", thin="nt") # Sample from posterior distribution 
#  return(as.mcmc(ps))
#})
#}) 

#?jags.model
#?ClusterExport
#?coda.samples


# Compile model - not run in parallel
#jm.logit <- jags.model("modelRubyToiyabeMultiSP.txt", win.data, inits = inits, n.chains=nc, n.adapt=300000)


#################################################################
#Alternative option to Call JAGS from R using R2JAGS
start.time<-Sys.time()
outj<-jags(data=win.data, inits=inits, parameters.to.save=params, model.file="modelRubyToiyabeMultiSP.txt",
           n.chains=nc, n.iter=ni, n.burnin=nb, n.thin=nt, DIC=FALSE)
end.time=Sys.time()
elapsed.time = difftime(end.time, start.time, units='mins')
elapsed.time





########## LOOKING AT THE OUTPUT ###########
#Has it converged? Rhat should be near 1
#Shows MCMC plots; lines should converge to be fat fuzzy caterpillars
traceplot(outj)



# Results
#ps.list <- mcmc.list(out)
#plot(ps.list)
##summary(ps.list)

#stopCluster(cl)

# Posterior samples when not run in parallel
#ps.logit <- coda.samples(jm.logit, params, n.iter=3000, thin = 3)





#Lookint at results when used R2JAGS
#***Warning: namespace 'rjags' is not available and has been replaced by .GlobalEnv when processing object 'outj'
print(outj, dig=3)
str(outj)
print(outj$BUGSoutput$summary, dig=3)
which(outj$BUGSoutput$summary[,8]>1.1)  #pulls out any Rhat's >1.1 (Rhat should be near 1 for convergence)


# Mean of all the iterations Z=occurrence matrix
print(outj$BUGSoutput$mean$Z, dig=3)    

#Number of occupied sites by this species among the 71
print(outj$BUGSoutput$mean$occ.sp, dig=3)  

#Number of occurring species at each site
print(outj$BUGSoutput$mean$occ.site, dig=3) 



#Shortcuts for calling out data from outj
MeanZ <- outj$BUGSoutput$mean$Z
MeanPsi <- outj$BUGSoutput$mean$psi
P <- outj$BUGSoutput$mean$p
P
#Save output as .csv

Table3 <- t(MeanZ)
colnames(Table3) <- site.list
rownames(Table3) <- species.list
Table3

write.table(Table3, file="C:/R/GreatBasin/ruby_toiyabes/RubyToiyabeMeanZ_10jun14.csv", 
            row.names=species.list, col.names=site.list, sep=",")


#############################################
#Does our naive occupancy data from Matrix A make sense with
# "true" occupancy from Matrix Z?

NaiveOcc  #See how cacluated above

#Make a transposed table to compare NaiveOcc and MeanZ
#rows=species, column1=NaiveOcc Sites, column2=MeanZ Sites
Table1 <- (cbind(dimnames(A)[[3]], t(NaiveOcc), t(MeanZ)))
colnames(Table1) <- c("Species", site.list, site.list)
Table1

#Comparison table of NaiveOcc and Z Matrix
write.table(Table1, 'comparison_10jun14.csv', sep = ",")


## Differences between NaiveOcc and MeanZ ##
round(MeanZ - NaiveOcc, digits=1)  

#Histograms of MeanZ and NaiveOcc
par(mfrow = c(1, 2))
hist(MeanZ, freq = FALSE, probability=TRUE)
hist(NaiveOcc, freq = FALSE)

sum(NaiveOcc > 0)/(72*34) #Overall NaiveOcc (for all sp. all sites)
sum(MeanZ > 0.1)/(72*34)  #Overall Z (for all sp. all sites) #Where did the 0.1 come from???


############################################
#Table of Mean psi
#MeanPsi are an average of conditions specified in the model for a site for a species
#Make a transposed table with rows=species, columns=MeanPsi
Table2 <- t(MeanPsi)
colnames(Table2) <- site.list
rownames(Table2) <- species.list
Table2
range(Table2)  #Range of Mean psi


#Histogram of Mean psi
hist(MeanPsi)
quantile(MeanPsi, c(0.25, 0.5, 0.975))



############################################
##Now let's look at detection
## We can't simply sum across the reps because they are probabilities
## detection probability = 1-(1-p)^t prob. of detct >=1X in t surveys 
## (above eq. is assuming have same p for each t...which we don't)


Prob <- matrix(NA, 34, 71) #Creates a new matrix
for(j in 1:71){
  for(i in 1:34){
    Prob[i, j] <- 1-((1 - P[j,1,i])*(1 - P[j,2,i])*(1 - P[j,3,i])*(1 - P[j,4,i])*(1 - P[j,5,i]))
  }
}
Prob  #These are the detection probabilities at each site

range(Prob) #Range of detection


C:/R/GreatBasin/ruby_toiyabes


###########################################
## Effect of covariates on occupancy

plot(outj$BUGSoutput$mean$b1, 1:34, xlim=c(-8,8), xlab = "Effect size", ylab="Species number", main="Effect of Elevation on Occupancy")
abline(v=0, lwd=1, col="black")
segments(outj$BUGSoutput$sd$b1, 1:34)
outj$BUGSoutput$sd$b1,7, 1:34)

, col="grey", lwd=1)
sig4<-outj$BUGSoutput$sd$b1,3, <0) &
  outj$BUGSoutput$sd$b1,7, <0) + outj$BUGSoutput$sd$b1,3, >0) & 
  outj$BUGSoutput$sd$b1,7, >0)
segments(outj$BUGSoutput$sd$b1,3, [sig4==1]), (1:34)[sig4==1],
outj$BUGSoutput$sd$b1,7, [sig4==1]), (1:34)[sig4==1], col="blue", lwd=2)
































#####################################
#RANDOM CODE


plot(ps.logit, ask=TRUE)
par(mfrow = c(1,1))
plot(ps.logit[["K"]], ask=TRUE)
ps.logit["G[1,1]"]


ps.list[1:10, "K"]
plot(ps.list[,c("K", "p0", "p.can", "b", "n0")]) # traceplots of just certain variables
par(mfrow = c(1,1))

summary(ps.logit)

ps.logit.mat2 <- as.matrix(ps.logit)
logitN2 <- ps.logit.mat2[,grep("Ntot", colnames(ps.logit.mat2))]
plot(colMeans(logitN2))


# Plot posterior summaries of pop size in each year
ps.dep.mat <- as.matrix(ps.dep)
ps.Ntot <- ps.dep.mat[,grep("Ntot", colnames(ps.dep.mat))]
head(ps.Ntot)

plot(1:(nYears+extraYears), colMeans(ps.Ntot), type="b", ylim=c(0, 2000),
     xlab="Year", ylab="Population size", pch=16, col="blue")
points(1:nYears, colSums(N))
arrows(1:(nYears), apply(ps.Ntot, 2, quantile, prob=0.025),
       1:(nYears), apply(ps.Ntot, 2, quantile, prob=0.975),
       angle=90, code=3, length=0.03)
polygon(c(20.5, 35, 35, 20.5, 20.5), c(-100, -100, 3000, 3000, -100),
        col=rgb(0,0,0,0.2), border=FALSE)
legend(5, 2000, c("Actual", "Posterior mean and CI"), pch=c(1,16),
       col=c("black", "blue"))


# MCMC diagnostics
gelman.diag(ps.logit)    # Should be ~ <1.1 (run chains longer if need be) - not sure if this works for list from multipe cores
autocorr.plot(ps.logit)  # If samples are autocorrelated, you need more iters and/or more thinning
crosscorr(ps.logit)      # Correlation among parameters

