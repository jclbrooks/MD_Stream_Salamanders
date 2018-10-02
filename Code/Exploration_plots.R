###############################
# Exploratory plots of salamander and water chemistry
# J. Brooks
###############################

# load packages
library(ggplot2)
library(dplyr)
library(stringr)


# read in data
sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)
counts <- read.csv("Data/Just_Count_Data.csv", stringsAsFactors = FALSE)


head(sal)
str(sal)
summary(sal)

# attach(sal)
pairs(sal[ ,c(20,23,24,25,27,28)])

plot(total ~ air, data = sal)

# Add reference to the up, down
sal <- sal %>%
  mutate(type = ifelse(Type == "res", up_down, type))
str(sal)

g1 <- ggplot(sal, aes(stream, total, order("POPLICKTRIB","ELKLICK","BLUELICK", "MILL", "BEARHILL", "DUNGHILL", "ALEX", "KOCH", "WSHALEN")))
#g1 + geom_violin(scale="area", data= NULL) # look for data in ggplot()

g1 + geom_boxplot()
#g1 + geom_pointrange(ymin = 0, ymax=20)
#g1 + geom_dotplot(binaxis="y", stackdir="center")

#par(mfrow=c(1,1))
g1 <- ggplot(sal, aes(stream, air))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(stream, water))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(stream, pH))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(stream, EC))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(stream, TDS))
g1 + geom_boxplot()

barplot(sal$total, names.arg = sal$stream)

mean(sal$total, na.rm = TRUE)

#if(sal$Stream == "res") print(mean(i))

g2 <- ggplot(sal, aes(x=total, y=up_down))
g2 + geom_boxplot()
g2 + geom_bar()

#mean(Totalsal$res)
mean_res <- function(total, up_down="res"){
  mean <- sal$total
}

mean_res(up_down = "res")

sapply(split(total, up_down), mean)
sapply(split(total, type), mean)

plot(mean(total, type))

# detach(sal)

#####

# Group by transect-visit

sal_means <- sal %>%
  group_by(stream, transect, up_down, type) %>%
  dplyr::select(-date, -observers, -visit) %>%
  summarise_all(mean, na.rm = TRUE)

sal_sds <- sal %>%
  group_by(stream, type, transect, up_down) %>%
  dplyr::select(-date, -observers, -visit) %>%
  summarise_all(sd, na.rm = TRUE)

print(sal_means, n = nrow(sal_means))
print(sal_sds, n = 36)

#sal_type <- sal %>%
#  group_by(type) %>%
#  dplyr::select(-date,-observers, -visit) %>%
#  summarise(avg_count = colMeans(sal[,c(8:19)], na.rm = T))

sal_counts <- sal %>%
  group_by(site, visit) %>%
  dplyr::select(-date, -observers) %>%
  summarise_all(mean, na.rm = TRUE)

######

###### plot ggplots with groupings? pH
ggplot(sal, aes(x = reorder(type, pH, FUN = median), pH)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, pH, FUN = median), pH)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# violin sal_means shows range of pH is much more constricted in ref than res

##### pH~up_down
ggplot(sal, aes(x = reorder(up_down, pH, FUN = median), pH)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, pH, FUN = median), pH)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# doesn't really show differences between up_down, but shows differences in ranges

##### water temp~ type
ggplot(sal, aes(x = reorder(type, water, FUN = median), water)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, water, FUN = median), water)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, water, FUN = median), water)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, water, FUN = median), water)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# violin plots show ranges differ a bit, overall patterns relatively the same

##### water temp~up_down
ggplot(sal, aes(x = reorder(up_down, water, FUN = median), water)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, water, FUN = median), water)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, water, FUN = median), water)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, water, FUN = median), water)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# no real diff between up_down

##### air temp~ type
ggplot(sal, aes(x = reorder(type, air, FUN = median), air)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, air, FUN = median), air)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, air, FUN = median), air)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, air, FUN = median), air)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# violin plots show ranges differ a bit, overall patterns relatively the same, ref slightly cooler?

##### air temp~up_down
ggplot(sal, aes(x = reorder(up_down, air, FUN = median), air)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, air, FUN = median), air)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, air, FUN = median), air)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, air, FUN = median), air)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# no real difference, downstream range is larger than both up and NA

##### EC~ type
ggplot(sal, aes(x = reorder(type, EC, FUN = median), EC)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, EC, FUN = median), EC)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, EC, FUN = median), EC)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, EC, FUN = median), EC)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw
# violin plots show ranges differ a bit, overall patterns relatively the same, ref has higher max EC

##### EC~up_down
ggplot(sal, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
# EC is higher down than up and higher up than NA, interesting....

#####

##### Explore the effect of distance from a restoration EBISL
ggplot(data = filter(sal_means, type != "ref"), aes(dist, EBISL)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "EBISL"]), na.rm = TRUE)), colour = "red")

ggplot(data = filter(sal_means, type != "ref"), aes(dist, EBISL)) + geom_boxplot(aes(group = dist), fill = "lightblue") + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "EBISL"]), na.rm = TRUE)), colour = "red") + theme_bw()

##### Explore the effect of distance from a restoration DMONA
ggplot(data = filter(sal_means, type != "ref"), aes(dist, DMONA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DMONA"]), na.rm = TRUE)), colour = "red")

ggplot(data = filter(sal_means, type != "ref"), aes(dist, DMONA)) + geom_boxplot(aes(group = dist), fill = "lightblue") + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DMONA"]), na.rm = TRUE)), colour = "red") + theme_bw()
# :((( not enough counts, too sensitive to res streams

##### Explore the effect of distance from a restoration GPORL
ggplot(data = filter(sal_means, type != "ref"), aes(dist, GPORL)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "GPORL"]), na.rm = TRUE)), colour = "red")

ggplot(data = filter(sal_means, type != "ref"), aes(dist, GPORL)) + geom_boxplot(aes(group = dist), fill = "lightblue") + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "GPORL"]), na.rm = TRUE)), colour = "red") + theme_bw()
# :(((( not enough counts, too sensitive to res streams

##### Explore the effect of distance from a restoration DOCHA
ggplot(data = filter(sal_means, type != "ref"), aes(dist, DOCHA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DOCHA"]), na.rm = TRUE)), colour = "red")

ggplot(data = filter(sal_means, type != "ref"), aes(dist, DOCHA)) + geom_boxplot(aes(group = dist), fill = "lightblue") + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DOCHA"]), na.rm = TRUE)), colour = "red") + theme_bw()
# below ref avg, boxplot better than line graph, still little data

##### Explore the effect of distance from a restoration DFUsA
ggplot(data = filter(sal_means, type != "ref"), aes(dist, DFUSA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DFUSA"]), na.rm = TRUE)), colour = "red")

ggplot(data = filter(sal_means, type != "ref"), aes(dist, DFUSA)) + geom_boxplot(aes(group = dist), fill = "lightblue") + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DFUSA"]), na.rm = TRUE)), colour = "red") + theme_bw()


##### 

##### Total counts

#counts = sal %>%
#  select(Total) %>%
#  summarise(count = colSums(numbers(sal[8:19,])))

#counts <- data.frame(species = c("DFUSL", "DFUSA", "DOCHL", "DOCHA", "DMONL", "DMONA",	"EBISL", "EBISA",	"GPORL",	
#                                 "GPORA", "PRUBL","PRUBA"),
#                     counts = colSums(values())
counts
ggplot(data = counts, aes(x=species, y=total)) + geom_bar(data = NULL, stat= "identity", fill = "lightblue") + theme_bw()
#rowMeans(sal, na.rm=FALSE, dims = 1)


######

##### Explore the effect of type on GPORL
ggplot(sal, aes(x = reorder(type, GPORL, FUN = mean), GPORL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, GPORL, FUN = mean), GPORL)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, GPORL, FUN = mean), GPORL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, GPORL, FUN = mean), GPORL)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()

##### Explore the effect of type on DMONA
ggplot(sal, aes(x = reorder(type, DMONA, FUN = mean), DMONA)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, DMONA, FUN = mean), DMONA)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, DMONA, FUN = mean), DMONA)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, DMONA, FUN = mean), DMONA)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()

##### Explore the effect of type on EBISL
ggplot(sal, aes(x = reorder(type, EBISL, FUN = mean), EBISL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, EBISL, FUN = mean), EBISL)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, EBISL, FUN = mean), EBISL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, EBISL, FUN = mean), EBISL)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()


######

##### Explore the effect of up_down on EBISL
ggplot(sal, aes(x = reorder(up_down, EBISL, FUN = mean), EBISL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, EBISL, FUN = mean), EBISL)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, EBISL, FUN = mean), EBISL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, EBISL, FUN = mean), EBISL)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()

##### Explore the effect of up_down on DFUSA
ggplot(sal, aes(x = reorder(up_down, DFUSA, FUN = mean), DFUSA)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, DFUSA, FUN = mean), DFUSA)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, DFUSA, FUN = mean), DFUSA)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, DFUSA, FUN = mean), DFUSA)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()
# too little data, graphs are cool but don't show much

##### Explore the effect of up_down on DOCHA
ggplot(sal, aes(x = reorder(up_down, DOCHA, FUN = mean), DOCHA)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(up_down, DOCHA, FUN = mean), DOCHA)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()

ggplot(sal_means, aes(x = reorder(up_down, DOCHA, FUN = mean), DOCHA)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(up_down, DOCHA, FUN = mean), DOCHA)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()
# cool graphs but too little data, can't really infer anything


#####

##### Paired boxplot of total~ grouped by treatment type
# Is there some way to do counts~species grouped by type? without moving around the data in the csv?
# Something in dplyr??
# ???  ggplot(sal_means, aes(x = reorder(counts, EBISL, FUN = mean), EBISL)) + facet_wrap(~type) + geom_boxplot(fill = "lightblue") + theme_bw()

library(tidyr)

foo <- sal_means[4:12]

bar <- foo %>%
  ungroup() %>%
  gather(species, count, -type, -dist)

ggplot(data = bar, aes(type, count)) + geom_boxplot() + facet_wrap(~species)

ggplot(data = bar, aes(type, count)) + geom_boxplot() + facet_wrap(~species, scales = "free")

ggplot(data = bar, aes(species, count)) + geom_boxplot() + facet_wrap(~type)

ggplot(data = bar, aes(species, count)) + geom_boxplot() + facet_wrap(~type, scales = "free")

######



#####

####### Graphs I would like to use ??
# sal_means box or violin GPORL~type
# sal or sal_means box DMONA~type
# sal box EBISL~type

# sal or sal_means box EBISL~up_down

# line graphs looking at avg count ~ distance for each species
# EBISL had a uptick in counts right after the sand dump, while DFUSA and DOCHA had a downtick
# GPORL and DMONA had very little counts and thus no observable relationship other than the fact that counts were lower than the average for ref streams

# violin sal_means water temp~ type
# violin sal_means pH~type
# box sal EC~up_down