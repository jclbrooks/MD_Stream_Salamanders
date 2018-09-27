###############################
# Exploratory plots of salamander and water chemistry
# J. Brooks
###############################

# load packages
library(ggplot2)
library(dplyr)


# read in data
sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)


head(sal)
str(sal)
summary(sal)

# attach(sal)
pairs(sal[ ,c(20,23,24,25,27,28)])

plot(Total ~ Air, data = sal)

# Add reference to the up, down
sal <- sal %>%
  mutate(type = ifelse(Type == "res", Up_down, Type))
str(sal)

g1 <- ggplot(sal, aes(Stream, Total, order("POPLICKTRIB","ELKLICK","BLUELICK", "MILL", "BEARHILL", "DUNGHILL", "ALEX", "KOCH", "WSHALEN")))
#g1 + geom_violin(scale="area", data= NULL) # look for data in ggplot()

g1 + geom_boxplot()
#g1 + geom_pointrange(ymin = 0, ymax=20)
#g1 + geom_dotplot(binaxis="y", stackdir="center")

#par(mfrow=c(1,1))
g1 <- ggplot(sal, aes(Stream, Air))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(Stream, Water))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(Stream, pH))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(Stream, EC))
g1 + geom_boxplot()

g1 <- ggplot(sal, aes(Stream, TDS))
g1 + geom_boxplot()

barplot(sal$Total, names.arg = sal$Stream)

mean(sal$Total, na.rm = TRUE)

#if(sal$Stream == "res") print(mean(i))

g2 <- ggplot(sal, aes(x=Total, y=Up_down))
g2 + geom_boxplot()
g2 + geom_bar()

#mean(Totalsal$res)
mean_res <- function(Total, Up_down="res"){
  mean <- sal$Total
}

mean_res(Up_down = "res")

sapply(split(Total, Up_down), mean)
sapply(split(Total, Type), mean)

plot(mean(Total, Type))

# detach(sal)


# Group by transect-visit

sal_means <- sal %>%
  group_by(Stream, Type, Transect, Up_down, type) %>%
  dplyr::select(-Date, -Observers, -Visit) %>%
  summarise_all(mean, na.rm = TRUE)

sal_sds <- sal %>%
  group_by(Stream, Type, Transect, Up_down, type) %>%
  dplyr::select(-Date, -Observers, -Visit) %>%
  summarise_all(sd, na.rm = TRUE)

print(sal_means, n = nrow(sal_means))
print(sal_sds, n = 36)

# plot ggplots with groupings?
ggplot(sal, aes(x = reorder(type, pH, FUN = median), pH)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, pH, FUN = median), pH)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal, aes(x = reorder(type, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue") + theme_bw()

ggplot(sal_means, aes(x = reorder(type, EBISL, FUN = mean), EBISL)) + geom_boxplot(fill = "lightblue") + theme_bw()
ggplot(sal_means, aes(x = reorder(type, EBISL, FUN = mean), EBISL)) + geom_violin(draw_quantiles = 0.5, fill = "lightblue") + theme_bw()
