###############
# Plots for WV Summit Presentation

# J. Brooks

###############

# Read-in data and data manipulation
library(ggplot2)
library(dplyr)

sal <- read.csv("C:/Users/Jacey/Documents/FSU/Research/R_code/MD_Stream_salamanders/Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)
counts <- read.csv("C:/Users/Jacey/Documents/FSU/Research/R_code/MD_Stream_salamanders/Data/Just_Count_Data.csv", stringsAsFactors = FALSE)

sal_means <- sal %>%
  group_by(stream, transect, up_down, type) %>%
  dplyr::select(-date, -observers, -visit) %>%
  summarise_all(mean, na.rm = TRUE)

sal_sds <- sal %>%
  group_by(stream, type, transect, up_down) %>%
  dplyr::select(-date, -observers, -visit) %>%
  summarise_all(sd, na.rm = TRUE)

#counts_means <- counts %>%
#  group_by(species) %>%
#  dplyr::select(-total, -species)
#  summarise(avg_count = rowMeans(sapply(counts, as.numeric, NA_integer_)),
#            sd_count = rowSds(sapply(counts, as.numeric, NA_integer_)))

# total counts
ggplot(data = counts, aes(x=species, y=total)) + 
  geom_bar(data = NULL, stat= "identity", fill = "darkorange2")  + 
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Total Counts") +
  xlab("Species")

#sal_means boxplot EBISL~up_down
ggplot(sal_means, aes(x = reorder(up_down, EBISL, FUN = mean), EBISL)) + 
  geom_boxplot(fill = "darkorange2") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Average E. bislineata Larvae") +
  xlab("Section of Stream") +
  scale_x_discrete(labels = c('Upstream','Downstream','Reference'))

#sal_means violin pH~type
ggplot(sal_means, aes(x = reorder(type, pH, FUN = median), pH)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "darkorange2") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("pH") +
  xlab("Stream Type") +
  scale_x_discrete(labels = c('Restoration','Reference'))



#EC~dist grouped by up_down
#ggplot(sal, aes(x = reorder(dist, EC, FUN = median), EC)) + geom_boxplot(fill = "darkorange2") + theme_bw() + 
#  facet_wrap(~up_down) +
#  theme(axis.text.x = element_text(size = 9),
#        axis.text.y = element_text(size = 10),
#        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
#        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
#        axis.line = element_line(colour = "black"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank()) +
#  ylab("Electrical Conductivity") +
#  xlab("Distance from Limestone Sand (m)")


#pH~dist grouped by up_down
#ggplot(sal, aes(x = reorder(dist, pH, FUN = median), EC)) + geom_boxplot(fill = "darkorange2") + theme_bw() + 
#  facet_wrap(~up_down) +
#  theme(axis.text.x = element_text(size = 9),
#        axis.text.y = element_text(size = 10),
#        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
#        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
#        axis.line = element_line(colour = "black"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank()) +
#  ylab("pH") +
#  xlab("Distance from Limestone Sand (m)")



#ggplot(sal, aes(x = reorder(total, pH, FUN = median), pH)) + geom_boxplot(fill = "darkorange2") + theme_bw() + 
#  facet_wrap(~type) +
#  theme(axis.text.x = element_text(size = 9),
#        axis.text.y = element_text(size = 10),
#        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
#        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
#        axis.line = element_line(colour = "black"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank()) +
#  ylab("Total Count of Salamanders") +
#  xlab("pH")


ggplot(sal, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_boxplot(fill = "darkorange2") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Electrical Conductivity (µS)") +
  xlab("Section of Stream") +
  scale_x_discrete(labels = c('Upstream','Downstream','Reference'))

ggplot(data = filter(sal_means, type != "ref"), aes(dist, EBISL)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "EBISL"]), na.rm = TRUE)), colour = "red")
ggplot(data = filter(sal_means, type != "ref"), aes(dist, DOCHA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DOCHA"]), na.rm = TRUE)), colour = "red")
ggplot(data = filter(sal_means, type != "ref"), aes(dist, DFUSA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DFUSA"]), na.rm = TRUE)), colour = "red")


