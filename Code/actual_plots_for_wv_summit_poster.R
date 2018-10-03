###############
# Plots for WV Summit Presentation

# J. Brooks

###############

# Read-in data and data manipulation
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)
counts <- read.csv("Data/Just_Count_Data.csv", stringsAsFactors = FALSE)

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
  geom_bar(data = NULL, stat= "identity", fill = "tan4")  + 
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
  geom_boxplot(fill = "tan4") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Average E. bislineata Larvae") +
  xlab("Section of Stream") +
  scale_x_discrete(labels = c('Upstream','Downstream','Reference'))

#sal_means boxplot pH~type
ggplot(sal_means, aes(x = reorder(up_down, pH, FUN = median), pH)) + 
  geom_boxplot(draw_quantiles = c(0.25, 0.5, 0.75), fill = "tan4") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("pH") +
  xlab("Stream Type") +
  ylim(4,8) +
  scale_x_discrete(labels = c('Up','Down','Reference'))



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


ggplot(sal, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_boxplot(fill = "tan4") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Electrical Conductivity (µS)") +
  xlab("Section of Stream") +
  ylim(0,150) +
  scale_x_discrete(labels = c('Upstream','Downstream','Reference'))

food <- sal_means[,c(1, 2, 3,4,5,6,9,11,12,14)]

bar <- food %>%
  ungroup() %>%
  select(-dist) %>%
  gather(sp_stage, count, -type, -up_down, -transect, -stream) %>%
  mutate(sp1 = gsub("(.{4})", "\\1.", sp_stage)) %>%
  separate(col = sp1, into = c("species", "stage"))



facet_labels <- c(`A`="Adult", `L`="Larvae")
ggplot(data = bar, aes(x = reorder(species, count, FUN = median), count, fill = up_down)) + geom_boxplot() + facet_wrap(~stage, labeller=as_labeller(facet_labels)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 20, color = "white"),
        strip.background =element_rect(fill="tan4")) +
  ylim(0,12.5) +
  ylab("Average Counts") +
  xlab("Species") +
  #scale_fill_brewer(palette = "OrRd") +
  scale_fill_manual(values=c("tan4", "tan3", "tan2"),
                   labels = c("Downstream", "Upstream", "Reference"),
                   name = "Section\nof\nStream") +
  scale_x_discrete(limits=c("DOCH", "DMON", "DFUS", "EBIS", "GPOR"))

# fill = "tan4" 

  
#ggplot(data = filter(sal_means, type != "ref"), aes(dist, EBISL)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "EBISL"]), na.rm = TRUE)), colour = "red")
#ggplot(data = filter(sal_means, type != "ref"), aes(dist, DOCHA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DOCHA"]), na.rm = TRUE)), colour = "red")
#ggplot(data = filter(sal_means, type != "ref"), aes(dist, DFUSA)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = mean(unlist(sal_means[which(sal_means$type == "ref"), "DFUSA"]), na.rm = TRUE)), colour = "red")

#spec <- sal_means[1:18]

#mat <- spec %>%
#  ungroup() %>%
#  gather(transect, visit, -dist)

#mat <- mat %>%
#  group_by(transect, visit) %>%
#  summarise_all(mean, na.rm = T)

#ggplot(data = mat, aes(type, count)) + geom_boxplot() + facet_wrap(~species)

#sal_counts <- sal %>%
#  group_by(transect, visit) %>%
#  dplyr::select(-date, -observers) %>%
#  summarise_all(mean, na.rm = TRUE)

#ggplot(sal, aes(x = reorder(up_down, EC, FUN = median), EC)) + geom_boxplot(fill = "darkorange2") + 
#  theme_classic() +
#  ylab("Electrical Conductivity (µS)") +
#  xlab("Section of Stream") +
#  scale_x_discrete(labels = c('Upstream','Downstream','Reference'))
