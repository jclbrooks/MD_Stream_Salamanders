# Checking Species Ranges
# We wanted to check if some of the species observed were found outside of the species range. We created a species distribution matrix in format_landscape_data.R and edited it into the usable spp_dist object in prep_occ.R. Here I compared this species distribution matrix to our occupancy matrix.

# spec_dist == combos
#str(spec_dist) # 210 obs. of 13 variables, from format_landscape_data.R
str(spp_dist) # line 52 of prep_occ.R; species distribution matrix
str(df_occ_all) # from line 95 prep_occ.R; occupancy matrix

#reformat the occupancy data 
occ <- df_occ_all %>%
  select(year, region, transect, species, pass1, pass2, pass3, pass4) %>%
  mutate(present_in_year = ifelse(pass1 == 1 | pass2 == 1 | pass3 == 1 | pass4 == 1, 1, NA),
         present_in_year = ifelse(is.na(present_in_year), 0, present_in_year)) %>%
  select(region, transect, species, present_in_year) %>%
  group_by(region, transect, species) %>%
  summarise(present = max(present_in_year)) %>%
  ungroup() %>%
  pivot_wider(names_from = species, values_from = present) %>%
  pivot_longer(cols = c(DFUS, DMON, DOCH, EBIS, EGUT, ELON, GPOR, PRUB), names_to = "species", values_to = "present") %>%
  mutate(present = ifelse(is.na(present), 0, present)) %>%
  select(-region) %>%
  arrange(transect, species)

# reformat the species distribution matrix
spec_distributions <- spp_dist %>%
  pivot_longer(cols = c(DFUS, DMON, DOCH, EBIS, EGUT, ELON, GPOR, PRUB), names_to = "species", values_to = "present_range") %>%
  select(transect, species, present_range) %>%
  arrange(transect,species) 

# sub <- subset(occ, !(transect %in% spec_dist_condense$transect))
# sub_opp <- subset(spec_dist_condense, !(transect %in% occ$transect))

length(unique(spec_distributions$transect))
length(unique(occ$transect))
subset(spec_distributions, !(transect %in% occ$transect)) # GREAT!! correct number of transects and all should match


test <- occ %>%
  left_join(spec_distributions)
summary(test) #shouldn't be any NA's
subset(test, (present_range = 0) & (present = 1)) #present_range is the known occupied range, present is the observed occupancy
# this shows there are no observations of a species outside of their range



# DOESN'T WORK
# occ <- df_occ_all %>%
#   select(year, region, transect, species, pass1, pass2, pass3, pass4) %>%
#   mutate(present_in_year = ifelse(pass1 == 1 | pass2 == 1 | pass3 == 1 | pass4 == 1, 1, NA),
#          present_in_year = ifelse(is.na(present_in_year), 0, present_in_year)) %>%
#   select(region, transect, species, present_in_year) %>%
#   group_by(region, transect, species) %>%
#   summarise(present = max(present_in_year)) %>%
#   ungroup() %>%
#   #complete(species, nesting(region, transect, present), fill = list(value1 = 0))
#   #tidyr::expand(nesting(region, transect, present), species) 
#   pivot_wider(names_from = species, values_from = present)
# # this shows which species were not found at each region-site
# 
# occ <- occ %>%
#   mutate(DFUS = ifelse(is.na(DFUS), 0, DFUS),
#          DMON = ifelse(is.na(DMON), 0, DMON),
#          DOCH = ifelse(is.na(DOCH), 0, DOCH),
#          EBIS = ifelse(is.na(EBIS), 0, EBIS),
#          GPOR = ifelse(is.na(GPOR), 0, GPOR),
#          EGUT = ifelse(is.na(EGUT), 0, EGUT),
#          ELON = ifelse(is.na(ELON), 0, ELON),
#          PRUB = ifelse(is.na(PRUB), 0, PRUB),
#          Capital = ifelse(region == "Capital", 1,0),
#          Canaan = ifelse(region == "Canaan", 1,0),
#          Shenandoah = ifelse(region == "Shenandoah", 1,0),
#          WMaryland = ifelse(region == "WMaryland", 1,0)) %>% # replaces NAs with 0's
#   select(-region)
# 
# 
# spec_dist$transect == occ$transect
# 
# a <- subset(spec_dist, !(transect %in% occ$transect))
# b <- subset(occ, !(transect %in% spec_dist$transect))

