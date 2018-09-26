### Hocking Lab Meeting 9-17-18


adults <- read.csv("dragonfly_2018.csv", stringsAsFactors = F)  # changes all factors to characters

str(adults)

location <- read.csv("location.csv", stringsAsFactors = F)

str(location)

library(dplyr)
#install.packages("dplyr")
library(dplyr)

# masked objects - replaced the function with the same name with the function that is in the package that you just loaded

# to avoid mixing up which package it is do this -> package::function()
dr <- dplyr::left_join(adults, location)
# should tell you "Joining, by = "whatever it is sorting and joining the two tables by"

str(df)

# combine a bunch of things : PIPES (%>%)

df <- adults %>%
  left_join(location) %>%
  select(Location, num_coll, Date, person_hours, temp, PaiSki) # only want these columns

# you can summarize columns, do interactions,

df <- adults %>%
  left_join(location) %>%
  select(Location, num_coll, Date, person_hours, temp, PaiSki) %>%
  mutate(temp_do = temp * DO)   # mutate() creates a new column

## LOOK AT STR TO SEE IF ALL NUMBERS ARE NUMBERS AND NOT CHARACTERS AS WELL AS OTHER VARIABLES

# can also filter them
df <- adults %>%
  left_join(location) %>%
  select(Location, num_co, Date, person_hours, temp, PaiSki) %>%
  mutate(temp_do = temp * DO,
         count = ifelse(is.na(PaiSki), 0 , PaiSki)) # if it is NA, return a 0; if not, return the value that it has  
%>% dplyr::filter(!is.na(temp), # temp that is not NA fill in 0s
                !is.na(DO))

str(df)

glm1 <- glm(count ~ temp + DO, data = df, family = "poisson")
# BUT you need to have a glmer because she went back to each site multiple times <- pseudoreplication b/c 
# pretending you have a larger data set than what you really do (IID, independent and identically distributed)
# ADD a random effect of site

# 8 schools problem, they collected data from 8 schools, there were students but within the same class but the class are always within same school, so some correlation; measurements taken at studet level, same area of town, same socio-economic background

# if you have a 100 schools, you can fit it to a normal dist to see the variability
# if you have <10 you should have it as a random effect
# if you only have like 3, you just have it as a fixed effect

# I will need to constrain my randomeffect with my hyper prior (the variance term in the Normal dist)

  
  
  