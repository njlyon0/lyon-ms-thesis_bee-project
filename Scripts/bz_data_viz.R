##  -----------------------------------------------------------------------------------------------------------------------  ##
                         # Lyon Thesis -- Chapter 2: Pollinators and PBG
##  -----------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## The bee dataset is characterized by a few hyper-abundant species and many rare species.
  ## This contributes to overdisperson and model failures to converge.
  ## By quantifying this variation we can better account for it/modify questions accordingly

# START ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(Rmisc)

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Bee Species Totals ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get cleaned data
bz.v0 <- read.csv("./Data/bz-long.csv")

# Remove placeholder entry for bowls collected without bees
bz.v1 <- subset(bz.v0, bz.v0$Bee.Species != "X.x")

# Get species totals
bz.tot <- aggregate(Number ~ Bee.Species, data = bz.v1, FUN = sum)

# Re-order bees by abundance and get a percent of total
bz.ord <- bz.tot[order(bz.tot$Number, decreasing = T), ]

# How many total bees were collected?
bz.tot.num <- sum(bz.ord$Number); bz.tot.num

# Calculate a percent of total for each of these species
bz.ord$Percent.Total <- round(( bz.ord$Number / bz.tot.num ) * 100, digits = 3)

# Save this dataframe
write.csv(bz.ord, "./Summary Info/bz-spp-totals.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                     # "Common" versus "Rare" Bee Species ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Identify a list of the bees that count as "common" (≥ 10% of total bee abundance)
bz.ord$Bee.Species[bz.ord$Percent.Total >= 10]

# Subset the long-format dataframe based on this criterion
bz.common <- subset(bz.v0, bz.v0$Bee.Species == "Agapostemon.virescens" |
                      bz.v0$Bee.Species == "Lasioglossum.sp" | 
                      bz.v0$Bee.Species == "Augochlorella.aurata" |
                      bz.v0$Bee.Species == "Ceratina.dupla" | 
                      bz.v0$Bee.Species == "Halictus.ligatus" |
                      bz.v0$Bee.Species == "X.x")

# Get the common bee frame into wide format
bz.common.wide <- spread(key = Bee.Species, value = Number, fill = 0, data = bz.common)

# Calculate abundance 
bz.common.wide$Abundance <- rowSums(bz.common.wide[,-c(1:8)])
  ## Species density doesn't mean a whole lot now that we removed most of the species
  
# Now save both of these dataframes out
write.csv(bz.common, "./Data/bz-long-common.csv", row.names = F)
write.csv(bz.common.wide, "./Data/bz-wide-common.csv", row.names = F)

# Get a dataframe of the rare species
bz.rare <- subset(bz.v0, bz.v0$Bee.Species != "Agapostemon.virescens" &
                      bz.v0$Bee.Species != "Lasioglossum.sp" & 
                      bz.v0$Bee.Species != "Augochlorella.aurata" &
                      bz.v0$Bee.Species != "Ceratina.dupla" & 
                      bz.v0$Bee.Species != "Halictus.ligatus" &
                      bz.v0$Bee.Species != "X.x")

# And save this in case it is relevant
write.csv(bz.rare, "./Data/bz-long-rare.csv", row.names = F)






# END ####






