##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                  # Lyon Thesis -- Chapter 2: "Bees"
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE
  ## To clean (aka "tidy" the data collected in 2018)

# START ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(plyr); library(tidyr); library(stringr); library(vegan)

# Load the data
bz.v0 <- read.csv("./Data/raw_bz18.csv")
  ## A full data dictionary is included in the Excel file into which data were recorded
  ## Please refer there if a column is unclear

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Cleaning and Treatment Addition ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Take only bowls that were retrieved (remove destroyed/off clip bowls)
sort(unique(bz.v0$Bowl.Status))
bz.v1 <- subset(bz.v0, bz.v0$Bowl.Status == "Retrieved")

# Check genus and species of bees for spelling errors and fix those that occur
sort(unique(bz.v1$Genus)); sort(unique(bz.v1$Species))
  ## No errors yet

# Get a single column of genus and species and check again
bz.v1$Bee.Species <- paste0(bz.v1$Genus, ".", bz.v1$Species)
sort(unique(bz.v1$Bee.Species))

# Remove the "X.x" placeholder species (from bowls that were recovered with no bees)
sort(unique(bz.v1$Bee.Species))
bz.v2 <- subset(bz.v1, bz.v1$Bee.Species != "X.x")
sort(unique(bz.v2$Bee.Species))

# Check any errors with bowl color spelling
sort(unique(as.character(bz.v2$Bowl.Color)))
bz.v2$Bowl.Color <- as.factor(gsub("^ Yellow$|^Yellow $|^Yelow$", "Yellow", bz.v2$Bowl.Color))
sort(unique(as.character(bz.v2$Bowl.Color)))

# Pull in treatment index
trmnts <- read.csv("./Indices/trmntinfo.csv")

# Put treatments into the dataframe
bz.v2$Adaptive.Mgmt <- trmnts$Adaptive.Mgmt[match(bz.v2$Patch, trmnts$Patch)]
bz.v2$YSB <- trmnts$YSB[match(bz.v2$Patch, trmnts$Patch)]
bz.v2$Herb.Trt <- trmnts$Herbicide.Treatment[match(bz.v2$Patch, trmnts$Patch)]

# Sum occurrences of the same species of bee from the same bowl
bz.v3 <- aggregate(Number ~ Round + Patch + Adaptive.Mgmt + YSB + Herb.Trt +
                   Height + Bowl.Position + Bowl.Color + Bee.Species,
                   FUN = sum, data = bz.v2)
  ## This re-orders the columns to match the above order (with Number furthest to the right though)

# The above step also removes unwanted columns, but in case you're curious what was lost...
setdiff(colnames(bz.v2), colnames(bz.v3))

##  ----------------------------------------------------------------------------------------------------------  ##
                        # Community Metric Calculation ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Now spread to wide format so each bowl gets its own row and bee species are columns
bz.wide <- spread(data = bz.v3, key = Bee.Species, value = Number, fill = 0)

# Calculate the classic trifecta of community metrics
bz.wide.v2 <- bz.wide
bz.wide.v2$Abundance <- rowSums(bz.wide[, -c(1:8)])
bz.wide.v2$Species.Density <- specnumber(bz.wide[, -c(1:8)])

# Save 
write.csv(bz.wide.v2, "./Data/actual_bz18.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Species Totals ####
##  ----------------------------------------------------------------------------------------------------------  ##
# People like to know species totals, so let's get those too
tots.v0 <- aggregate(Number ~ Bee.Species, FUN = sum, data = bz.v2)

# Add in a row for total number of bees collected
tots.v1 <- rbind(tots.v0, c("Total.Abundance", sum(tots.v0$Number)))

# Calculate percent of total for each species!
tots.v1$Percent.of.Total <- round(( as.numeric(tots.v1$Number) / sum(tots.v0$Number) ) * 100, digits = 2)

# And order from most to least abundant
tots.v2 <- tots.v1[order(as.numeric(tots.v1$Number), decreasing = T),]

# Save this file out
write.csv(tots.v2, "./Data/Summary Info/bz.tots.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Floral Data Processing ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Though I don't plan on using the floral data per se, that's no reason to not have it cleaned & ready
rm(list = ls())

# Read in data
flr <- read.csv("./Data/raw_bzflr18.csv")
str(flr)

# Remove the columns that have to do with data entry/checking etc.
flr.v2 <- flr[, -c(14:18)]
  ## Makes sense to record in the raw data but is not relevant to analysis
str(flr.v2)

# Check the spelling on the floral common names column and fix any errors
sort(unique(flr.v2$Flower.Common.Name))
flr.v2$Flower.Common.Name <- as.character(flr.v2$Flower.Common.Name)
sort(unique(flr.v2$Flower.Common.Name))
  ## No errors yet

# Get df into long format where post # is a column and all floral abundances are in another column
flr.v3 <- gather(data = flr.v2, key = "Post", value = Flower.Number, ... = 8:13)
str(flr.v3)

# Get Post.ID
flr.v3$Post.ID <- paste0(flr.v3$Patch, "-", flr.v3$Post)
str(flr.v3)

# Pull in treatment index
trmnts <- read.csv("./Indices/trmntinfo.csv")

# Put treatments into the dataframe
flr.v3$Adaptive.Mgmt <- trmnts$Adaptive.Mgmt[match(flr.v3$Patch, trmnts$Patch)]
flr.v3$YSB <- trmnts$YSB[match(flr.v3$Patch, trmnts$Patch)]
flr.v3$Herb.Trt <- trmnts$Herbicide.Treatment[match(flr.v3$Patch, trmnts$Patch)]
str(flr.v3)

# Kind of an idiot check, but make sure every flower has only one record per post
flr.v4 <- aggregate(Flower.Number ~ Round + Site + Patch + Post.ID +
                      Adaptive.Mgmt + YSB + Herb.Trt + Flower.Common.Name, FUN = sum, data = flr.v3)
str(flr.v4)

# The massive rows you lost were all the flowers where the gather step created a row for 0 abundance
count(is.na(flr.v3$Flower.Number))

# As with bee cleaning, the aggregate step loses columns not specified, so do a quick check to make sure
setdiff(colnames(flr.v3), colnames(flr.v4))

# Get the data in wide format where floral common species are column names and abundance is the fill
flr.wide <- spread(data = flr.v4, key = Flower.Common.Name, value = Flower.Number, fill = 0)
str(flr.wide)

# Calculate the classic trifecta of community metrics
flr.wide.v2 <- flr.wide
flr.wide.v2$Abundance <- rowSums(flr.wide[, -c(1:7)])
flr.wide.v2$Species.Density <- specnumber(flr.wide[, -c(1:7)])
flr.wide.v2$Diversity <- diversity(flr.wide[, -c(1:7)], index = "shannon")

# Save it
write.csv(flr.wide.v2, "./Data/actual_bzflr18.csv", row.names = F)

# END ####

