##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                # Lyon Thesis -- Preliminary Bee Project
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Tidy code for 2017 floral data from bee transects
  ## See "2017_bz.R" for more specific information

# Set WD
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Required libraries
library(plyr); library(tidyr); library(vegan);
library(ggplot2); library(cowplot)

##  ----------------------------------------------------------------------------------------------------------  ##
                        # Cleaning and Response Calculation ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# Index files
## Treatments
treats <- read.csv("./Indices/trmntinfo.csv")

## Julian Days
julians <- read.csv("./Indices/julianinfo.csv")

## Bee Functional Diversity
fxns <- read.csv("./Indices/fxninfo.csv")


# Data file
flr <- read.csv("Data/Raw/bzflr17_raw.csv")

# PAUSE

# FLORAL DATA DICTIONARY ####
colnames(flr)
# For simplicity, only columns that are not found in the bee dataframe will be explained here. Sound fair?
# "Nectar.Common.Name" = common name of the plant species observed on a transect
# "Section.1" = the abundance of inflorescences of that plant species found from 0-20m on the transect
# "Section.2" = the abundance of inflorescences of that plant species found from 20-40m on the transect
# "Section.3" = the abundance of inflorescences of that plant species found from 40-60m on the transect
# "Section.4" = the abundance of inflorescences of that plant species found from 60-80m on the transect
# "Section.5" = the abundance of inflorescences of that plant species found from 80-100m on the transect
## Transect total length is 100 meters
# "Collector" = initials of person who identified/counted flowers
# "Enterer" = initials of person who entered the data from the datasheet into the Excel spreadsheet
# "Checker" = initials of person who checked that the entries in Excel were consistent with the datasheet
## NJL = Nick J Lyon; EKS = Emma K Stivers; ELM = Erin Lynn McCall; LMG = Luke M Goodman

# RESUME CODING

# Sum section counts together for full transect counts
flr$TransectTotals <- rowSums(flr[,8:12])

# And ditch the section counts (not relevant to the scale of question we're asking)
flr_v2 <- flr[,-c(8:12)]

# Might as well add treatment labels (could be nice to actually answer the question at hand?)
flr_v2$Herbicide.Treatment <- treats$Herbicide.Treatment[match(flr_v2$SiteCode, treats$Patch)]

# Reorder so that you have only the columns you want and in the order you want 'em
str(flr_v2)
flr_v3 <- flr_v2[,c(1, 3, 5:6, 12, 7, 11)]
str(flr_v3)

# Check Nectar common names
flr_v3$Nectar.Common.Name <- tolower(flr_v3$Nectar.Common.Name)
sort(unique(flr_v3$Nectar.Common.Name))

# Remove foolish empty rows/unspecific "species" references
flr_v4 <- flr_v3[!(flr_v3$Nectar.Common.Name == ""),]

# Standardize any names that are redundant (multiple common names that refer to the same species)
flr_v4$Nectar.Common.Name <- gsub("lance-leafed plantain", "ribwort plantain", flr_v4$Nectar.Common.Name)
flr_v4$Nectar.Common.Name <- gsub("lance leaf plantain", "ribwort plantain", flr_v4$Nectar.Common.Name)
flr_v4$Nectar.Common.Name <- gsub("common plantain", "broadleaf plantain", flr_v4$Nectar.Common.Name)
flr_v4$Nectar.Common.Name <- gsub("bee balm", "bergamot", flr_v4$Nectar.Common.Name)
sort(unique(flr_v4$Nectar.Common.Name))

# Now to fix the capture date column
## Floral data were collected one day before the bee data because pan traps were set out
## on the same day flowers were counted, but the traps weren't re-collected until 24 hours later
flr_v4$Capture.Date <- flr_v4$Capture.Date + 0.01
sort(unique(flr_v4$Capture.Date))
sort(unique(bz_v5$Capture.Date))

# Now that the dates are fixed, let's swap 'em for Julian dates
flr_v4$Julian <- julians$Julian[match(flr_v4$Capture.Date, julians$Date)]

# And do a quick check to make sure both dataframes' date modifications are still in agreement
sort(unique(flr_v4$Julian))
sort(unique(bz_v5$Julian))

# Want to reorder quickly
flr_v5 <- flr_v4[, c(1, 8, 3:7)]

# Idiot check and save clean version
str(flr_v5)
write.csv(flr_v5, "./Data/clean_2017flr.csv", row.names = F)

# Wide format
flr_wide <- spread(flr_v5, Nectar.Common.Name, TransectTotals, fill = 0)
str(flr_wide)

# Push to new wide dataframe and add calculated response variables
flr_wide_v2 <- flr_wide
flr_wide_v2$Abundance <- as.vector(rowSums(flr_wide[,-c(1:5)]))
flr_wide_v2$Species.Density <- as.vector(specnumber(flr_wide[,-c(1:5)]))
flr_wide_v2$Diversity <- as.vector(diversity(flr_wide[,-c(1:5)], index = "shannon"))

# Final pre-save check
str(flr_wide_v2)

# Save
write.csv(flr_wide_v2, "./Data/clean_2017flr_wide.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
# Explore the Data ####
##  ----------------------------------------------------------------------------------------------------------  ##
rm(list = ls())

# Data
flr <- read.csv("./Data/clean_2017flr.csv")

# Graphing shortcuts
nah <- element_blank()
herb.colors <- c("Ref" = "#003c30", "Con" = "#01665e",
                 "Spr" = "#35978f", "SnS" = "#c7eae5")
mega.colors <- c("Ref-High" = "#003c30", "Ref-Low"  = "#543005",
                 "Con-High" = "#01665e", "Con-Low"  = "#8c510a",
                 "Spr-High" = "#35978f", "Spr-Low" = "#bf812d",
                 "SnS-High" = "#c7eae5", "SnS-Low"  = "#dfc27d")



# END ####


