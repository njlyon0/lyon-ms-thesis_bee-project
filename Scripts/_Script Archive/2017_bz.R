##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                        # Lyon Thesis -- Bee Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Due to unequal sampling intensity across the flowering phenology (I hate cows) these data are not analyzable
# This makes this code entirely exploratory, so
# several times this code will refer to these data and any plot produced from them as "year 0"

# Set WD
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Required libraries
library(plyr); library(ggplot2); library(sp); library(cowplot)

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Cleaning and Response Calculation ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# Index file
beetreat <- read.csv("./Indices/trmntinfo_17.csv")
julindex <- read.csv("./Indices/julianinfo.csv")

##  ----------------------------------------------------------  ##
                  # Bee Tidy ####
##  ----------------------------------------------------------  ##
# Read in data
bz <- read.csv("Data/Raw/bz17_raw.csv")

# PAUSE!

# DATA DICTIONARY:
colnames(bz)
# "Sampling.Event.ID" = a unique number code for each combination of date and patch sampled.
# "Capture.Year" = Sort of irrelevant, but "2017" in all cases
# "Capture.Date" =  the date the bees were collected on listed as a decimal
# "Round" = number of times that particular patch was sampled that summer
# "Site" = pasture from which bees were collected (three letter abbreviation in all cases)
# "SiteCode" = site abbreviation, hyphen, patch abbreviation (single letter for patch)
# "Specimen.ID" = unique 4-digit code for each individual bee
    ## For pinned bees (see later column), this can be used to find specific bees in my collection
# "Family" = Family of the collected bees
# "Genus" =  genus of the collected bees
# "Species" =  specific epithet of collected bees
  ## For genus Lasioglossum this is actually subgenus ID, but the premise is the same
# "Sex" = either "M" for male, "F" for female, or "Q" for queen (no Qs in this dataset)
# "Number" =  # 1 for bees and 0 for rows kept only for posterity (i.e. no bowls recovered or no bees collected)
# "Bowls.Recovered" =  # number of bowls recovered per transect (maximum 6)
# "ID.Checked." = holdover from quality control where bees with a checked species ID were given a "Y" for yes
# "Pinned." = Either a "Y" for yes pinned (i.e. is in my collection), or an explanation why they aren't there

# RESUME CODE

# Remove posterity rows (row created when site had no bees at that height)
bz_v2 <- bz[!(bz$Number == 0),]

# also all PYW and PYS sites (transect sampling discontinued due to bovine a-holes)
bz_v3 <- bz_v2[!(bz_v2$Site == "PYS" | bz_v2$Site == "PYW"),]

# Also remove bees from transects where fewer than 5 bowls were collected
bz_v4 <- subset(bz_v3, bz_v3$Bowls.Recovered >= 5)

# Hopefully there are no spelling errors, but check to be sure
sort(unique(bz_v4$Genus))
sort(unique(bz_v4$Species))

# Need to create one column of binomial latin names for bees
bz_v4$Bee.Species <- paste0(bz_v4$Genus, " ", bz_v4$Species)
sort(unique(bz_v4$Bee.Species))

# Remove the handful of rows (3) where mold made an ID confirmation impossible
bz_v5 <- bz_v4[!(bz_v4$Bee.Species == " "),]

# Add in treatment column 
bz_v5$Fescue.Treatment <- beetreat$Fescue.Treatment[match(bz_v5$SiteCode, beetreat$DataCode)]
str(bz_v5)

# Insidious, but I have a space after "High" one time that will make it be treated as a different group
sort(unique(as.character(bz_v5$Height))) # see the space before the end quote?
bz_v5$Height <- gsub("High ", "High", bz_v5$Height)
sort(unique(as.character(bz_v5$Height)))

# Foolish, but two of the dates were mistakenly recorded as being the same as their corresponding floral data
# Easy fix though due to the format we entered dates (patting myself on the back for that one)
bz_v5$Capture.Date <- as.numeric(gsub(6.08, 6.09, bz_v5$Capture.Date))
bz_v5$Capture.Date <- as.numeric(gsub(6.20, 6.21, bz_v5$Capture.Date))
bz_v5$Capture.Date <- as.numeric(gsub(6.19, 6.20, bz_v5$Capture.Date))

# Now that date is fixed, get Julian dates
bz_v5$Julian <- julindex$Julian[match(bz_v5$Capture.Date, julindex$Date)]
  ## If you're unfamiliar, it's just a scale of dates where each day is an integer between 1 and 365
  ## Makes linear trends easier to plot over time
str(bz_v5)

# Sum through just patches
    ## Code also removes columns that make sense to record but aren't valuable in this context
    ## You can infer which of these columns those are because they are not listed here
bz_v6 <- aggregate(Number ~ Sampling.Event.ID + Julian + Site + SiteCode + 
                     Height + Fescue.Treatment + Bee.Species, data = bz_v5, FUN = sum)
str(bz_v6)

# Save this dataframe out
write.csv(bz_v6, "./Data/clean_2017bz.csv", row.names = F)

# Now mush to wide format (i.e. each species becomes a column populated by its abundances)
bz_wide <- spread(bz_v6, Bee.Species, Number, fill = 0)
str(bz_wide) # idiot check

# Smush calculated variables into main dataframe
bz_wide_v2 <- bz_wide
bz_wide_v2$Abundance <- as.vector(rowSums(bz_wide[, -c(1:6)]))
bz_wide_v2$Species.Density <- as.vector(specnumber(bz_wide[, -c(1:6)]))
bz_wide_v2$Diversity <- as.vector(diversity(bz_wide[, -c(1:6)], index = "shannon"))

# Final pre-save check
str(bz_wide_v2)

# Save for wide format
write.csv(bz_wide_v2, "./Data/clean_2017bz_wide.csv", row.names = F)

# Create an annual report variant (without sampling event ID or date)
ann.rep.v0 <- aggregate(Number ~ Site + SiteCode + Fescue.Treatment + Bee.Species, data = bz_v6, FUN = sum)

# Get that in wide format for interpretability's sake
ann.rep.v1 <- spread(ann.rep.v0, Bee.Species, Number, fill = NA)

# And save it out
write.csv(ann.rep.v1, "./Data/report_2017.csv", row.names = F)

##  ----------------------------------------------------------  ##
                 # Floral Tidy ####
##  ----------------------------------------------------------  ##
# Data file
flr <- read.csv("Data/Raw/bzflr17_raw.csv")

# PAUSE

# DATA DICTIONARY:
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
flr_v2$Fescue.Treatment <- beetreat$Fescue.Treatment[match(flr_v2$SiteCode, beetreat$DataCode)]

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
flr_v4$Julian <- julindex$Julian[match(flr_v4$Capture.Date, julindex$Date)]

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
bz <- read.csv("./Data/clean_2017bz.csv")
flr <- read.csv("./Data/clean_2017flr.csv")

# Re-level herbicide treatment column
unique(bz$Fescue.Treatment)
bz$Fescue.Treatment <- factor(as.character(bz$Fescue.Treatment), levels = c("Ref", "Con", "Spr", "SnS"))
unique(bz$Fescue.Treatment)

# Graphing shortcuts
nah <- element_blank()
bee.colors <- rev(topo.colors(length(unique(bz$Bee.Species))))

# Useful summary dataframes
bz.ht <- aggregate(Number ~ Height + Bee.Species, data = bz, FUN = sum)

##  ----------------------------------------------------------  ##
    # High vs. Low Community Composition ####
##  ----------------------------------------------------------  ##
# Get a separate high and low dataframe
hi <- subset(bz.ht, bz.ht$Height == "High")
lo <- subset(bz.ht, bz.ht$Height == "Low")

# Get a pie chart of species abundances (across all sites/dates) for "high" bees
hibz.pie <- ggplot(hi, aes(x = Height, y = Number, fill = Bee.Species)) +
  coord_polar(theta = "y", start = 0, direction = 1) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "") +
  theme(axis.title = nah, panel.border = nah, panel.grid = nah, legend.title = nah,
        axis.ticks = nah, axis.line = nah); hibz.pie

# Do the same for "low" bees
lobz.pie <- ggplot(lo, aes(x = Height, y = Number, fill = Bee.Species)) +
  coord_polar(theta = "y", start = 0, direction = 1) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "") +
  theme(axis.title = nah, panel.border = nah, panel.grid = nah, legend.title = nah,
        axis.ticks = nah, axis.line = nah); lobz.pie
  
# Plot 'em
plot_grid(hibz.pie, lobz.pie, labels = c("", ""), nrow = 1, ncol = 2)

jpeg("./Graphs/bzspp_2017pies.jpeg")
ggplot(bz.ht, aes(x = Height, y = Number, fill = Bee.Species)) +
  coord_polar(theta = "y", start = 0, direction = 1) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "") +
  theme(axis.title = nah, panel.border = nah, panel.grid = nah, legend.title = nah,
        axis.ticks = nah, axis.line = nah, legend.position = "right")
dev.off()

##  ----------------------------------------------------------  ##
        # Single Sp. Resp to Treatment ####
##  ----------------------------------------------------------  ##
# Get just full season totals for each bee species
bz.tot <- aggregate(Number ~ Bee.Species, data = bz, FUN = sum)

# Calculate the relative abundance of each species (%)
bz.tot$Rel.Abun <- (bz.tot$Number / sum(bz.tot$Number) ) * 100

# Who are the top 5?
bz.tot[order(bz.tot$Rel.Abun, decreasing = T),][c(1:5),]
  ## Will now plot the top 3 (because #3 is doubly as abundant as #4)

# Get dataframes of just these ones
augaur <- subset(bz, bz$Bee.Species == "Augochlorella aurata")
lasdia <- subset(bz, bz$Bee.Species == "Lasioglossum Dialictus")
hallig <- subset(bz, bz$Bee.Species == "Halictus ligatus")

# And plot 'em
ggplot(augaur, aes(x = as.factor(Julian), y = Number, fill = Fescue.Treatment))+
  geom_bar(stat = 'identity') +
  labs(x = "Julian Date", y = "Augochlorella aurata #") +
  scale_fill_manual(values = topo.colors(4)) +
  facet_grid(Fescue.Treatment ~ .) +
  theme(legend.title = nah, legend.position = "none")

ggsave("./Graphs/bz2017_auguar.pdf", plot = last_plot())

ggplot(lasdia, aes(x = as.factor(Julian), y = Number, fill = Fescue.Treatment))+
  geom_bar(stat = 'identity') +
  labs(x = "Julian Date", y = "Lasioglossum Dialictus spp. #") +
  scale_fill_manual(values = topo.colors(4)) +
  facet_grid(Fescue.Treatment ~ .) +
  theme(legend.title = nah, legend.position = "none")

ggsave("./Graphs/bz2017_lasdia.pdf", plot = last_plot())

ggplot(hallig, aes(x = as.factor(Julian), y = Number, fill = Fescue.Treatment))+
  geom_bar(stat = 'identity') +
  labs(x = "Julian Date", y = "Halictus ligatus #") +
  scale_fill_manual(values = topo.colors(4)) +
  facet_grid(Fescue.Treatment ~ .) +
  theme(legend.title = nah, legend.position = "none")

ggsave("./Graphs/bz2017_hallig.pdf", plot = last_plot())
