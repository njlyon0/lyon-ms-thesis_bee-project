##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                               # Lyon Thesis -- Preliminary Bee Project
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Unequal sampling intensity through the season/too few replicates make this dataset unanalyzable.
  ## However, it may have value in terms of anecdote in demonstrating patterns we did/didn't expect.
  ## This will be valuable in post-hoc justification for continuing/refining the bee project into 2018

# Set WD
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Required libraries
library(plyr); library(tidyr); library(vegan);
library(ggplot2); library(cowplot); library(gridExtra); library(grid)

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


##  ----------------------------------------------------------  ##
                  # Bee Tidy ####
##  ----------------------------------------------------------  ##
# Read in data
bz <- read.csv("Data/Raw/bz17_raw.csv")

# BEE DATA DICTIONARY ####
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

# Add in columns for site treatment, bee sociality, and nesting habit
bz_v5$Herbicide.Treatment <- treats$Herbicide.Treatment[match(bz_v5$SiteCode, treats$Patch)]
bz_v5$Sociality <- fxns$Sociality[match(bz_v5$Genus, fxns$Genus)]
bz_v5$Nest.Type <- fxns$Nesting.Habitat[match(bz_v5$Genus, fxns$Genus)]
str(bz_v5)

# Simplify the sociality columns to more manageable number of levels
sort(unique(bz_v5$Sociality))
bz_v5$Sociality <- gsub("Eusocial", "Social", bz_v5$Sociality)
bz_v5$Sociality <- gsub("Solitary/Communal|Solitary/Semi-Social|Solitary/Primitive Social",
                        "Semi-Social", bz_v5$Sociality)
sort(unique(bz_v5$Sociality))

# Simplify nest as well (just to eliminate hives)
sort(unique(bz_v5$Nest.Type))
bz_v5$Nest.Type <- as.factor(gsub("Cavities/Hives", "Cavities", bz_v5$Nest.Type))
sort(unique(bz_v5$Nest.Type))

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
bz_v5$Julian <- julians$Julian[match(bz_v5$Capture.Date, julians$Date)]
  ## If you're unfamiliar, it's just a scale of dates where each day is an integer between 1 and 365
  ## Makes linear trends easier to plot over time
str(bz_v5)

# Get patch-wide averages across the season
    ## Code also removes columns that make sense to record but aren't valuable in this context
    ## You can infer which of these columns those are because they are not listed here
bz_v6 <- aggregate(Number ~ Sampling.Event.ID + Site + SiteCode + Height + Genus + Bee.Species +
                   Herbicide.Treatment + Sociality + Nest.Type, data = bz_v5, FUN = sum)
str(bz_v6)

# Save this dataframe out
write.csv(bz_v6, "./Data/clean_2017bz.csv", row.names = F)

# Now mush to wide format (i.e. each species becomes a column populated by its abundances)
bz_wide <- spread(bz_v6, Bee.Species, Number, fill = 0)
str(bz_wide) # idiot check

# Smush calculated variables into main dataframe
bz_wide_v2 <- bz_wide
bz_wide_v2$Abundance <- as.vector(rowSums(bz_wide[, -c(1:7)]))
bz_wide_v2$Species.Density <- as.vector(specnumber(bz_wide[, -c(1:7)]))
bz_wide_v2$Diversity <- as.vector(diversity(bz_wide[, -c(1:7)], index = "shannon"))

# Final pre-save check
str(bz_wide_v2)

# Save for wide format
write.csv(bz_wide_v2, "./Data/clean_2017bz_wide.csv", row.names = F)

# Create an annual report variant (without sampling event ID or date)
ann.rep.v0 <- aggregate(Number ~ Site + SiteCode + Herbicide.Treatment + Bee.Species, data = bz_v6, FUN = sum)

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
bz <- read.csv("./Data/clean_2017bz.csv")
flr <- read.csv("./Data/clean_2017flr.csv")

# Re-level herbicide treatment column
bz$Herbicide.Treatment <- factor(as.character(bz$Herbicide.Treatment), levels = c("Ref", "Con", "Spr", "SnS"))
unique(bz$Herbicide.Treatment)

bz$Sociality <- factor(bz$Sociality, levels = c("Social", "Semi-Social", "Solitary"))
unique(bz$Sociality)

# Graphing shortcuts
nah <- element_blank()
bee.colors <- rev(topo.colors(length(unique(bz$Bee.Species))))
gen.colors <- rev(sp::bpy.colors(length(unique(bz$Genus))))
herb.colors <- c("Ref" = "#003c30", "Con" = "#01665e",
                 "Spr" = "#35978f", "SnS" = "#c7eae5")
mega.colors <- c("Ref-High" = "#003c30", "Ref-Low"  = "#543005",
                 "Con-High" = "#01665e", "Con-Low"  = "#8c510a",
                 "Spr-High" = "#35978f", "Spr-Low" = "#bf812d",
                 "SnS-High" = "#c7eae5", "SnS-Low"  = "#dfc27d")

# Useful summary dataframes
bz.ht <- aggregate(Number ~ Height + Bee.Species, data = bz, FUN = sum)
bz.gen <- aggregate(Number ~ Height + Genus, data = bz, FUN = sum)
bz.sp <- aggregate(Number ~ Bee.Species, data = bz, FUN = sum)

##  ----------------------------------------------------------  ##
    # High vs. Low Community Composition ####
##  ----------------------------------------------------------  ##
# Get a separate high and low dataframe
hi <- subset(bz.ht, bz.ht$Height == "High")
lo <- subset(bz.ht, bz.ht$Height == "Low")

# Low bees are missing some high bees and I want those absences in my plot down the line
  ## First get NAs in the same places
lo <- lo[match(hi$Bee.Species, lo$Bee.Species),]

  ## Next, fill in those pesky NAs in the number column with 0s
lo$Number
lo[is.na(lo$Number),3] <- 0
lo$Number

  ## Then overwrite the low bee "Bee.Species" column with the one from the high bees
lo$Bee.Species <- hi$Bee.Species
unique(lo$Bee.Species == hi$Bee.Species) # worked!

# Re-order them both by species abundances in the high communities
bee.levels <- as.vector(hi[order(hi$Number, decreasing = T), 2])
levels(hi$Bee.Species); levels(lo$Bee.Species)
hi$Bee.Species <- factor(as.character(hi$Bee.Species), levels = bee.levels)
lo$Bee.Species <- factor(as.character(lo$Bee.Species), levels = bee.levels)
levels(hi$Bee.Species); levels(lo$Bee.Species)

# Make the species rank plots
hi.spp.plt <- ggplot(hi, aes(x = Bee.Species, y = Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  ylim(low = 0, high = 65) +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  guides(fill = guide_legend(ncol = 4)) +
  theme(panel.grid = nah, axis.text.x = nah, legend.title = nah, legend.position = "top",
        legend.text = element_text(size = 5)); hi.spp.plt

# Get the legend as it's own object
bee.legend <- get_legend(hi.spp.plt)

# Now re-draw the high species plot without the legend
hi.spp.plt <- hi.spp.plt + theme(legend.position = "none"); hi.spp.plt

# Do the low plot now
lo.spp.plt <- ggplot(lo, aes(x = Bee.Species, y = Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  ylim(low = 0, high = 65) +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  theme(panel.grid = nah , legend.position = "none", axis.text.x = nah); lo.spp.plt

# Now do both plots and the legend in a single graphic
plot_grid(bee.legend, hi.spp.plt, lo.spp.plt,
          labels = c("", "High", "Low"), label_x = 0.8, nrow = 3, ncol = 1)

# And save this graphic
ggsave("./Graphs/bz_rankabun_2017.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
           # Bee Functional Diversity ####
##  ----------------------------------------------------------  ##
# Any interesting treatment patterns with either nesting habit or sociality?
ggplot(bz, aes(x = Herbicide.Treatment, y = Number, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = herb.colors) +
  labs(x = "Herbicide Treatment", y = "Number") +
  theme(panel.grid = nah, legend.position = "none") +
  facet_grid(Sociality ~ Nest.Type)

ggsave("Graphs/bz_fxn1_2017.pdf", plot = last_plot())

# Any of those patterns maintained with height info?
  ## Create a variable for use on the x-axis
bz$Combo.Var <- paste0(bz$Herbicide.Treatment, "-", bz$Height)
bz$Combo.Var <- factor(bz$Combo.Var, levels = c("Ref-High", "Ref-Low", "Con-High", "Con-Low",
                                                "Spr-High", "Spr-Low", "SnS-High", "SnS-Low"))
sort(unique(bz$Combo.Var))

  ## Plot it
ggplot(bz, aes(x = Combo.Var, y = Number, fill = Combo.Var)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = mega.colors) +
  labs(x = "Treatment * Height", y = "Bee Abundance") +
  theme(panel.grid = nah , axis.text.x = element_text(angle = -90)) +
  facet_grid(Sociality ~ Nest.Type)

ggsave("Graphs/bz_fxn2_2017.pdf", plot = last_plot())

# END ####
