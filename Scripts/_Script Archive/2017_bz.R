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

# Before you can move on to wide format stuff, you'll need to ditch some of this sociality stuff
bz_v7 <- bz_v6[,-c(5, 8:9)]

# Now mush to wide format (i.e. each species becomes a column populated by its abundances)
bz_wide <- spread(bz_v7, Bee.Species, Number, fill = 0)
str(bz_wide) # idiot check

# Smush calculated variables into main dataframe
bz_wide_v2 <- bz_wide
bz_wide_v2$Abundance <- as.vector(rowSums(bz_wide[, -c(1:5)]))
bz_wide_v2$Species.Density <- as.vector(specnumber(bz_wide[, -c(1:5)]))
bz_wide_v2$Diversity <- as.vector(diversity(bz_wide[, -c(1:5)], index = "shannon"))

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

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Explore the Data ####
##  ----------------------------------------------------------------------------------------------------------  ##
rm(list = ls())

# Data
bz.1 <- read.csv("./Data/clean_2017bz_wide.csv")

# Ditch the community metrics and non-patch specific columns
bz.2 <- bz.1[,-c(1, (ncol(bz.1)-2):ncol(bz.1))]

# Get it in long format so that all species are represented (even when 0 of that species was found there)
bz <- gather(bz.2, key = "Bee.Species", value = "Number", 5:20)
count(bz$Bee.Species)
str(bz)

# Re-level treatment, patch, and site
bz$Herbicide.Treatment <- factor(as.character(bz$Herbicide.Treatment),
                                 levels = c("Ref", "Con", "Spr", "SnS"))
unique(bz$Herbicide.Treatment)
bz$SiteCode <- factor(bz$SiteCode,
                      levels = c("GIL-S", "GIL-N", "GIL-C", "LTR-W","LTR-C", "LTR-E", 
                                 "STE-W","STE-N", "STE-S", "KLN-E", "PYN-N", "RIS-S"))
unique(bz$SiteCode)
bz$Site <- factor(bz$Site, levels = c("GIL", "LTR", "STE", "KLN", "PYN", "RIS"))
unique(bz$Site)

# We'll want rank abundance so let's re-level the bee species column to match that order
bz.sp <- aggregate(Number ~ Bee.Species, data = bz, FUN = sum)
bee.order <- as.vector(bz.sp[order(bz.sp$Number, decreasing = T), 1])
bz$Bee.Species <- factor(bz$Bee.Species, levels = bee.order)
levels(bz$Bee.Species)

# Graphing shortcuts
nah <- element_blank()
bee.colors <- rev(topo.colors(length(unique(bz$Bee.Species))))
herb.colors <- c("Ref" = "#003c30", "Con" = "#01665e",
                 "Spr" = "#35978f", "SnS" = "#c7eae5")

##  ----------------------------------------------------------  ##
    # High vs. Low Community Composition ####
##  ----------------------------------------------------------  ##
# Get a dataframe that only includes height and species
  ## push it to wide in one fell swoop because we don't really care about long format here
bz.ht <- spread((aggregate(Number ~ Height + Bee.Species, data = bz, FUN = sum)), Height, Number, fill = 0)

# Draw a bad plot to strip the legend from
leg.plt <- ggplot(bz, aes(x = Bee.Species, y = Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  guides(fill = guide_legend(ncol = 3)) +
  theme(panel.grid = nah, axis.text.x = nah, legend.title = nah, legend.position = "top",
        legend.text = element_text(size = 7)); leg.plt

# Get the legend as it's own object
bee.legend <- get_legend(leg.plt)

# Make the species rank plots
hi.spp.plt <- ggplot(bz.ht, aes(x = Bee.Species, y = High, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  ylim(low = 0, high = 65) +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  theme(panel.grid = nah, axis.text.x = nah, legend.title = nah, legend.position = "none"); hi.spp.plt

# Do the low plot now
lo.spp.plt <- ggplot(bz.ht, aes(x = Bee.Species, y = Low, fill = Bee.Species)) +
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
          # Among Patch Heterogeneity ####
##  ----------------------------------------------------------  ##
# Plot rank abundance and facet by patch
ggplot(bz, aes(x = Bee.Species, y = Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  facet_grid(SiteCode ~ .)+
  theme(panel.grid = nah, axis.text.x = nah, legend.position = "none")

## Not super easy to read, but basically it looks like patches are more similar to patches from the same site
  ## than they are to patches in other sites...

# Let's check site-to-site variation
ggplot(bz, aes(x = Bee.Species, y = Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  facet_grid(Site ~ .)+
  theme(panel.grid = nah, axis.text.x = nah, legend.position = "none")

##  ----------------------------------------------------------  ##
       # Among Treatment Differences ####
##  ----------------------------------------------------------  ##
# Despite the above result, are there any patterns in treatment rank abundances?
ggplot(bz, aes(x = Bee.Species, y = Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = bee.colors) +
  labs(x = "", y = "Number") +
  facet_grid(Herbicide.Treatment ~ .)+
  theme(panel.grid = nah, axis.text.x = nah, legend.position = "none")

##  ----------------------------------------------------------  ##
          # Bee Functional Diversity ####
##  ----------------------------------------------------------  ##
# We will actually be needing a different dataframe to address this
bz.lng <- read.csv("./Data/clean_2017bz.csv")

# Aggregate to get just functional diversity info
bz.fxn <- aggregate(Number ~ Herbicide.Treatment + Sociality + Nest.Type + Height + Bee.Species,
                    data = bz.lng, FUN = sum)

# Re-level the factors that need it
bz.fxn$Herbicide.Treatment <- factor(as.character(bz.fxn$Herbicide.Treatment),
                                     levels = c("Ref", "Con", "Spr", "SnS"))
bz.fxn$Sociality <- factor(bz.fxn$Sociality,
                           levels = c("Social", "Semi-Social", "Solitary"))
unique(bz.fxn$Herbicide.Treatment); unique(bz.fxn$Sociality)

# Any interesting treatment patterns with either nesting habit or sociality?
ggplot(bz.fxn, aes(x = Herbicide.Treatment, y = Number, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = herb.colors) +
  labs(x = "Herbicide Treatment", y = "Number") +
  theme(panel.grid = nah, legend.position = "none") +
  facet_grid(Sociality ~ Nest.Type)

ggsave("Graphs/bz_fxn_2017.pdf", plot = last_plot())

# Any of those patterns maintained with height info?
## Create a variable for use on the x-axis
bz.fxn$Combo.Var <- paste0(bz.fxn$Herbicide.Treatment, "-", bz.fxn$Height)
bz.fxn$Combo.Var <- factor(bz.fxn$Combo.Var, levels = c("Ref-High", "Ref-Low", "Con-High", "Con-Low",
                                                        "Spr-High", "Spr-Low", "SnS-High", "SnS-Low"))
sort(unique(bz.fxn$Combo.Var))

# Get a color shortcut for your new variable
mega.colors <- c("Ref-High" = "#003c30", "Ref-Low"  = "#543005",
                 "Con-High" = "#01665e", "Con-Low"  = "#8c510a",
                 "Spr-High" = "#35978f", "Spr-Low" = "#bf812d",
                 "SnS-High" = "#c7eae5", "SnS-Low"  = "#dfc27d")

## Plot it
ggplot(bz.fxn, aes(x = Combo.Var, y = Number, fill = Combo.Var)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = mega.colors) +
  labs(x = "Treatment * Height", y = "Bee Abundance") +
  theme(panel.grid = nah , axis.text.x = element_text(angle = -90)) +
  facet_grid(Sociality ~ Nest.Type)



# END ####
