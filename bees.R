##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                        # Lyon Thesis -- Bee Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Due to unequal sampling intensity across the flowering phenology (I hate cows) these data are not analyzable
# Cleaning and summary is largely done for exploratory purposes
# Analysis is entirely excluded

# Clear environment to reduce error chances
rm(list = ls())

# Set WD
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Chaps1&2-GRG")

# Required libraries
library(tidyr); library(stringr); library(geomorph); library(vegan); library(ggplot2); library(cowplot)

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Cleaning and Response Calculation
##  ----------------------------------------------------------------------------------------------------------  ##

# Index file
beetreat <- read.csv("./Indeces/trmntinfo.csv")
julindex <- read.csv("./Indeces/julianinfo.csv")

##  ----------------------------------------------------------  ##
      # Bee Cleaning and Response Calculation
##  ----------------------------------------------------------  ##
# Read in data
bz <- read.csv("Data/Raw/bz17_raw.csv")

# Remove posterity rows (row created when site had no bees at that height)
bz_v2 <- bz[!(bz$Number == 0),]

# also all PYW and PYS sites (transect sampling discontinued due to bovine a-holes)
bz_v2 <- bz_v2[!(bz_v2$Site == "PYS" | bz_v2$Site == "PYW"),]

# Also remove bees from transects where fewer than 5 bowls were collected
bz_v2 <- subset(bz_v2, bz_v2$Bowls.Recovered >= 5)

# Hopefully there are no spelling errors, but check to be sure
sort(unique(bz_v2$Genus))
sort(unique(bz_v2$Species))

# Need to create one column of bee species names
bz_v2$Bee.Species <- paste0(bz_v2$Genus, ".", bz_v2$Species)
sort(unique(bz_v2$Bee.Species))

# Remove any accidental rows where bee genus/species were left blank
bz_v2 <- bz_v2[!(bz_v2$Bee.Species == "."),]

# Add in treatment column and Julian date column
bz_v2$Fescue.Treatment <- beetreat$Fescue.Treatment[match(bz_v2$SiteCode, beetreat$DataCode)]
bz_v2$Julian <- julindex$Julian[match(bz_v2$Capture.Date, julindex$Date)]
str(bz_v2)

# Insidious, but I have a space after "High" one time that will make it be treated as a different group
sort(unique(as.character(bz_v2$Height)))
bz_v2$Height <- gsub("High ", "High", bz_v2$Height)
sort(unique(as.character(bz_v2$Height)))

# Sum through just patches
    ## (code also removes columns that make sense to record but aren't valuable in this context)
bz_v3 <- aggregate(Number ~ Sampling.Event.ID + Capture.Year + Julian + SiteCode + Height + Fescue.Treatment + Bee.Species,
                   data = bz_v2, FUN = sum)
str(bz_v3)

# Save this dataframe out
write.csv(bz_v3, "./Data/Curiosity/bz_roughyear.csv", row.names = F)

# Now mush to wide format
bz_wide <- spread(bz_v3, Bee.Species, Number, fill = 0)
str(bz_wide) # idiot check

# Calculate response variables
bzabun <- as.vector(rowSums(bz_wide[, -c(1:6)]))
bzdens <- as.vector(specnumber(bz_wide[, -c(1:6)]))
bzdive <- as.vector(diversity(bz_wide[, -c(1:6)], index = "shannon"))

# Smush calculated variables into main dataframe
bz_wide_v2 <- bz_wide
bz_wide_v2$Abundance <- bzabun
bz_wide_v2$Species.Density <- bzdens
bz_wide_v2$Diversity <- bzdive

# Final pre-save check
str(bz_wide_v2)

# Save for wide format
write.csv(bz_wide_v2, "./Data/Curiosity/bz_roughyear_wide.csv", row.names = F)

##  ----------------------------------------------------------  ##
    # Nectar Cleaning and Response Calculation
##  ----------------------------------------------------------  ##
# Data file
nec <- read.csv("Data/Raw/bznec17_raw.csv")

# Sum section counts together for full transect counts
nec$TransectTotals <- rowSums(nec[,8:12])
nec_v2 <- nec[,-c(8:12)]

# Might as well add treatment labels (could be nice to actually answer the question at hand?)
nec_v2$Fescue.Treatment <- beetreat$Fescue.Treatment[match(nec_v2$SiteCode, beetreat$DataCode)]
nec_v2$Julian <- julindex$Julian[match(nec_v2$Capture.Date, julindex$Date)]

# Reorder so the treatment column is nicely with all the other explanatory stuff
str(nec_v2)
nec_v3 <- nec_v2[,c(1:2, 13, 6, 12, 7, 11)]
str(nec_v3)

# Check nectar common names
nec_v3$Nectar.Common.Name <- tolower(nec_v3$Nectar.Common.Name)
sort(unique(nec_v3$Nectar.Common.Name))

# Remove foolish empty rows/unspecific "species" references
nec_v4 <- nec_v3[!(nec_v3$Nectar.Common.Name == ""),]

# Standardize any names that are redundant (multiple common names that refer to the same species)
nec_v4$Nectar.Common.Name <- gsub("lance-leafed plantain", "ribwort plantain", nec_v4$Nectar.Common.Name)
nec_v4$Nectar.Common.Name <- gsub("lance leaf plantain", "ribwort plantain", nec_v4$Nectar.Common.Name)
nec_v4$Nectar.Common.Name <- gsub("common plantain", "broadleaf plantain", nec_v4$Nectar.Common.Name)
nec_v4$Nectar.Common.Name <- gsub("bee balm", "bergamot", nec_v4$Nectar.Common.Name)
sort(unique(nec_v4$Nectar.Common.Name))

# Idiot check and save clean version
str(nec_v4)
write.csv(nec_v4, "./Data/Clean/bzflr_clean.csv", row.names = F)

# Wide format
nec_wide <- spread(nec_v4, Nectar.Common.Name, TransectTotals, fill = 0)
str(nec_wide)

# Calculate response variables
necabun <- as.vector(rowSums(nec_wide[,-c(1:5)]))
necdens <- as.vector(specnumber(nec_wide[,-c(1:5)]))
necdive <-  as.vector(diversity(nec_wide[,-c(1:5)], index = "shannon"))

# Push to new wide dataframe and add calculated response variables
nec_wide_v2 <- nec_wide
nec_wide_v2$Abundance <- necabun
nec_wide_v2$Species.Density <- necdens
nec_wide_v2$Diversity <- necdive

# Final pre-save check
str(nec_wide_v2)

# Save
write.csv(nec_wide_v2, "./Data/Ready/edge_nec_ready.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Explore the Data
##  ----------------------------------------------------------------------------------------------------------  ##
rm(list = ls())

# Data
bz <- read.csv("./Data/Curiosity/bz_roughyear.csv")
flr <- read.csv("./Data/Ready/edge_nec_ready.csv")

# Plotting shortcuts
treat.labs <- c("Ref", "Con", "Spr", "SnS")
treat.colors <- c("Ref" = "#0868ac", "Con" = "#43a2ca", "Spr" = "#7bccc4", "SnS" = "#bae4bc", "Total" = "#252525")

##  ----------------------------------------------------------  ##
     # Species Abundances of Preliminary Year
##  ----------------------------------------------------------  ##
# Get down to just raw species totals in either high or low
bz.simp <- aggregate(Number ~ Height + Bee.Species, FUN = sum, data = bz)

flr.lng <- gather(flr, key = "Floral.Species", value = "Number", 
                  colnames(flr[,-c(1:5 , (ncol(flr)-2):(ncol(flr)) )]))

flr.simp <- aggregate(Number ~ Floral.Species, FUN = sum, data = flr.lng)

# Want to know how many of each species were found in either high or low bowls in the preliminary study
bz.specabun <- ggplot(bz.simp, aes(Bee.Species, Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rev(sp::bpy.colors(length(unique(bz.simp$Bee.Species))))) +
  facet_grid(Height ~ .) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.title = element_blank()); bz.specabun

jpeg("./Graphs/Bee Curiosity/Species Abundances.jpg")
bz.specabun
dev.off()

# Plot the floral species' total abundances to get a feel for what is up
flr.specabun <- ggplot(flr.simp, aes(Floral.Species, Number, fill = Floral.Species)) +
  geom_bar(stat = 'identity') +
  ylab("Number Ramets") +
  scale_fill_manual(values = rev(topo.colors(length(unique(flr.simp$Floral.Species))))) +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        legend.title = element_blank(), legend.position = "none"); flr.specabun

##  ----------------------------------------------------------  ##
            # Among Patch Differences
##  ----------------------------------------------------------  ##
# Get patch-specific sums (ditching date and height)
bz.ptch <- aggregate(Number ~ SiteCode + Bee.Species, data = bz, FUN = sum)

# Get pasture column & combo height + patch column
bz.ptch$Pasture <- as.factor(stringr::str_sub(bz.ptch$SiteCode, 1, 3))

# Ditch reference sites
bz.ptch2 <- subset(bz.ptch, bz.ptch$Pasture != "KLN" & bz.ptch$Pasture != "RIS" & bz.ptch$Pasture != "PYN")

# Plot
ptch.df.plt <- ggplot(bz.ptch2, aes(SiteCode, Number, fill = Bee.Species)) +
  geom_bar(stat = 'identity') +
  facet_grid(Pasture ~ .) +
  scale_fill_manual(values = rev(sp::bpy.colors(length(unique(bz.ptch2$Bee.Species))))) +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        legend.title = element_blank()); ptch.df.plt
  
jpeg("./Graphs/Bee Curiosity/Patch Differences.jpg")
ptch.df.plt
dev.off()


