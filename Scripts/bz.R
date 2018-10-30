##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                               # Lyon Thesis -- Chapter 2: Pollinators and PBG
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Big-Picture Question:
  ## How do pollinator communities (e.g. bee and butterfly) and their floral resources
  ## vary among  patches of patch-burn graze (hereafter PBG) sites?

# Script taxon: BEES

# START ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(tidyr); library(Rmisc); library(ggplot2); library(lme4); library(emmeans)

# Graphing shortcuts
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b") # shades of purple
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())

# QUESTION (that I need to ask myself) ####
# If I think the rare bees are adding to my overdispersion, and remove them bcz of that,
# what do I gain from an aggregate "common bees" abundance that I don't get out of single-sp work?

# Maybe visit Katie again and ask about single species stuff?
  ## I.e. if overdispersion is a non-issue, what other statistical stuff do I need to consider

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Data Prep ####
##  ----------------------------------------------------------------------------------------------------------  ##
# NOTE ON DATA
  ## Due to a high frequency of rare species (i.e. many species that occur in <10 samples)
  ## --and the subsequent overdispersion of analyses including them--
  ## The data used for analysis focuses only on these "Common" bees
  ## Don't believe me? Check out the species totals for bees
bz.tots <- read.csv("./Species Totals/bz.spp.csv")
bz.tots
  ## Let's use only bee species that account for ≥10% of the total bees observed, so:

# Now read in the clean data
bz.cln <- read.csv("./Data/bz-long.csv")

# Subset for only the common bees
bz.common.cln <- subset(bz.cln, bz.cln$Bee.Species == "Agapostemon.virescens" |
                      bz.cln$Bee.Species == "Lasioglossum.sp" | 
                      bz.cln$Bee.Species == "Augochlorella.aurata" |
                      bz.cln$Bee.Species == "Ceratina.dupla" | 
                      bz.cln$Bee.Species == "Halictus.ligatus" |
                      bz.cln$Bee.Species == "X.x")

# Get the common bee frame into wide format
bz <- spread(key = Bee.Species, value = Number, fill = 0, data = bz.common.cln)

# Calculate abundance 
  ## Species density doesn't mean a whole lot now that we removed most of the species
bz$Abundance <- rowSums(bz[,-c(1:8)])

# Make years since burn a factor
bz$YSB <- as.factor(bz$YSB)
unique(bz$YSB)

# Get a dataframe for each round
unique(bz$Round)
bz.r1 <- subset(bz, bz$Round == "R1")
bz.r2 <- subset(bz, bz$Round == "R2")
bz.r3 <- subset(bz, bz$Round == "R3")

# Get a secondary dataframe for the rare bees (to qualitatively report)
bz.rare.v0 <- subset(bz.cln, bz.cln$Bee.Species != "Agapostemon.virescens" &
                    bz.cln$Bee.Species != "Lasioglossum.sp" & 
                    bz.cln$Bee.Species != "Augochlorella.aurata" &
                    bz.cln$Bee.Species != "Ceratina.dupla" & 
                    bz.cln$Bee.Species != "Halictus.ligatus" &
                    bz.cln$Bee.Species != "X.x")

# Sum through everything but YSB
bz.rare.cln <- aggregate(Number ~ YSB + Bee.Species, data = bz.rare.v0, FUN = sum)

# Push it to a more intuitive table format (i.e. wide format)
bz.rare <- spread(key = YSB, value = Number, fill = NA, data = bz.rare.cln)

# Save 
write.csv(bz.rare, "./Summary Info/rare-bz.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                                 # Bee Round 1 ####
##  ----------------------------------------------------------------------------------------------------------  ##

# Stats-Recommended Model
glmer(Abundance ~ Height * YSB + Site:Height + Site:YSB + Site + Bowl.Color +
        (1|Patch) + (1|Post.ID) + (1|Observation), data = bz.r1, family = poisson)


# Analysis
bz.r1.ab.mem <- glmer(Abundance ~ YSB * Height + 
                      (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = test, family = poisson)
summary(bz.r1.ab.mem)
  ## sig!

plyr::count(bz.r1$Abundance)

bz.r1.dn.mem <- glmer(Species.Density ~ YSB * Height +
                      (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r1, family = poisson)
summary(bz.r1.dn.mem)
  ## Variance explained by Post.ID and Patch is essentially 0, so drop 'em

# Re-do
bz.r1.dn.mem <- glmer(Species.Density ~ YSB * Height +
                      (1|Bowl.Color) + (1|Site) + (1|factor(1:nrow(bz.r1))),
                      data = bz.r1, family = poisson)
summary(bz.r1.dn.mem)

# Plotting
ggplot(bz.r1, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(bz.r1, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------------------------------------------------------  ##
                                   # Bee Round 2 ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Analysis
bz.r2.ab.mem <- glmer(Abundance ~ YSB * Height + 
                        (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r2, family = poisson)
summary(bz.r2.ab.mem)
  ## sig!

bz.r2.dn.mem <- glmer(Species.Density ~ YSB * Height +
                        (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r2, family = poisson)
summary(bz.r2.dn.mem)
  ## failed to converge

plyr::count(bz.r2$Species.Density)

# Plotting
ggplot(bz.r2, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(bz.r2, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Bee Round 3 ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Not enough non-zero data to have meaningful analyses, so instead will just get summary values
aggregate(Abundance ~ YSB + Height, FUN = sum, data = bz.r3)
summarySE(data = bz.r3, measurevar = "Species.Density", groupvars = c("YSB", "Height"))




# Plotting
ggplot(bz.r3, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(bz.r3, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

# END ####

