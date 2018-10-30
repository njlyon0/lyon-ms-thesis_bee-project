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

# QUESTION (that I need to ask myself) ####
# If I think the rare bees are adding to my overdispersion, and remove them bcz of that,
# what do I gain from an aggregate "common bees" abundance that I don't get out of single-sp work?

# Maybe visit Katie again and ask about single species stuff?
  ## I.e. if overdispersion is a non-issue, what other statistical stuff do I need to consider

##  -----------------------------------------------------------------------------  ##
                # Background Data Exploration ####
##  -----------------------------------------------------------------------------  ##
# DATA NOTE
  ## The bee dataset is characterized by a few hyper-abundant species and many rare species.
  ## This contributes to overdisperson and model failures to converge.
  ## By quantifying this variation we can better account for it/modify questions accordingly

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

##  -----------------------------------------------------------------------------  ##
              # "Common" versus "Rare" Bee Species
##  -----------------------------------------------------------------------------  ##
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

##  ----------------------------------------------------------------------------------------------------------  ##
                               # Data Exploration ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the ordination stuff from the other scripts into this section


##  ----------------------------------------------------------------------------------------------------------  ##
                            # Analysis & Plotting Prep ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear the environment
rm(list = ls())
  ## Step is vital due to earlier work
  
# Get the clean data from the common bees
bz <- read.csv("./Data/bz-wide-common.csv")

# Get a dataframe for each round
unique(bz$Round)
bz.r1 <- subset(bz, bz$Round == "R1")
bz.r2 <- subset(bz, bz$Round == "R2")
bz.r3 <- subset(bz, bz$Round == "R3")

# ggplot graphing shortcut calls
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b") # shades of purple
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())


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

