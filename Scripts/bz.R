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
library(vegan); library(ggplot2); library(lme4); library(emmeans)

# Get cleaned data
bz <- read.csv("./Data/bz-wide.csv")

# Make years since burn a factor
bz$YSB <- as.factor(bz$YSB)
unique(bz$YSB)

# Get a dataframe for each round
unique(bz$Round)
bz.r1 <- subset(bz, bz$Round == "R1")
bz.r2 <- subset(bz, bz$Round == "R2")
bz.r3 <- subset(bz, bz$Round == "R3")

# Graphing shortcuts
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b") # shades of purple
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                               # Exploratory Visualization ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Visualize abundance data over rounds before analysis
bz.ab.avg <- Rmisc::summarySE(data = bz, measurevar = "Abundance", groupvars = c("YSB", "Round", "Height"))

ggplot(bz.ab.avg, aes(x = Round, y = Abundance, fill = YSB, color = YSB)) +
  geom_line(aes(group = YSB), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Abundance - se, ymax = Abundance + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = c(0.8, 0.9))

# And for species density
bz.dn.avg <- Rmisc::summarySE(data = bz, measurevar = "Species.Density",
                               groupvars = c("YSB", "Round", "Height"))

ggplot(bz.dn.avg, aes(x = Round, y = Species.Density, fill = YSB, color = YSB)) +
  geom_line(aes(group = YSB), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Species.Density - se, ymax = Species.Density + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = c(0.8, 0.9))

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Bee Round 1 Analysis ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Analysis
bz.r1.ab.mem <- glmer(Abundance ~ YSB * Height + 
                      (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r1, family = poisson)
summary(bz.r1.ab.mem)
  ## sig!

bz.r1.dn.mem <- glmer(Species.Density ~ YSB * Height +
                      (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r1, family = poisson)
summary(bz.r1.dn.mem)
  ## sig!

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
                              # Bee Round 2 Analysis ####
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
                              # Bee Round 3 Analysis ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Analysis
bz.r3.ab.mem <- glmer(Abundance ~ YSB * Height + 
                        (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r3, family = poisson)
summary(bz.r3.ab.mem)
## sig!

bz.r3.dn.mem <- glmer(Species.Density ~ YSB * Height +
                        (1|Bowl.Color) + (1|Site) + (1|Patch) + (1|Post.ID),
                      data = bz.r3, family = poisson)
summary(bz.r3.dn.mem)
## failed to converge

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

