##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                           # Lyon Thesis -- Chapter 2: Pollinators and PBG
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Big-Picture Question:
  ## How do pollinator communities (e.g. bee and butterfly) and their floral resources
  ## vary among  patches of patch-burn graze (hereafter PBG) sites?

# Script taxon: BUTTERFLIES

# START ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(ggplot2); library(lme4); library(emmeans)

# Get cleaned data
bf <- read.csv("./Data/bf-wide.csv")
str(bf)

# Make years since burn a factor
bf$YSB <- as.factor(bf$YSB)
unique(bf$YSB)

# Graphing shortcuts
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b") # shades of purple
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                            # Butterfly Analysis & Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Due to the use of mixed-effects models, will need to re-level "YSB" to perform pairwise comparisons

# Re-level the YSB factor
sort(unique(bf$YSB))
bf$YSB <- factor(bf$YSB, levels = c(1, 0, 2))
sort(unique(bf$YSB))

# Re-analyze to get 1 vs. 2 YSB comparison
bf.ab.mem.base1 <- glmer(Abundance ~ YSB +
                           (1|Site) + (1|Patch) + (1|Date),
                         data = bf, family = poisson)
summary(bf.ab.mem.base1)

bf.dn.mem.base1 <- glmer(Species.Density ~ YSB +
                           (1|Site) + (1|Patch) + (1|Date),
                         data = bf, family = poisson)
summary(bf.dn.mem.base1)

# Manually re-set leveling
bf$YSB <- factor(bf$YSB, levels = c(0, 1, 2))
sort(unique(bf$YSB))

# Analyze
bf.ab.mem.base0 <- glmer(Abundance ~ YSB +
                  (1|Site) + (1|Patch) + (1|Date),
                  data = bf, family = poisson)
summary(bf.ab.mem.base0)

bf.dn.mem.base0 <- glmer(Species.Density ~ YSB +
                  (1|Site) + (1|Patch) + (1|Date),
                  data = bf, family = poisson)
summary(bf.dn.mem.base0)

# Connected letter diagram?
## Abundance
# 0 YSB = A | 1 = A | 2 = A

## Species Density
# 0 YSB = A | 1 = A | 2 = A

# Visualize the differences in interior floral resources
ggplot(bf, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Butterfly Abundance") + 
  scale_fill_manual(values = colors) +
  pref.theme + theme(legend.position = "none")

ggplot(bf, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Butterfly Density") + 
  scale_fill_manual(values = colors) +
  pref.theme + theme(legend.position = "none")

# END ####

