##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                    # Lyon Thesis -- Chapter 2: "Bees"
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE
  ## Two questions from 2018 bee data
    ### 1. Do bee communities vary among patches of patch-burn grazed (PBG) sites? And if so, how?
    ### 2. Do bees respond to the fescue treatments at the patch level? And if so, how?

# START ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(ggplot2); library(lme4); library(emmeans)

# Get cleaned data
bz <- read.csv("./Data/actual_bz18.csv")

# Make years since burn a factor
bz$YSB <- as.factor(bz$YSB)
unique(bz$YSB)

# Re-level the "Herb.Trt" column
bz$Herb.Trt <- factor(as.character(bz$Herb.Trt), levels = c("Con", "Spr", "SnS", "Ref", "x"))
unique(bz$Herb.Trt)

# Get the floral data and do the same thing
flr <- read.csv("./Data/actual_bzflr18.csv")
flr$YSB <- as.factor(flr$YSB)
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS", "Ref", "x"))
unique(flr$YSB); unique(flr$Herb.Trt)
  ## Because all rounds were collected in the same way, no need for subsetting by round

# Get PBG and SnS separated for both taxa
pbg <- subset(bz, bz$Adaptive.Mgmt == "PBG")
sns <- subset(bz, bz$Adaptive.Mgmt == "GB")
pbg.flr <- subset(flr, flr$Adaptive.Mgmt == "PBG")
sns.flr <- subset(flr, flr$Adaptive.Mgmt == "GB")

# Graphing shortcuts
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b", # purples
            "Con" = "#00441b",  "Spr" = "#1b7837", "SnS" = "#5aae61") # greens (Ref = 0 YSB)
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Patch-Burn Graze Question ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get a dataframe for each round
unique(pbg$Round)
pbg.r1 <- subset(pbg, pbg$Round == "R1")
pbg.r2 <- subset(pbg, pbg$Round == "R2")
pbg.r3 <- subset(pbg, pbg$Round == "R3")

# Visualize the data before analysis!
pbg.avg <- Rmisc::summarySE(data = pbg, measurevar = "Abundance",
                            groupvars = c("YSB", "Round", "Height"))

ggplot(pbg.avg, aes(x = Round, y = Abundance, fill = YSB, color = YSB)) +
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
pbg.avg <- Rmisc::summarySE(data = pbg, measurevar = "Species.Density",
                            groupvars = c("YSB", "Round", "Height"))

ggplot(pbg.avg, aes(x = Round, y = Species.Density, fill = YSB, color = YSB)) +
  geom_line(aes(group = YSB), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Species.Density - se, ymax = Species.Density + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = c(0.8, 0.9))

##  ----------------------------------------------------------  ##
   # Round 1 Bees - PBG Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis
pbg.r1.ab.mem <- glmer(Abundance ~ YSB * Height + (1|Patch) + (1|Bowl.Color), data = pbg.r1, family = poisson)
summary(pbg.r1.ab.mem)

pbg.r1.dn.mem <- glmer(Species.Density ~ YSB * Height + (1|Patch) + (1|Bowl.Color), data = pbg.r1, family = poisson)
summary(pbg.r1.dn.mem) ## NS

# Plotting
ggplot(pbg.r1, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(pbg.r1, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------  ##
  # Round 2 Bees - PBG Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis
pbg.r2.ab.mem <- glmer(Abundance ~ YSB * Height + (1|Patch) + (1|Bowl.Color), data = pbg.r2, family = poisson)
summary(pbg.r2.ab.mem)
  ## Low < High

pbg.r2.dn.mem <- glmer(Species.Density ~ YSB * Height + (1|Patch) + (1|Bowl.Color), data = pbg.r2, family = poisson)
summary(pbg.r2.dn.mem)
  ## Low < High

# Plotting
ggplot(pbg.r2, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(pbg.r2, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------  ##
   # Round 3 Bees - PBG Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis
pbg.r3.ab.mem <- glmer(Abundance ~ YSB * Height + (1|Patch) + (1|Bowl.Color), data = pbg.r3, family = poisson)
summary(pbg.r3.ab.mem)

pbg.r3.dn.mem <- glmer(Species.Density ~ YSB * Height + (1|Patch) + (1|Bowl.Color), data = pbg.r3, family = poisson)
summary(pbg.r3.dn.mem) ## Low < High

# Plotting
ggplot(pbg.r3, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(pbg.r3, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------  ##
    # Floral Data -  PBG Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analyze!
pbg.flr.ab.mem <- glmer(Abundance ~ YSB * Round + (1|Patch), data = pbg.flr, family = poisson)
summary(pbg.flr.ab.mem)
  ## R1-YSB 0 = A | R1-YSB 1 = A | R1-YSB 2 = A
  ## R2-YSB 0 = A | R2-YSB 1 = B | R2-YSB 2 = B
  ## R3-YSB 0 = A | R3-YSB 1 = B | R3-YSB 2 = B

pbg.flr.dn.mem <- glmer(Species.Density ~ YSB * Round + (1|Patch), data = pbg.flr, family = poisson)
summary(pbg.flr.dn.mem) ## NS

# Plotting
ggplot(pbg.flr, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Floral Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Round ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(pbg.flr, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Floral Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Round ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Spray and Seed Question ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get a dataframe for each round
unique(sns$Round)
sns.r1 <- subset(sns, sns$Round == "R1")
sns.r3 <- subset(sns, sns$Round == "R3")

# Do some visualization of the full dataset
sns.avg <- Rmisc::summarySE(data = sns, measurevar = "Abundance",
                            groupvars = c("Herb.Trt", "Round", "Height"))

ggplot(sns.avg, aes(x = Round, y = Abundance, fill = Herb.Trt, color = Herb.Trt)) +
  geom_line(aes(group = Herb.Trt), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Abundance - se, ymax = Abundance + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = c(0.8, 0.9))

# And for species density
sns.avg <- Rmisc::summarySE(data = sns, measurevar = "Species.Density",
                            groupvars = c("Herb.Trt", "Round", "Height"))

ggplot(sns.avg, aes(x = Round, y = Species.Density, fill = Herb.Trt, color = Herb.Trt)) +
  geom_line(aes(group = Herb.Trt), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Species.Density - se, ymax = Species.Density + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = c(0.8, 0.9))

##  ----------------------------------------------------------  ##
 # Round 1 Bees - Herbicide Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis
sns.r1.ab.mem <- glmer(Abundance ~ Herb.Trt * Height + (1|Patch) + (1|Bowl.Color), data = sns.r1, family = poisson)
summary(sns.r1.ab.mem) # low < high (regardless of treatment)

sns.r1.dn.mem <- glmer(Species.Density ~ Herb.Trt * Height + (1|Patch) + (1|Bowl.Color), data = sns.r1, family = poisson)
summary(sns.r1.dn.mem) ## NS

# Plotting
ggplot(sns.r1, aes(x = Herb.Trt, y = Abundance, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(sns.r1, aes(x = Herb.Trt, y = Species.Density, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------  ##
# Round 3 Bees - Herbicide Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis
sns.r3.ab.mem <- glmer(Abundance ~ Herb.Trt * Height + (1|Patch) + (1|Bowl.Color), data = sns.r3, family = poisson)
summary(sns.r3.ab.mem) # low < high (regardless of treatment)

sns.r3.dn.mem <- glmer(Species.Density ~ Herb.Trt * Height + (1|Patch) + (1|Bowl.Color), data = sns.r3, family = poisson)
summary(sns.r3.dn.mem) ## NS

# Plotting
ggplot(sns.r3, aes(x = Herb.Trt, y = Abundance, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(sns.r3, aes(x = Herb.Trt, y = Species.Density, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Bee Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")

##  ----------------------------------------------------------  ##
  # Floral Data -  Herbicide Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analyze!
sns.flr.ab.mem <- glmer(Abundance ~ Herb.Trt * Round + (1|Patch), data = sns.flr, family = poisson)
summary(sns.flr.ab.mem)
  ## R1-Con = A | R1-Spr = A | R1-SnS = A
  ## R3-Con = B | R3-Spr = B | R3-SnS = B

sns.flr.dn.mem <- glmer(Species.Density ~ Herb.Trt * Round + (1|Patch), data = sns.flr, family = poisson)
summary(sns.flr.dn.mem) ## NS

# Plotting
ggplot(sns.flr, aes(x = Herb.Trt, y = Abundance, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Floral Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Round ~ .) +
  pref.theme + theme(legend.position = "none")

ggplot(sns.flr, aes(x = Herb.Trt, y = Species.Density, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Floral Species Density") + 
  scale_fill_manual(values = colors) +
  facet_grid(Round ~ .) +
  pref.theme + theme(legend.position = "none")




# END ####

