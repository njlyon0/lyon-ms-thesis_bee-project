### --------------------------------------------------- ##
# Lyon MS Thesis - Native Bee Project - Bee Data
## --------------------------------------------------- ##
# Written by Nicholas L Lyon

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, vegan, AICcmodavg, lme4, emmeans, njlyon0/helpR)

# Clear environment
rm(list = ls())

## --------------------------------------------------- ##
# Data Wrangling - Broad ----
## --------------------------------------------------- ##
# Load data
flowers_v0 <- read.csv(file.path("data", "bee-proj_ALL_floral-long.csv"))

# Take a look
dplyr::glimpse(flowers_v0)

# Do some filtering
flowers_v1 <- flowers_v0 %>% 
  ## Remove 2017 flowers
  dplyr::filter(Capture_Year != 2017)
  
# Check it out
dplyr::glimpse(flowers_v1)

# Create a plotting shortcut
flower_theme <- theme_classic() + 
  theme(legend.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))













# End ----


# Get cleaned data
bz.flr <- read.csv("./Data/bz-flr-wide.csv")
bf.flr <- read.csv("./Data/bf-flr-wide.csv")

# Make years since burn a factor
bz.flr$YSB <- as.factor(bz.flr$YSB)
bf.flr$YSB <- as.factor(bf.flr$YSB)
unique(bz.flr$YSB); unique(bf.flr$YSB)

# Graphing shortcuts
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b") # shades of purple
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                            # Floral Data - FROM BEE POSTS ####
##  ----------------------------------------------------------------------------------------------------------  ##
# These data were collected in 1m radius circles around bee posts
  ## Bee posts are located exclusively along the fenceline of patches, so:
  ## Hereafter these floral resources may be refered to as "edge floral resources"

# Analyze!
bz.flr.ab.mem <- glmer(Abundance ~ YSB * Round +
                      (1|Site) + (1|Patch),
                      data = bz.flr, family = poisson)
summary(bz.flr.ab.mem)
## R1-YSB 0 = A | R1-YSB 1 = A | R1-YSB 2 = A
## R2-YSB 0 = A | R2-YSB 1 = B | R2-YSB 2 = B
## R3-YSB 0 = A | R3-YSB 1 = B | R3-YSB 2 = B

bz.flr.dn.mem <- glmer(Species.Density ~ YSB * Round +
                      (1|Site) + (1|Patch),
                      data = bz.flr, family = poisson)
summary(bz.flr.dn.mem) ## NS

# Get plot-able summary dataframes
bz.flr.ab.avg <- Rmisc::summarySE(data = bz.flr, measurevar = "Abundance", groupvars = c("YSB", "Round"))
bz.flr.dn.avg <- Rmisc::summarySE(data = bz.flr, measurevar = "Species.Density", groupvars = c("YSB", "Round"))

# Visualize the floral differences!
ggplot(bz.flr.ab.avg, aes(x = Round, y = Abundance, fill = YSB, color = YSB)) +
  geom_line(aes(group = YSB), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Abundance - se, ymax = Abundance + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Sampling Round", y = "Edge Floral Abundance") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  pref.theme + theme(legend.position = c(0.15, 0.9))

# And for species density
ggplot(bz.flr.dn.avg, aes(x = Round, y = Species.Density, fill = YSB, color = YSB)) +
  geom_line(aes(group = YSB), size = 1, position = dodge) +
  geom_errorbar(aes(ymin = Species.Density - se, ymax = Species.Density + se),
                width = .2, size = 0.6, position = dodge) +
  geom_point(shape = 21, position = dodge) +
  labs(x = "Sampling Round", y = "Edge Floral Species Density") + 
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  pref.theme + theme(legend.position = c(0.15, 0.9))

##  ----------------------------------------------------------------------------------------------------------  ##
                      # Floral Data - FROM BFLY TRANSECTS ####
##  ----------------------------------------------------------------------------------------------------------  ##
# These data were collected within 1m of one side of the 100m butterfly transect.
  ## Butterfly data are located towards the center of each patch, so:
  ## Hereafter these floral resources may be refered to as "interior floral resources"

# Due to the use of mixed-effects models, will need to re-level "YSB" to perform pairwise comparisons

# Re-level the YSB factor
sort(unique(bf.flr$YSB))
bf.flr$YSB <- factor(bf.flr$YSB, levels = c(1, 0, 2))
sort(unique(bf.flr$YSB))

# Analyze to get 1 vs. 2 YSB comparison
bf.flr.ab.mem.base1 <- glmer(Abundance ~ YSB + (1|Site) + (1|Patch) + (1|Date),
                         data = bf.flr, family = poisson)

bf.flr.dn.mem.base1 <- glmer(Species.Density ~ YSB + (1|Site) + (1|Patch) + (1|Date),
                         data = bf.flr, family = poisson)

# Manually re-set leveling
bf.flr$YSB <- factor(bf.flr$YSB, levels = c(0, 1, 2))
sort(unique(bf.flr$YSB))

# Analyze
bf.flr.ab.mem.base0 <- glmer(Abundance ~ YSB + (1|Site) + (1|Patch) + (1|Date),
                         data = bf.flr, family = poisson)

bf.flr.dn.mem.base0 <- glmer(Species.Density ~ YSB + (1|Site) + (1|Patch) + (1|Date),
                         data = bf.flr, family = poisson)

# Assess significance
summary(bf.flr.ab.mem.base1)
summary(bf.flr.ab.mem.base0)
  ## YSB 0 = A | YSB 1 = B(.) | YSB 2 = A

summary(bf.flr.dn.mem.base1)
summary(bf.flr.dn.mem.base0)
  ## NS

# Visualize the differences in interior floral resources
ggplot(bf.flr, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Interior Floral Abundance") + 
  scale_fill_manual(values = colors) +
  pref.theme + theme(legend.position = "none")

ggplot(bf.flr, aes(x = YSB, y = Species.Density, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Interior Species Density") + 
  scale_fill_manual(values = colors) +
  pref.theme + theme(legend.position = "none")

# END ####

