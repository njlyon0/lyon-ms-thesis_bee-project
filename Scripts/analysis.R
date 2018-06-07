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
library(vegan); library(ggplot2)

# Get cleaned data
bz <- read.csv("./Data/actual_bz18.csv")

# Make years since burn a factor
bz$YSB <- as.factor(bz$YSB)
str(bz$YSB)

# Graphing shortcuts
colors <- c("0" = "#9970ab", "1" = "#762a83", "2" = "#40004b", # purples
            "Ref" = "#9970ab", "Con" = "#00441b",  "Spr" = "#1b7837", "SnS" = "#5aae61") # greens (Ref = 0 YSB)
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Patch-Burn Graze Question ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Prep the patch-burn graze dataframe (and remove columns that are irrelevant post-prep)
pbg <- subset(bz, bz$Adaptive.Mgmt == "PBG")[,-c(3, 5)]

# Get a dataframe for each round
pbg.r1 <- subset(pbg, pbg$Round == "R1")

##  ----------------------------------------------------------  ##
     # Round 1 PBG Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis





# Plotting
ggplot(pbg.r1, aes(x = YSB, y = Abundance, fill = YSB)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Years Since Burn", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")




##  ----------------------------------------------------------------------------------------------------------  ##
                              # Spray and Seed Question ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Prep the fescue project dataframe
sns <- subset(bz, bz$Herb.Trt != "x")[, -c(3:4)]

# Re-level the spray and seed treatments
unique(sns$Herb.Trt)
sns$Herb.Trt <- factor(as.character(sns$Herb.Trt), levels = c("Ref", "Con", "Spr", "SnS"))
unique(sns$Herb.Trt)

# Get a dataframe for each round
sns.r1 <- subset(sns, sns$Round == "R1")


##  ----------------------------------------------------------  ##
   # Round 1 Herbicide Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Analysis









# Plotting
ggplot(sns.r1, aes(x = Herb.Trt, y = Abundance, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Bee Abundance") + 
  scale_fill_manual(values = colors) +
  facet_grid(Height ~ .) +
  pref.theme + theme(legend.position = "none")



# END ####

