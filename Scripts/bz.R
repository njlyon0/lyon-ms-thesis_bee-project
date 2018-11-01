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
# Check out the variation in community composition among sites before assessing treatment effects

# Get sums of the data that have the following as rows:
  ## Patch:
bz.patch.v1 <- aggregate(Number ~ Site + Patch + YSB + Bee.Species,
                        FUN = sum, data = bz.v1)

  ## Bowl (all bees):
bz.bowl.v1 <- aggregate(Number ~ Site + Patch + YSB + Round + Height + Bee.Species, 
                        FUN = sum, data = bz.v1)

  ## Round (rare bees):
bz.rare.bowl.v1 <- aggregate(Number ~ Site + Patch + YSB + Bee.Species, 
                        FUN = sum, data = bz.rare)

# Spread each of these into wide format
bz.patch.v2 <- spread(key = Bee.Species, value = Number, fill = 0, data = bz.patch.v1)
bz.bowl.v2 <- spread(key = Bee.Species, value = Number, fill = 0, data = bz.bowl.v1)
bz.rare.bowl.v2 <- spread(key = Bee.Species, value = Number, fill = 0, data = bz.rare.bowl.v1)

# Get community matrices without identifier columns
bz.patch.rsp <- bz.patch.v2[,-c(1:3)]
bz.bowl.rsp <- bz.bowl.v2[,-c(1:5)]
bz.rare.bowl.rsp <- bz.rare.bowl.v2[,-c(1:5)]

# Get distance measures for each of these matrices
bz.patch.dst <- vegdist(bz.patch.rsp, method = "jaccard")
bz.bowl.dst <- vegdist(bz.bowl.rsp, method = "jaccard")
bz.rare.bowl.dst <- vegdist(bz.rare.bowl.rsp, method = "jaccard")

# Perform nonmetric multidimensional scaling ordination
bz.patch.mds <- metaMDS(bz.patch.dst, distance = "jaccard", engine = "monoMDS",
                        autotransform = F, expand = F, k = 2, try = 100)
bz.bowl.mds <- metaMDS(bz.bowl.dst, distance = "jaccard", engine = "monoMDS",
                       autotransform = F, expand = F, k = 2, try = 100)
bz.rare.bowl.mds <- metaMDS(bz.rare.bowl.dst, distance = "jaccard", engine = "monoMDS",
                            autotransform = F, expand = F, k = 2, try = 100)

# Check stress (should be progressively worse the less-defined the community is)
bz.patch.mds$stress; bz.bowl.mds$stress; bz.rare.bowl.mds$stress

# Get an NMS function for each of the two scales of data
nms.3.ord <- function(mod, groupcol, g1, g2, g3, lntp1 = 4, lntp2 = 2, lntp3 = 1, title = NA,
                      legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g3 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 3 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), main = title, type = 'none', xlab = "", ylab = "")
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#d73027" # red
  col2 <- "#fdae61" # orange
  col3 <- "#4575b4" # blue
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = c(21:23), bg = col1)
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = c(21:23), bg = col2)
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = c(21:23), bg = col3)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3)
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, 
              col = c(g1 = col1, g2 = col2, g3 = col3),
              display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         pch = 22, cex = 1.15, 
         pt.bg = c(col1, col2, col3))
  
}
nms.ptc.ord <- function(mod, groupcol, g1, g2, g3, g4, g5, g6, g7, g8, g9, 
                        lntp1 = 4, lntp2 = 2, lntp3 = 1, title = NA, legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g9 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 3 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), main = title, type = 'none', xlab = "", ylab = "")
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#d73027" # red
  col2 <- "#fdae61" # orange
  col3 <- "#4575b4" # blue
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = c(21), bg = col1)
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = c(22), bg = col1)
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = c(23), bg = col1)
  
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = c(21), bg = col2)
  points(mod$points[groupcol == g5, 1], mod$points[groupcol == g5, 2], pch = c(22), bg = col2)
  points(mod$points[groupcol == g6, 1], mod$points[groupcol == g6, 2], pch = c(23), bg = col2)
  
  points(mod$points[groupcol == g7, 1], mod$points[groupcol == g7, 2], pch = c(21), bg = col3)
  points(mod$points[groupcol == g8, 1], mod$points[groupcol == g8, 2], pch = c(22), bg = col3)
  points(mod$points[groupcol == g9, 1], mod$points[groupcol == g9, 2], pch = c(23), bg = col3)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- rep(c(lntp1, lntp2, lntp3), 3)
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, 
              col = c(g1 = col1, g2 = col1, g3 = col1,
                      g4 = col2, g5 = col2, g6 = col2,
                      g7 = col3, g8 = col3, g9 = col3),
              display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         pch = rep(c(21, 22, 23), 3), cex = 1.15, 
         pt.bg = c(rep(col1, 3), rep(col2, 3), rep(col3, 3)))
  
}

# Get legend shortcuts
patch.leg <- c("KLN", "PYN", "RIS")
bowl.leg <- as.vector(sort(unique(bz.bowl.v2$Patch)))

# Do the ordinations and save them out
jpeg(file = "./Graphs/bz_ords.jpg", width = 600, height = 400, quality = 100)

par(mfrow = c(1, 3), mar = c(1, 2, 2, 1))
nms.3.ord(mod = bz.patch.mds, groupcol = bz.patch.v2$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bee Sites", legcont = patch.leg)
nms.ptc.ord(mod = bz.bowl.mds, groupcol = bz.bowl.v2$Patch,
            g1 = "KLN-C", g2 = "KLN-E", g3 = "KLN-W",
            g4 = "PYN-N", g5 = "PYN-S", g6 = "PYN-W",
            g7 = "RIS-C", g8 = "RIS-N", g9 = "RIS-S",
            title = "Bee Transects", legcont = bowl.leg)
nms.3.ord(mod = bz.rare.bowl.mds, groupcol = bz.rare.bowl.v2$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bee Sites", legcont = patch.leg)
#nms.ptc.ord(mod = bz.rare.bowl.mds, groupcol = bz.rare.bowl.v2$Patch,
 #           g1 = "KLN-C", g2 = "KLN-E", g3 = "KLN-W",
  #          g4 = "PYN-N", g5 = "PYN-S", g6 = "PYN-W",
   #         g7 = "RIS-C", g8 = "RIS-N", g9 = "RIS-S",
    #        title = "Rare Bee Transects", legcont = bowl.leg)

dev.off(); par(mfrow = c(1, 1))

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

