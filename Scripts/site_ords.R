##  --------------------------------------------------------------------------------------------------------------------  ##
                          # Lyon PBG Manuscript -- Site Ordinations
##  --------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Check to see if the sites have different communities (outside of PBG treatment)
  ## Could force my hand in terms of taking landscape context into account

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(tidyr)

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Data Retrieval & Prep ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Read in long-form data for each of the four groups
bf.lng <- read.csv("./Data/bf-long.csv")
  ## Butterflies
bz.lng <- read.csv("./Data/bz-long.csv")
  ## Bees
bf.flr.lng <- read.csv("./Data/bf-flr-long.csv")
  ## Flowers from butterfly transects (bf.flr)
bz.flr.lng <- read.csv("./Data/bz-flr-long.csv")
  ## Flowers from bee transects (bz.flr)

# For each dataset, sum across all samples within the same patch
bf.v2 <- aggregate(Number ~ Site + Patch + YSB + BFLY.Common.Name,
                   FUN = sum, data = bf.lng)
bz.v2 <- aggregate(Number ~ Site + Patch + YSB + Bee.Species,
                   FUN = sum, data = bz.lng)
bf.flr.v2 <- aggregate(TransectTotals ~ Site + Patch + YSB + Nectar.Plant.Name,
                       FUN = sum, data = bf.flr.lng)
bz.flr.v2 <- aggregate(Flower.Number ~ Site + Patch + YSB + Flower.Common.Name,
                       FUN = sum, data = bz.flr.lng)

# Set the names of the columns of each of these to be the same (to make copy/pasting easier)
colnames(bf.v2); colnames(bz.v2); colnames(bf.flr.v2); colnames(bz.flr.v2)
pref.col.names <- c("Site", "Patch", "YSB", "Species", "Number")
colnames(bf.v2) <- pref.col.names
colnames(bz.v2) <- pref.col.names
colnames(bf.flr.v2) <- pref.col.names
colnames(bz.flr.v2) <- pref.col.names
colnames(bf.v2); colnames(bz.v2); colnames(bf.flr.v2); colnames(bz.flr.v2)

# Get each of those simplified dataframes into wide format
bf.v3 <- spread(key = Species, value = Number, fill = 0, data = bf.v2)
bz.v3 <- spread(key = Species, value = Number, fill = 0, data = bz.v2)[,-35]
bf.flr.v3 <- spread(key = Species, value = Number, fill = 0, data = bf.flr.v2)
bz.flr.v3 <- spread(key = Species, value = Number, fill = 0, data = bz.flr.v2)

# Get community composition matrixes
bf.rsp <- as.matrix(bf.v3[,-c(1:3)])
bz.rsp <- as.matrix(bz.v3[,-c(1:3)])
bf.flr.rsp <- as.matrix(bf.flr.v3[,-c(1:3)])
bz.flr.rsp <- as.matrix(bz.flr.v3[,-c(1:3)])

# Get distances from these matrixes
bf.dst <- vegdist(bf.rsp, method = "jaccard")
bz.dst <- vegdist(bz.rsp, method = "jaccard")
bf.flr.dst <- vegdist(bf.flr.rsp, method = "jaccard")
bz.flr.dst <- vegdist(bz.flr.rsp, method = "jaccard")

# Do the multidimensional scaling for each of these dataframes
bf.mds <- metaMDS(bf.dst, distance = "jaccard", engine = "monoMDS",
                     autotransform = F, expand = F, k = 2, try = 100)
bz.mds <- metaMDS(bz.dst, distance = "jaccard", engine = "monoMDS",
                  autotransform = F, expand = F, k = 2, try = 100)
bf.flr.mds <- metaMDS(bf.flr.dst, distance = "jaccard", engine = "monoMDS",
                  autotransform = F, expand = F, k = 2, try = 100)
bz.flr.mds <- metaMDS(bz.flr.dst, distance = "jaccard", engine = "monoMDS",
                  autotransform = F, expand = F, k = 2, try = 100)

# Check stress (ideally is < 0.15)
bf.mds$stress
bz.mds$stress
bf.flr.mds$stress
bz.flr.mds$stress

##  ----------------------------------------------------------------------------------------------------------  ##
                                    # NMS Ordinations ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the NMS function
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

# Plotting shortcut(s)
site.leg <- c("KLN", "PYN", "RIS")

# COLOR LEGEND:
  ## Red = KLN
  ## Orange = PYN
  ## Blue = RIS

# Use it!
nms.3.ord(mod = bf.mds, groupcol = bf.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Butterfly", legcont = site.leg)
nms.3.ord(mod = bz.mds, groupcol = bz.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bees", legcont = site.leg)
nms.3.ord(mod = bf.flr.mds, groupcol = bf.flr.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bfly Flowers", legcont = site.leg)
nms.3.ord(mod = bz.flr.mds, groupcol = bz.flr.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bee Flowers", legcont = site.leg)

# Save out a graph to look at all together
jpeg(file = "./Graphs/site_nms_ords.jpg", width = 600, height = 400, quality = 100)

par(mfrow = c(2, 2), mar = c(1, 2, 2, 1))
nms.3.ord(mod = bf.mds, groupcol = bf.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Butterfly", legcont = site.leg)
nms.3.ord(mod = bz.mds, groupcol = bz.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bees", legcont = site.leg)
nms.3.ord(mod = bf.flr.mds, groupcol = bf.flr.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bfly Flowers", legcont = site.leg)
nms.3.ord(mod = bz.flr.mds, groupcol = bz.flr.v3$Site, g1 = "KLN", g2 = "PYN", g3 = "RIS",
          title = "Bee Flowers", legcont = site.leg)

dev.off(); par(mfrow = c(1, 1))

