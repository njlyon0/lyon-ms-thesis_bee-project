##  --------------------------------------------------------------------------------------------------------------------  ##
                            # Lyon PBG Manuscript -- Transect Ordinations
##  --------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Check out transect communities to see if they are driving site differences

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(tidyr)

##  ----------------------------------------------------------------------------------------------------------  ##
                            # Data Retrieval & Prep ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Read in long-form data
bz.lng <- read.csv("./Data/bz-long.csv")
  ## Bees

# For each dataset, sum across all samples within the same patch
bz.v2 <- aggregate(Number ~ Site + Patch + YSB + Round + Height + Bee.Species,
                   FUN = sum, data = bz.lng)

# Set the names of the columns of each of these to be the same (to make copy/pasting easier)
colnames(bz.v2)
pref.col.names <- c("Site", "Patch", "YSB", "Round", "Height", "Species", "Number")
colnames(bz.v2) <- pref.col.names
colnames(bz.v2)

# Get each of those simplified dataframes into wide format
bz.v3 <- spread(key = Species, value = Number, fill = 0, data = bz.v2)[,-37]

# Get an abundance column (you'll see)
bz.v3$Abundance <- rowSums(bz.v3[,-c(1:5)])

# Remove all transect/height combos where 0 bees were recovered
sort(unique(bz.v3$Abundance))
bz.v4 <- subset(bz.v3, bz.v3$Abundance != 0)
sort(unique(bz.v4$Abundance))

# Get community composition matrixes
bz.rsp <- as.matrix(bz.v4[,-c(1:5, ncol(bz.v4))])

# Get distances from these matrixes
bz.dst <- vegdist(bz.rsp, method = "jaccard")

# Do the multidimensional scaling for each of these dataframes
bz.mds <- metaMDS(bz.dst, distance = "jaccard", engine = "monoMDS",
                  autotransform = F, expand = F, k = 2, try = 100)

# Check stress (ideally is < 0.15)
bz.mds$stress

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # NMS Ordinations ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Classic NMS function
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

# Heavily modified NMS function (for assessing among patch variation)
nms.ptc.ord <- function(mod, groupcol, g1, g2, g3, g4, g5, g6, g7, g8, g9, 
                      lntp1 = 4, lntp2 = 2, lntp3 = 1, title = NA,
                      legcont, legpos = "topright") {
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


# Plotting shortcut(s)
site.leg <- as.vector(sort(unique(bz.v4$Patch)))

# COLOR LEGEND:
  ## Red = KLN
  ## Orange = PYN
  ## Blue = RIS

# Use it!
nms.ptc.ord(mod = bz.mds, groupcol = bz.v4$Patch,
            g1 = "KLN-C", g2 = "KLN-E", g3 = "KLN-W",
            g4 = "PYN-N", g5 = "PYN-S", g6 = "PYN-W",
            g7 = "RIS-C", g8 = "RIS-N", g9 = "RIS-S",
          title = "Bees", legcont = site.leg)

# Save out a graph to look at all together
jpeg(file = "./Graphs/bz_ptc_nms_ords.jpg", width = 600, height = 400, quality = 100)

nms.ptc.ord(mod = bz.mds, groupcol = bz.v4$Patch,
            g1 = "KLN-C", g2 = "KLN-E", g3 = "KLN-W",
            g4 = "PYN-N", g5 = "PYN-S", g6 = "PYN-W",
            g7 = "RIS-C", g8 = "RIS-N", g9 = "RIS-S",
            title = "Bee Transects", legcont = site.leg)

dev.off()

