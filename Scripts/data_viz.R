##  --------------------------------------------------------------------------------------------------------------------  ##
                          # Lyon PBG Manuscript -- Data Visualization
##  --------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Examine the data for patterns that may obscure/inflate the patterns I'm looking for
  ## In plain English: to visualize the data

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(vegan); library(tidyr); library(Rmisc); library(ggplot2); library(cowplot)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line("black"),
                    legend.title = element_blank())
bowl.cols = c("Blue" = "blue", "White" = "gray", "Yellow" = "yellow")
site.cols = c("KLN" = "#d73027", "PYN" = "#ffffbf", "RIS" = "#4575b4")

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Bee Data Visualization ####
##  ----------------------------------------------------------------------------------------------------------  ##
bz <- read.csv("./Data/bz-wide.csv")

# Visualize in number form
plyr::count(df = bz, vars = c("Abundance", "Site"))

# Visualize number of bees collected by bowl color, site, and height
bz.abun.reps <- ggplot(data = bz, aes(x = as.factor(YSB), y = Abundance, color = Bowl.Color, shape = Bowl.Color)) +
  geom_point(size = 2, stat = 'identity', position = dodge) +
  labs(x = "Years Since Burn", y = "Abundance")+
  scale_color_manual(values = bowl.cols) +
  facet_grid(Height ~ Site) +
  pref.theme + theme(legend.position = 'none'); bz.abun.reps

# What about just site differences?
bz.abun.sites <- ggplot(data = bz, aes(x = Site, y = Abundance, fill = Site)) +
  geom_boxplot() +
  labs(x = "Years Since Burn", y = "Abundance")+
  scale_fill_manual(values = site.cols) +
  pref.theme + theme(legend.position = 'none'); bz.abun.sites

# Make a composite plot
plot_grid(bz.abun.reps, bz.abun.sites, labels = c("A", "B"), ncol = 2, nrow = 1)
ggsave("./Graphs/bz_abun_viz.pdf", plot = last_plot(), width = 6, height = 4)

# Do the same for species density
  ## Replicate level visualization
bz.dens.reps <- ggplot(data = bz, aes(x = as.factor(YSB), y = Species.Density, color = Bowl.Color, shape = Bowl.Color)) +
  geom_point(size = 2, stat = 'identity', position = dodge) +
  labs(x = "Years Since Burn", y = "Species Density")+
  scale_color_manual(values = bowl.cols) +
  facet_grid(Height ~ Site) +
  pref.theme + theme(legend.position = 'none'); bz.dens.reps

  ## Site-level
bz.dens.sites <- ggplot(data = bz, aes(x = Site, y = Species.Density, fill = Site)) +
  geom_boxplot() +
  labs(x = "Years Since Burn", y = "Species Density")+
  scale_fill_manual(values = site.cols) +
  pref.theme + theme(legend.position = 'none'); bz.dens.sites

  ## Composite plot
plot_grid(bz.dens.reps, bz.dens.sites, labels = c("A", "B"), ncol = 2, nrow = 1)
ggsave("./Graphs/bz_dens_viz.pdf", plot = last_plot(), width = 6, height = 4)



