## --------------------------------------------------- ##
   # Lyon MS Thesis - Native Bee Project - Floral Data
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
