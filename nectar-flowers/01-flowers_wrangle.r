##  ------------------------------------------------------------  ##
# Bee Project - Wrangle Flowers
##  ------------------------------------------------------------  ##
# Purpose:
## Quality control/wrangle nectar-producing flower community composition data

# Get required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source(file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# 2017 Standardization ----
##  ------------------------------------------  ##

# Read in data
flr.17_v00 <- read.csv(file = file.path("data", "raw", "bee-project_raw-flowers_2017.csv"))

# Check structure
dplyr::glimpse(flr.17_v00)

# Do big-picture standardizing
flr.17_v01 <- flr.17_v00 %>% 
  # Rename columns
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = SiteCode,
                nectar.common = Nectar.Common.Name) %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with("sampling."), -Collector, 
                -Enterer, -Checker) %>% 
  # Make floral common names lowercase
  dplyr::mutate(nectar.common = tolower(nectar.common)) %>% 
  # Reshape to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("Section."),
                      names_to = "section.id", values_to = "flower_ct")

# Re-check structure
dplyr::glimpse(flr.17_v01)

##  ------------------------------------------  ##
# 2018 Standardization ----
##  ------------------------------------------  ##

# Read in data
flr.18_v00 <- read.csv(file = file.path("data", "raw", "bee-project_raw-flowers_2018.csv"))

# Check structure
dplyr::glimpse(flr.18_v00)

# Do big-picture standardizing
flr.18_v01 <- flr.18_v00 %>% 
  # Rename columns
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = Patch,
                nectar.common = Flower.Common.Name) %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with("sampling."), -Collector, -Enterer, 
                -Entry.Date, -Checker, -Check.Date) %>% 
  # Make floral common names lowercase
  dplyr::mutate(nectar.common = tolower(nectar.common)) %>% 
  # Reshape to long format
  tidyr::pivot_longer(cols = P1:P6, names_to = "point.id", values_to = "flower_ct")

# Re-check structure
dplyr::glimpse(flr.18_v01)

##  ------------------------------------------  ##
# Combine Years ----
##  ------------------------------------------  ##

# Combine into a single data object
flr_v01 <- dplyr::bind_rows(flr.17_v01, flr.18_v01) %>% 
  # Put capture date in a less ambiguous format
  tidyr::separate_wider_delim(cols = capture.date, delim = ".",
                              names = c("capture.month", "capture.day")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("capture."),
                              .fns = as.numeric))

# Check structure
dplyr::glimpse(flr_v01)

# Standardize floral names where needed
flr_v02 <- flr_v01 %>% 
  dplyr::mutate(nectar.common = dplyr::case_when(
    nectar.common %in% c("bee balm") ~ "bergamot",
    nectar.common %in% c("plantago lanceolata") ~ "ribwort plantain",
    # nectar.common %in% c() ~ "",
    T ~ nectar.common))

# Check remaining names
sort(unique(flr_v02$nectar.common))

# Remove section/point identification and summarize
flr_v03 <- flr_v02 %>% 
  dplyr::group_by(capture.year, capture.month, capture.day, 
                  pasture, patch, nectar.common) %>% 
  dplyr::summarize(flower.total = sum(flower_ct, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Remove any rows with 0 flowers
  dplyr::filter(flower.total > 0)

# Re-check structure
dplyr::glimpse(flr_v03)

# Export this tidied data!
write.csv(x = flr_v03, row.names = FALSE, na = '',
          file = file.path("data", "01_nectar-community-tidy.csv"))

# End ----
