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

# Do big-picture standardizing & streamlining
flr.17_v01 <- flr.17_v00 %>% 
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
    sampling.round = Round,
    capture.year = Capture.Year,
    capture.date = Capture.Date,
    pasture = Site,
    patch = SiteCode,
    nectar.common = Nectar.Common.Name) %>% 
  dplyr::select(-dplyr::starts_with("sampling."), -Collector, -Enterer, -Checker) %>% 
  dplyr::mutate(nectar.common = tolower(nectar.common)) %>% 
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

# Do big-picture standardizing & streamlining
flr.18_v01 <- flr.18_v00 %>% 
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
    sampling.round = Round,
    capture.year = Capture.Year,
    capture.date = Capture.Date,
    pasture = Site,
    patch = Patch,
    nectar.common = Flower.Common.Name) %>% 
  dplyr::select(-dplyr::starts_with("sampling."), -Collector, -Enterer, 
    -Entry.Date, -Checker, -Check.Date) %>% 
  dplyr::mutate(nectar.common = tolower(nectar.common)) %>% 
  tidyr::pivot_longer(cols = P1:P6, names_to = "point.id", values_to = "flower_ct")

# Re-check structure
dplyr::glimpse(flr.18_v01)

##  ------------------------------------------  ##
# Combine Years ----
##  ------------------------------------------  ##

# Combine into a single data object
flr_v01 <- dplyr::bind_rows(flr.17_v01, flr.18_v01) %>% 
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
    TRUE ~ nectar.common))

# Check remaining names
sort(unique(flr_v02$nectar.common))

# Remove section/point identification and summarize
flr_v03 <- flr_v02 %>% 
  dplyr::group_by(capture.year, capture.month, capture.day, pasture, patch, nectar.common) %>% 
  dplyr::summarize(flower.total = sum(flower_ct, na.rm = TRUE),
    .groups = "drop") %>%
  dplyr::filter(flower.total > 0)

# Re-check structure
dplyr::glimpse(flr_v03)

##  ------------------------------------------  ##
# Integrate Treatment Info ----
##  ------------------------------------------  ##

# Add management info explicitly
flr_v04 <- flr_v03 %>% 
  dplyr::mutate(
    treatment_mgmt = dplyr::case_when(
      pasture %in% toupper(c("kln", "pyn", "ris")) ~ "patch-burn-graze",
      pasture %in% toupper(c("gil", "ltr", "ste", "pyw", "pys")) ~ "graze-and-burn",
      TRUE ~ NA),
    treatment_herbicide = dplyr::case_when(
      pasture %in% toupper(c("kln", "pyn", "ris")) ~ "none",
      patch == toupper("pys-w") ~ "none",
      patch %in% toupper(c("gil-s", "ltr-w", "ste-w", "pyw-s")) ~ "control",
      patch %in% toupper(c("gil-n", "ltr-c", "ste-n", "pyw-c")) ~ "spray-only",
      patch %in% toupper(c("gil-c", "ltr-e", "ste-s", "pyw-n")) ~ "spray-and-seed",
      TRUE ~ NA),
    time.since.fire_years = dplyr::case_when(
      capture.year == 2017 & pasture %in% toupper(c("gil", "ltr", "pyw")) ~ 2,
      capture.year == 2017 & pasture == toupper("ste") ~ 1,
      capture.year == 2017 & patch %in% toupper(c("kln-c", "pyn-w", "ris-n")) ~ 0,
      capture.year == 2017 & patch %in% toupper(c("kln-e", "pyn-n", "ris-s", "pys-w")) ~ 1,
      capture.year == 2017 & patch %in% toupper(c("kln-w", "pyn-s", "ris-c")) ~ 2,
      capture.year == 2018 & pasture %in% toupper(c("gil", "ltr", "ste", "pyw")) ~ 0,
      capture.year == 2018 & pasture == toupper("ste") ~ 2,
      capture.year == 2018 & patch %in% toupper(c("kln-w", "pyn-s", "ris-c")) ~ 0,
      capture.year == 2018 & patch %in% toupper(c("kln-c", "pyn-w", "ris-n")) ~ 1,
      capture.year == 2018 & patch %in% toupper(c("kln-e", "pyn-n", "ris-s")) ~ 2,
      TRUE ~ NA),
    .after = patch)

# Check management categories
flr_v04 %>% 
  dplyr::select(pasture, treatment_mgmt) %>% 
  dplyr::distinct()

# Check herbicide categories
flr_v04 %>% 
  dplyr::group_by(treatment_herbicide) %>% 
  dplyr::summarize(ct = dplyr::n(),
    patches = paste(unique(patch), collapse = "; "),
    .groups = "drop")

# Check TSF values
flr_v04 %>% 
  dplyr::group_by(time.since.fire_years) %>% 
  dplyr::summarize(ct = dplyr::n(),
    patches = paste(unique(paste0(patch, "-", capture.year)), collapse = "; "),
    .groups = "drop")

# Check structure
dplyr::glimpse(flr_v04)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
flr_v99 <- flr_v04

# Check structure
dplyr::glimpse(flr_v99)

# Export this tidied data!
write.csv(x = flr_v99, row.names = FALSE, na = '',
          file = file.path("data", "01_nectar-community-tidy.csv"))

# End ----
