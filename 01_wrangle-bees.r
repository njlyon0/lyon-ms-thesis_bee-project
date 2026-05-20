##  ------------------------------------------------------------  ##
# Bee Project - Wrangle
##  ------------------------------------------------------------  ##
# Purpose:
## Quality control/wrangle native bee community composition data

# Get required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source(file.path("-setup.r"))

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Wrangle 2017 Data ----
##  ------------------------------------------  ##

# Read in data
bz.17_v00 <- read.csv(file = file.path("data", "raw", "bee-project_raw-bees_2017.csv"))

# Check structure
dplyr::glimpse(bz.17_v00)

# Simplify/standardize column names, remove placeholder rows, and make qualitative variables quantitative
bz.17_v01 <- bz.17_v00 %>% 
  dplyr::filter(is.na(Capture.Year) != T) %>% 
  dplyr::mutate(height_cm = ifelse(test = (Height == "Low"),
                                   yes = 2.5, no = 100),
                .after = Height) %>% 
  dplyr::mutate(species = paste0(Genus, ".", Species),
                .after = Species) %>% 
  dplyr::mutate(bowls.recovered_percent = (Bowls.Recovered / 6) * 100,
                .after = Bowls.Recovered) %>% 
  dplyr::select(-Height, -Species, -Bowls.Recovered, -ID.Checked., -Pinned.) %>% 
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = SiteCode,
                specimen.id = Specimen.ID,
                family = Family,
                genus = Genus,
                sex = Sex,
                number = Number)

# Re-check structure
dplyr::glimpse(bz.17_v01)

# Summarize within bee species
bz.17_v02 <- bz.17_v01 %>% 
  dplyr::mutate(species = ifelse(test = genus == "Lasioglossum",
                                 yes = "Lasioglossum.sp", no = species)) %>% 
  dplyr::group_by(capture.year, capture.date, pasture, patch, height_cm,
                  bowls.recovered_percent, family, genus, species) %>% 
  dplyr::summarize(bee.total = sum(number, na.rm = T),
                   .groups = "drop")

# Re-check structure
dplyr::glimpse(bz.17_v02)

# Remove instances where bees were not found or otherwise not recorded
bz.17_v03 <- bz.17_v02 %>% 
  dplyr::filter(!species %in% c("X.X", "."))

# Check what was lost
supportR::diff_check(old = unique(bz.17_v02$species), new = unique(bz.17_v03$species))

# Final structure check
dplyr::glimpse(bz.17_v03)

##  ------------------------------------------  ##      
            # 2018 Standardization ----
##  ------------------------------------------  ##      

# Read in data
bz.18_v00 <- read.csv(file = file.path("data", "raw", "bee-project_raw-bees_2018.csv"))

# Check structure
dplyr::glimpse(bz.18_v00)

# Simplify/standardize column names, remove placeholder rows, and make qualitative variables quantitative
bz.18_v01 <- bz.18_v00 %>% 
  dplyr::filter(is.na(Capture.Year) != T) %>% 
  dplyr::mutate(height_cm = ifelse(test = (Height == "Low"),
                                   yes = 2.5, no = 100),
                .after = Height) %>% 
  dplyr::mutate(bowl.size_oz = as.numeric(gsub(pattern = "oz", replacement = "",
                                               x = Bowl.Size)),
                .after = Bowl.Size) %>% 
  dplyr::mutate(species = paste0(Genus, ".", Species),
                .after = Species) %>% 
  dplyr::select(-Height, -Species, -Bowl.Size, -ID.Check, -Pinned) %>% 
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = Patch,
                bowl.position = Bowl.Position,
                bowl.color = Bowl.Color,
                bowl.status = Bowl.Status,
                specimen.id = Specimen.ID,
                family = Family,
                genus = Genus,
                sex = Sex,
                number = Number)

# Check structure
dplyr::glimpse(bz.18_v01)

# QC column contents and remove bad rows
bz.18_v02 <- bz.18_v01 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::all_of(c("bowl.color", "bowl.status")),
                              .fns = tolower)) %>% 
  dplyr::mutate(bowl.color = dplyr::case_when(
    bowl.color %in% c("bue") ~ "blue",
    bowl.color %in% c(" yellow", "yellow ", "yelow") ~ "yellow",
    bowl.color %in% c("whie") ~ "white",
    TRUE ~ bowl.color)) %>% 
  dplyr::mutate(species = dplyr::case_when(
    species %in% c("Andrena.cressonii cressonii") ~ "Andrena.cressonii",
    genus == "Lasioglossum" ~ "Lasioglossum.sp",
    TRUE ~ species)) %>% 
  dplyr::filter(!bowl.status %in% c("off clip", "destroyed")) %>% 
  dplyr::select(-bowl.status) %>% 
  dplyr::group_by(sampling.event.id, height_cm) %>% 
  dplyr::mutate(bowls.recovered_ct = length(unique(bowl.position)),
                bowls.recovered_percent = (bowls.recovered_ct / 6) * 100) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-bowls.recovered_ct)

# Check structure
dplyr::glimpse(bz.18_v02)

# Remove 'bad' species and summarize within critical columns
bz.18_v03 <- bz.18_v02 %>%
  dplyr::filter(!species %in% c("X.x", ".", "Andrena.sp", "Hylaeus.sp")) %>% 
  dplyr::group_by(capture.year, capture.date, pasture, patch, height_cm,
                  bowls.recovered_percent, bowl.color, bowl.size_oz, 
                  family, genus, species) %>% 
  dplyr::summarize(bee.total = sum(number, na.rm = T),
                   .groups = "drop")

# Final structure check
dplyr::glimpse(bz.18_v03)

##  ------------------------------------------  ##      
# Combine Years ----
##  ------------------------------------------  ##      

# Combine the two data files
bz_v01 <- dplyr::bind_rows(bz.18_v03, bz.17_v03)

# Check structure
dplyr::glimpse(bz_v01)

# There should be 6 bowls / transect / "height"; check distribution
hist(x = bz_v01$bowls.recovered_percent)

# Drop instances where insufficiently many bowls were recovered
bz_v02 <- bz_v01 %>% 
  dplyr::filter(bowls.recovered_percent >= 80) %>%
  dplyr::select(-bowls.recovered_percent)

# How many rows were lost?
nrow(bz_v01) - nrow(bz_v02)

# Check structure
dplyr::glimpse(bz_v02)

# Fill in some last critical information
bz_v03 <- bz_v02 %>% 
  dplyr::mutate(
    bowl.color = dplyr::case_when(
      capture.year == 2017 & is.na(bowl.color) == TRUE ~ "unrecorded",
      TRUE ~ bowl.color),
    bowl.size_oz = dplyr::case_when(
      capture.year == 2017 & is.na(bowl.size_oz) == TRUE ~ 3.25,
      TRUE ~ bowl.size_oz)) %>% 
  tidyr::separate_wider_delim(cols = capture.date, delim = ".",
                              names = c("capture.month", "capture.day")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("capture."),
                              .fns = as.numeric))

# Check structure
dplyr::glimpse(bz_v03)

##  ------------------------------------------  ##
# Integrate Treatment Info ----
##  ------------------------------------------  ##

# Add management info explicitly
bz_v04 <- bz_v03

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
bz_v99 <- bz_v04

# Check structure
dplyr::glimpse(bz_v99)

# Export this tidied data!
write.csv(x = bz_v99, row.names = FALSE, na = '',
          file = file.path("data", "01_bee-community-tidy.csv"))

# End ----
