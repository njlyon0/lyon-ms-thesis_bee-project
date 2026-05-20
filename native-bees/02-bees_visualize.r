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

# Read data
bz_v01 <- read.csv(file = file.path("data", "01_bee-community-tidy.csv"))

# Check structure
dplyr::glimpse(bz_v01)

##  ------------------------------------------  ##
# Prepare Data ----
##  ------------------------------------------  ##

# Pare down data and summarize within relevant categories
bz_v02 <- bz_v01 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(bz_v01), 
    y = c("capture.day", "genus", "species", "bee.total"))))) %>% 
  dplyr::summarize(abundance = sum(bee.total, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(bz_v02)

##  ------------------------------------------  ##
# Visualize Data
##  ------------------------------------------  ##

# Kitchen sink plot for sampling methodology
ggplot(bz_v02, aes(x = family, y = abundance, fill = bowl.color)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ capture.year) +
  labs(x = "Family", y = "Abundance") +
  scale_fill_manual(values = c("unrecorded" = "pink", "blue" = "blue", "white" = "gray33", "yellow" = "gold")) +
  supportR::theme_lyon() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Export this
ggsave(filename = file.path("graphs", "bee-method-effects.png"),
  height = 5, width = 7, units = "in")

# Kitchen sink graph for management information
bz_v02 %>% 
  dplyr::filter(family %in% c("Apidae", "Halictidae")) %>% 
  ggplot(data = ., aes(x = treatment_herbicide, y = abundance, fill = family)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ capture.year) +
  labs(x = "Management", y = "Abundance") +
  supportR::theme_lyon() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Export this
ggsave(filename = file.path("graphs", "bee-management-effects.png"),
  height = 5, width = 7, units = "in")

# Kitchen sink graph for time since fire
bz_v02 %>% 
  dplyr::filter(family %in% c("Apidae", "Halictidae")) %>% 
  ggplot(data = ., aes(x = as.factor(time.since.fire_years), y = abundance, 
    fill = family)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ capture.year) +
  labs(x = "Time Since Fire (Years)", y = "Abundance") +
  supportR::theme_lyon()

# Export this
ggsave(filename = file.path("graphs", "bee-fire-effects.png"),
  height = 5, width = 7, units = "in")

# End ----
