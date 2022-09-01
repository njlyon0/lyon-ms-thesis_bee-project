## --------------------------------------------------- ##
    # Lyon MS Thesis - Native Bee Project - Bee Data
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
bees_v0 <- read.csv(file.path("data", "bee-proj_2018_bees-long.csv"))

# Take a look
dplyr::glimpse(bees_v0)

# Make a version where we reclaim genus ID
bees_v1 <- bees_v0 %>%
  tidyr::separate(col = Bee_Species, sep = "\\.", remove = F,
                  into = c("Bee_Genus", "Bee_Epithet")) %>%
  # Drop epithet
  dplyr::select(-Bee_Epithet) %>%
  # Also tidy some other columns
  dplyr::rename(Bowl_Height = Height,
                Bee_Family = Family)

# Check it out
dplyr::glimpse(bees_v1)

# Create a plotting shortcut
bee_theme <- theme_classic() + 
  theme(legend.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

## --------------------------------------------------- ##
                  # Data Exploration ----
## --------------------------------------------------- ##
# Note: There are many random effects, bee counts / bowl are quite low, and over dispersion is a likely threat to model convergence. Let's do some exploratory plotting to help tease these apart.

# Drop bee identity and sum within other variables
per_bowl <- bees_v1 %>%
  dplyr::group_by(across(Sampling_Event_ID:Bowl_Status)) %>%
  dplyr::summarize(Number = sum(Number, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Also split post position off
  tidyr::separate(col = Post_ID, remove = F, sep = "-",
                  into = c("Site_delete", "Patch_delete",
                           "Bowl_Position")) %>%
  dplyr::select(-dplyr::ends_with("_delete"))

# This creates a total bees per bowl count
glimpse(per_bowl)

# Create exploratory plots folder
dir.create("exploratory_plots", showWarnings = F)

# Plot bowl size to see if it (visually) differs
ggplot(per_bowl, aes(fill = Bowl_Size_oz, x = Round, y = Number)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) + 
  facet_wrap(. ~ Site) +
  bee_theme

# Export
ggsave(file.path("exploratory_plots", "bees_bowl-size.png"))

# Now bowl color
ggplot(per_bowl, aes(fill = Bowl_Color, x = Round, y = Number)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) + 
  facet_wrap(. ~ Site) +
  scale_fill_manual(values = c("blue", "white", "orange")) + 
  bee_theme

# Export
ggsave(file.path("exploratory_plots", "bees_bowl-color.png"))

# Now bowl position
ggplot(per_bowl, aes(fill = Bowl_Position, x = Round, y = Number)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) + 
  facet_wrap(. ~ Site) +
  bee_theme

# Export
ggsave(file.path("exploratory_plots", "bees_bowl-position.png"))

# Finally, bowl height
ggplot(per_bowl, aes(fill = Bowl_Height, x = Round, y = Number)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) + 
  facet_wrap(. ~ Site) +
  bee_theme

# Export
ggsave(file.path("exploratory_plots", "bees_bowl-height.png"))

# Take aways:
## 1) bowl size exactly correlates to sampling round so any model including round basically includes size
## 2) bowl color seems to matter because blue bowls got more bees
## 3) bowl position is a dog's breakfast (which is fine because I had no a priori reason to think it would matter)
## 4) does look like high bowls get more bees

## --------------------------------------------------- ##
            # Data Wrangling - Specific ----
## --------------------------------------------------- ##
# Time to implement some final wrangling with inference from our exploration

bees_actual <- bees_v1 %>%
  # Can drop bowl position and bowl size
  dplyr::select(Capture_Date, Round, Site, Patch,
                Adaptive_Mgmt:Herbicide_Trt,
                Bowl_Height, Bowl_Color, dplyr::contains("Bee_"),
                Number) %>%
  # That done, summarize within everything else
  dplyr::group_by(across(Capture_Date:Bee_Genus)) %>%
  dplyr::summarize(Bee_Count = sum(Number, na.rm = T)) %>%
  dplyr::ungroup()
  
# Check it out
glimpse(bees_actual)

## --------------------------------------------------- ##
                  # Model Selection ----
## --------------------------------------------------- ##
# We still have a lot of variables so let's do model selection

# Fit a model with the kitchen sink in it (i.e., everything)
lm_sink <- lm(Bee_Count ~ Capture_Date + Round + Site + Adaptive_Mgmt + Years_Since_Burn + Herbicide_Trt + Bowl_Height + Bowl_Color + Bee_Family + Bee_Species, data = bees_actual)

# Fit one for my actual hypothesis
lm_hope <- lm(Bee_Count ~ Years_Since_Burn + Herbicide_Trt + Bowl_Height + Bee_Species + Bowl_Color, data = bees_actual)

# Fit one where we don't think species or YSB matters
lm_fam <- lm(Bee_Count ~ Adaptive_Mgmt + Herbicide_Trt + Bowl_Height + Bowl_Color + Bee_Family, data = bees_actual)

# One where only site-level characters matter
lm_site <- lm(Bee_Count ~ Site + Adaptive_Mgmt + Bee_Species, data = bees_actual)

# One where only patch-level characters matter
lm_patch <- lm(Bee_Count ~ Round + Years_Since_Burn + Herbicide_Trt + Bowl_Height + Bowl_Color + Bee_Species, data = bees_actual)

# One where its pure seasonality and site
lm_time <- lm(Bee_Count ~ Capture_Date + Round + Site + Bee_Species, data = bees_actual)

# Last one, where it's just bee taxa
lm_spp <- lm(Bee_Count ~ Bowl_Color + Bee_Family + Bee_Species, data = bees_actual)

# List the models
model_list <- list(lm_sink, lm_hope, lm_fam, lm_site, lm_patch, lm_spp, lm_time)

# Compare model explanatory power using Akaike (AIC!)
AICcmodavg::aictab(cand.set = model_list, sort = T,
                   modnames = c("Sink", "Hypothesis", "Family", "Site",
                                "Patch", "Taxa", "Seasonality"))

# More content to come...

## --------------------------------------------------- ##
              # Treatment Clarification ----
## --------------------------------------------------- ##

# Let's create a simple treatment table per patch/site for reference
treatments <- bees_actual %>%
  # Pare down to needed columns
  dplyr::select(Site, Patch, Adaptive_Mgmt:Herbicide_Trt) %>%
  # Get only unique combinations
  unique()

# Check it
treatments

# Save it
dir.create("reference", showWarnings = F)
write.csv(treatments, row.names = F,
          file.path("reference", "treatment_table.csv"))

# End ----
