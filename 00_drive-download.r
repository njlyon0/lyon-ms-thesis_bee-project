##  ------------------------------------------------------------  ##
# Bee Project - Data Download
##  ------------------------------------------------------------  ##
# Purpose:
## Download raw data from Google Drive

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Make Folders ----
##  ------------------------------------------  ##

# List files in relevant Drive folder
(drive_raw <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1PBA3pYOvpK1CSWS0FUxzBVooU3i1ZeJS")) %>% 
    dplyr::filter(stringr::str_detect(string = name, pattern = "bee-project")))

# Download those files
purrr::walk2(.x = drive_raw$id, .y = drive_raw$name,
  .f = ~ googledrive::drive_download(file = .x, overwrite = TRUE,
    path = file.path("data", "raw", .y)))

# End ----