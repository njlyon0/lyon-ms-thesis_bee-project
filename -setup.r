##  ------------------------------------------------------------  ##
# Set Up Steps
##  ------------------------------------------------------------  ##
# Purpose:
## Do any generally-needed tasks used by other scripts in this repository

# Get required libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

##  ------------------------------------------  ##      
# Make Folders ----
##  ------------------------------------------  ##      

# Create needed folder(s)
dir.create(path = file.path("data", "raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(path = file.path("graphs"), showWarnings = FALSE)

# End ----
