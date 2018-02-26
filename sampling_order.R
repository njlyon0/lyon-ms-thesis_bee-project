##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                      # Determining Site Sampling Order
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# This is only a useful code twice in the next four years, so I'm doing it as a seperate file to simplify my other codes
  ## Want to randomly sample one site of each treatment during each sampling effort

# Required libraries & WD
library(geomorph); library(vegan); library(tidyr); library(ggplot2)
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Chaps1&2-GRG")

# Read in special treatment index (with only bee site info)
beetreat <- read.csv("./Indeces/beeonlytrmntindex.csv")

# This is a dumb way but I need this code so infrequently (2x in 2 years) that I'm okay with it being bad (sorry future me)
ref <- subset(beetreat, beetreat$Fescue.Treatment == "Ref")
con <- subset(beetreat, beetreat$Fescue.Treatment == "Con")
spr <- subset(beetreat, beetreat$Fescue.Treatment == "Spr")
sns <- subset(beetreat, beetreat$Fescue.Treatment == "SnS")

sample(ref$DataCode, replace = F)
sample(con$DataCode, replace = F)
sample(spr$DataCode, replace = F)
sample(sns$DataCode, replace = F)

##  ---------------------------------------------------  ##
      # 2017 order (Ref, Con, Spr, SnS):
##  ---------------------------------------------------  ##

# Group 1 -> RIS-S  &  PYW-S  &  STE-N  &  GIL-C
# Group 2 -> KLN-E  &  GIL-S  &  LTR-C  &  PYW-N
# Group 3 -> PYS-W  &  STE-W  &  GIL-N  &  LTR-E
# Group 4 -> PYN-N  &  LTR-W  &  PYW-C  &  STE-S

##  ---------------------------------------------------  ##
      # 2018 order (Ref, Con, Spr, SnS):
##  ---------------------------------------------------  ##

