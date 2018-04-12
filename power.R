##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                    # Lyon Thesis -- Chapter 2: "Bees"
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE
  ## To do a power analysis on the proposed study design of the bee chapter

# START ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bee.Project")

# Load libraries
library(pwr); library(simr)

##  -----------------------------------------------------  ##
          # Using "pwr" Library ####
##  -----------------------------------------------------  ##

# What if we treat this as a balanced ANOVA
  ## it almost certainly won't be, but this is a good simplifying assumption

# For both PBG and fescue projects there are three treatments (1 YSB vs. 2 vs. 3 & Con vs. Spr vs. SnS)
# But high and low are nested within each too, so there are really 6 groups (sorta)


# Let's assume a very small effect size ("f" in the above equation)
pwr.anova.test(k = 6, n = 6, f = 0.1, sig.level = 0.05)
  ## Very low power...

# What if the effect size was bigger?
pwr.anova.test(k = 6, n = 6, f = 0.25, sig.level = 0.05)
pwr.anova.test(k = 6, n = 6, f = 0.4, sig.level = 0.05)

##  -----------------------------------------------------  ##
        # Using "simr" Library ####
##  -----------------------------------------------------  ##
# Let's get some of our terms set here

# First need to simulate a faux mixed-effect model


sim.mem <- makeLmer(formula, fixef, VarCorr, sigma, data)

# From PDF on CRAN
lm1 <- lmer(y ~ x + (x|g), data=simdata)
lm0 <- lmer(y ~ x + (1|g), data=simdata)
anova(lm1, lm0)
compare(. ~ x + (1|g))(lm1)
rcompare(~ (1|g))(lm1)
## Not run: powerSim(fm1, compare(. ~ x + (1|g)))



# END ####




