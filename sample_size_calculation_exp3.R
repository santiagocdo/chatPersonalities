# Sample size calculation for Preregistration
# see: https://aspredicted.org/9yg6-y3xp.pdf)

# library
if (!require(pwr)) {install.packages("pwr")}; library(pwr)

# We calculate the sample size with function pwr.t.test from the R package pwr. 
# Based on a previous pilot, we need 90 participants to detect an effect size 
# of .3 with a power of 0.8 and an alpha of 0.05.
pwr.t.test(d = .3, power = .8, sig.level = .05, type = "paired")
