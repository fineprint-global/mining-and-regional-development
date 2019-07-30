
# Packages

library(tidyverse)
library(sp)
library(sf)
library(spdep)
library(MASS)
library(OECD)
library(dummies)
library(xtable)
library(gridExtra)
library(scales)

# Data
source("R/data.R")

# Run model
source("R/model_loop.R")

# Figure 1 global metal ore extraction
source("R/figure_1.R")

# Figure 2 mining properties map
# Due to copyright reasons, we cannot provide this data 

# Figure 3 scatter plots extraction vs. growth
source("R/figure_3.R")

# Figure 4 posterior distribution variances 
source("R/figure_4.R")

# Figure 5 cumulated ore extraction
# Due to copyright reasons, we cannot provide this data 

