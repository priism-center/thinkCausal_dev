# ----- ----- ----- ----- title
#
# Title: thinkCausal Shiny App
# Subtitle: Concepts - All Confounders Measured
# 
# Description: illustrate why ignorability is important for causal inference 
#              using regression analysis.

# Dependencies -----------------------------------------------------------------
library(shiny)
library(shinythemes)
library(xtable)
library(MASS)
library(tidyverse)


# Color palette ----------------------------------------------------------------
blue <- "#335C81"
red <- "#F42C04"


map(list.files('R', recursive = TRUE), function(file) source(file.path('R', file)))







