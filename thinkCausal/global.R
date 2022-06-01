library(shiny)

# for reading data
library(readstata13)
library(openxlsx)
library(Hmisc) # for spss
library(readr) # for csv, txt

# for javascript tools
library(shinyjs) # for running javascript on the server-side
library(DT) # for javascript datatables
library(sortable) # for drag and drop divs
library(shinydisconnect) # for showing a popup when shiny crashes

# for data munging and plotting
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(patchwork)
library(ggdendro)

# for bart
library(plotBart)
library(bartCause)

# global options
#options(shiny.reactlog = TRUE) # for testing; when running, hit Ctrl-F3 to see the reactivity tree
options(shiny.maxRequestSize = 10*1024^2) # increase maximum file upload size limit to 10mb
# options(shiny.autoreload = TRUE)

# set seed
set.seed(2)

# set module IDs
module_ids <- list(
  learning = list(
    randomization = 'learning_randomization',
    post_treatment = 'learning_post_treatment',
    estimands = 'learning_estimands',
    bias_efficiency = 'learning_bias_efficiency',
    rct_covariates = 'learning_rct_covariates',
    ignorability = 'learning_ignorability',
    confounders_measured = 'learning_confounders_measured',
    potential_outcomes = "learning_potential_outcomes"
  ),
  analysis = list(
    design = 'analysis_design',
    data = 'analysis_upload_data',
    verify = 'analysis_verify_data', 
    eda = 'analysis_eda',
    balance = 'analysis_balance', 
    overlap = 'analysis_overlap', 
    model = 'analysis_model',
    diagnostic = 'analysis_diagnostic',
    results = 'analysis_results',
    subgroup = 'analysis_subgroup' 
  )
)


# source ------------------------------------------------------------------

# functions
map(list.files('R', recursive = TRUE), function(file) source(file.path('R', file)))

# modules
path_modules <- list.files('modules', pattern = "(_module.R)$", recursive = TRUE, full.names = TRUE)
map(path_modules, function(file) source(file))
rm(path_modules)

# UI 
map(list.files(file.path('UI', 'concepts')), function(file) source(file.path("UI", "concepts", file)))
map(list.files(file.path('UI', 'pages')), function(file) source(file.path("UI", "pages", file)))
map(list.files(file.path('UI', 'headers')), function(file) source(file.path("UI", "headers", file)))


# create namespace functions ----------------------------------------------

# make functions for each id and save them in the global environment
# purrr::map(unlist(module_ids), make_namespace_function)

theme_set(theme_minimal_no_transparency())