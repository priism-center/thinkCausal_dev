library(shiny)

# for reading data
library(foreign)
library(readstata13)
library(openxlsx)
library(Hmisc) # for spss
library(readr)

# for javascript and Shiny tools
library(shinyjs) # for running javascript on the server-side
library(DT) # for javascript datatables
library(sortable) # for drag and drop divs

# for data munging and plotting
# library(tidyverse) # TODO: split this up to the individual packages
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(rpart.plot) # for plotting single regression tree; TODO: move to plotBart?

# for bart
library(plotBart) # devtools::install_github("joemarlo/plotBart")
library(bartCause)

# global options
options(shiny.reactlog = TRUE) # for testing; when running, hit Ctrl-F3 to see the reactivity tree
options(shiny.maxRequestSize = 10*1024^2) # increase maximum file upload size limit to 10mb


# data and objects --------------------------------------------------------

# violet color
violet_col <- "#5c5980"


# # data and objects for the concepts modules -----------------------------

# randomization module
## read in randomization df
randomization_df <- read_csv(
  'data/randomization_df.csv',
  col_types = cols(
    Cholesterol_LDL = col_double(),
    Cholesterol_HDL = col_double(),
    Age = col_double(),
    Genetic_disposition = col_double(),
    Packs_per_day = col_double(),
    Exercise_per_week = col_double(),
    treat = col_double()
  )
) %>% as.data.frame()
rownames(randomization_df) <- 1:nrow(randomization_df)


# UI files (this should always be last) -----------------------------------

# read in all the UI and module files
map(list.files('R', recursive = TRUE), function(file) source(file.path('R', file)))
map(list.files(file.path('UI', 'concepts')), function(file) source(file.path("UI", "concepts", file)))
map(list.files(file.path('UI', 'pages')), function(file) source(file.path("UI", "pages", file)))
map(list.files(file.path('UI', 'headers')), function(file) source(file.path("UI", "headers", file)))
