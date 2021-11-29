library(shiny)

# for reading data
library(foreign)
library(readstata13)
library(openxlsx)
library(Hmisc) # for spss
library(readr)

# for javascript tools
library(shinyjs) # for running javascript on the server-side
library(DT) # for javascript datatables
library(sortable) # for drag and drop divs

# for data munging and plotting
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)

# for bart
library(plotBart) # devtools::install_github("joemarlo/plotBart", ref = "0.1.0")
library(bartCause)

# global options
options(shiny.reactlog = TRUE) # for testing; when running, hit Ctrl-F3 to see the reactivity tree
options(shiny.maxRequestSize = 10*1024^2) # increase maximum file upload size limit to 10mb


# UI files (this should always be last) -----------------------------------

# read in all the UI and module files
map(list.files('R', recursive = TRUE), function(file) source(file.path('R', file)))
map(list.files(file.path('UI', 'concepts')), function(file) source(file.path("UI", "concepts", file)))
map(list.files(file.path('UI', 'pages')), function(file) source(file.path("UI", "pages", file)))
map(list.files(file.path('UI', 'headers')), function(file) source(file.path("UI", "headers", file)))
