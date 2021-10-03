library(shiny)

# for reading data
library(foreign)
library(readstata13)
library(openxlsx)

# for javascript and Shiny tools
library(shinyjs) # for running javascript on the server-side
library(shinyWidgets) # for alerts
library(DT) # for javascript datatables
library(sortable) # for drag and drop divs
library(kableExtra) # for one html table. Can easily replace with one custom function -- see https://github.com/joemarlo/sequenchr/blob/57375aebe59330cfcc02d69a7fd98ec476a6bf28/R/helpers.R#L173

# for data munging and plotting
library(tidyverse) # TODO: split this up to the individual packages
library(patchwork) # for combining ggplots
library(viridis) # for color blind sensitive colors
library(rpart.plot) # for plotting single regression tree
library(gtools) # for combination function
# for bart
library(plotBart) # devtools::install_github("joemarlo/plotBart")
library(bartCause)

# global options
options(shiny.reactlog = TRUE) # for testing; when running, hit Ctrl-F3 to see the reactivity tree
options(shiny.maxRequestSize = 10*1024^2) # increase maximum file upload size limit to 10mb


## delete
# interactive_table <- create_interactive_table(.data, correct_answers, extra_header, extra_header_widths, table_id = 'mytable', ns = NS('yyp'))
##

# data and objects --------------------------------------------------------

# violet color
violet_col <- "#5c5980"

# text for model specification
analysis_model_text <- list(
  'design' = list(
    'non_random' = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat",
    'random' = 'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
    'quasi' = 'Sed euismod nisi porta lorem mollis aliquam ut. Et sollicitudin ac orci phasellus egestas. Purus viverra accumsan in nisl nisi scelerisque eu ultrices vitae.',
    "None" = 'Please make a selection'
  ),
  'estimand' = list(
    'ATE' = 'Vestibulum lorem sed risus ultricies. Leo vel fringilla est ullamcorper eget nulla. Enim diam vulputate ut pharetra sit amet aliquam id diam.',
    'ATT' = 'Rutrum tellus pellentesque eu tincidunt tortor aliquam. Nulla facilisi etiam dignissim diam quis. Aliquet nibh praesent tristique magna sit amet purus gravida. Integer feugiat scelerisque varius morbi enim nunc.',
    'ATC' = 'Commodo ullamcorper a lacus vestibulum sed arcu non odio. Augue eget arcu dictum varius duis at consectetur. Duis at tellus at urna condimentum mattis. Ultricies mi eget mauris pharetra.',
    "None" = 'Please make a selection'
  ),
  'support' = list(
    'none' = "None",
    'sd' = 'Condimentum lacinia quis vel eros donec. Et leo duis ut diam. Adipiscing elit duis tristique sollicitudin nibh sit. Ut placerat orci nulla pellentesque dignissim enim sit amet.',
    'chi' = 'Integer vitae justo eget magna fermentum iaculis eu non. Sed libero enim sed faucibus turpis in. At volutpat diam ut venenatis.',
    "None" = 'Please make a selection'
  )
)


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
