require(foreign)
require(readstata13)
require(openxlsx)
require(tidyverse)
require(DT)
require(shinyBS)
require(shiny)
library(shinythemes)
library(shinyWidgets) # for slider skin
library(arm) # currently just for lalonde dataset
library(viridis) # for color blind sensitive colors
library(bartCause)
theme_set(theme_minimal())

# toy dataset
data(lalonde)
user_data <- lalonde
col_names <- names(user_data)
X_names <- setdiff(col_names, "treat")
categorical_names <- c("black", "hisp", "married", "nodegr", "treat") # will need to figure out automatically which vars are categorical
user_data[categorical_names] <- map_dfc(categorical_names, function(col_name){
  col <- as.logical(user_data[[col_name]])
})

# read in randomization df
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

# violet color
violet_col <- "#5c5980"

# ensure there is a z column (we'll have to standardize column names in the background)
user_data$z <- user_data$treat

# text for model specification
analysis_model_text <- list(
  'design' = list(
    'non_random' = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat",
    'random' = 'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
    'quasi' = 'Sed euismod nisi porta lorem mollis aliquam ut. Et sollicitudin ac orci phasellus egestas. Purus viverra accumsan in nisl nisi scelerisque eu ultrices vitae.'
  ),
  'estimand' = list(
    'ATE' = 'Vestibulum lorem sed risus ultricies. Leo vel fringilla est ullamcorper eget nulla. Enim diam vulputate ut pharetra sit amet aliquam id diam.',
    'ATT' = 'Rutrum tellus pellentesque eu tincidunt tortor aliquam. Nulla facilisi etiam dignissim diam quis. Aliquet nibh praesent tristique magna sit amet purus gravida. Integer feugiat scelerisque varius morbi enim nunc.',
    'ATC' = 'Commodo ullamcorper a lacus vestibulum sed arcu non odio. Augue eget arcu dictum varius duis at consectetur. Duis at tellus at urna condimentum mattis. Ultricies mi eget mauris pharetra.',
    "None" = 'None selected yet'
  ),
  'support' = list(
    'none' = "None",
    'sd' = 'Condimentum lacinia quis vel eros donec. Et leo duis ut diam. Adipiscing elit duis tristique sollicitudin nibh sit. Ut placerat orci nulla pellentesque dignissim enim sit amet.',
    'chi' = 'Integer vitae justo eget magna fermentum iaculis eu non. Sed libero enim sed faucibus turpis in. At volutpat diam ut venenatis.'
  )
)

# read in all the UI and module files
map(list.files('R', recursive = TRUE), function(file) source(file.path('R', file)))
map(list.files(file.path('UI', 'concepts')), function(file) source(file.path("UI", "concepts", file)))
map(list.files(file.path('UI', 'tabs')), function(file) source(file.path("UI", "tabs", file)))
map(list.files(file.path('UI', 'nodes')), function(file) source(file.path("UI", "nodes", file)))
map(list.files(file.path('UI', 'dropdowns')), function(file) source(file.path("UI", "dropdowns", file)))
