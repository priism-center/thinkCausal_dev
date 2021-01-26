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

# violet color
violet_col <- "#5c5980"

# ensure there is a z column (we'll have to standardize column names in the background)
user_data$z <- user_data$treat

map(list.files('UI/tabs'), function(file) source(file.path("UI/tabs", file)))
map(list.files('UI/nodes'), function(file) source(file.path("UI/nodes", file)))
map(list.files('UI/dropdowns'), function(file) source(file.path("UI/dropdowns", file)))
