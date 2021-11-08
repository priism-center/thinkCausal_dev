### this is a dev script that mimics the Shiny app analysis workflow
### useful for testing functions
library(dplyr)
library(readr)
library(bartCause)
theme_set(theme_minimal())


# setup -------------------------------------------------------------------

X <- read_csv("data/lalonde.csv")
X <- dplyr::select(X, 'treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')
X <- clean_auto_convert_logicals(X)

treatment_v <- X[, 1]
response_v <- X[, 2]
confounders_mat <- as.matrix(X[, 3:ncol(X)])

# run model    
model_results <- bartCause::bartc(
  response = response_v,
  treatment = treatment_v,
  confounders = confounders_mat,
  estimand = 'ate',
  commonSup.rule = 'none'
)


# functions to test -------------------------------------------------------

classes_categorical <- c('logical', 'character', 'factor')
classes_continuous <- c('numeric', 'double', 'integer')
cols_by_class <- split(colnames(X), sapply(X, function(x) class(x)[1]))
store$selected_df_categorical_vars <- as.vector(unlist(cols_by_class[classes_categorical]))
store$selected_df_numeric_vars <- as.vector(unlist(cols_by_class[classes_continuous]))



plot_exploration(
  .data = X,
  plot_type = 'Boxplot', #c("Pairs", 'Scatter', 'Histogram', 'Density', 'Boxplot'),
  .x = 're78',
  .y = 'age',
  .fill = 'age',
  .fill_static = "#5c5980",
  .size = 'age',
  .alpha = 0.5,
  vars_pairs,
  n_bins = 30,
  jitter = FALSE,
  .groups = 'None',
  .facet = 'None',
  .facet_second = 'None',
  include_regression = 'None'
  )

