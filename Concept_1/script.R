
  library(tidyverse)
  library(bartCause)
  library(plotBart)
  source('clean_auto_convert_logicals.R')
  
  # select columns and rename
  X <- readr::read_csv(file = 'lalonde.csv', col_names = TRUE)
  X <- X[, c('treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')]
  colnames(X) <- c('treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')
  X <- clean_auto_convert_logicals(X)
  
  # run model  
  treatment_v <- X[, 1]
  response_v <- X[, 2]
  confounders_mat <- as.matrix(X[, 3:ncol(X)])
  model_results <- bartCause::bartc(
    response = response_v,
    treatment = treatment_v,
    confounders = confounders_mat,
    estimand = 'ate',
    commonSup.rule = 'none'
  )
  
  # plot results and diagnostics
  plot_ITE(X)
  plot_trace(X)
  plot_diagnostic_common_support(X, .rule = 'none')
  
