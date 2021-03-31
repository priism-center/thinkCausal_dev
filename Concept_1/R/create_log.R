
#' Build a reproducible 'log' which mimics the thinkCausal work-flow
#'
#' @return string
#' @export
#'
#' @examples
create_log <- function(uploaded_file_name, uploaded_file_type, uploaded_file_header, uploaded_file_delim, selected_columns, column_names, estimand, common_support){
  
  # choose which file readr was used
  file_readr <- switch(
    uploaded_file_type,
    csv = paste0("readr::read_csv(file = '", uploaded_file_name, "', col_names = ", uploaded_file_header, ")"),
    dta = paste0("readstata13::read.dta13(file = '", uploaded_file_name, "')"),
    xlsx = paste0("xlsx::read.xlsx(file = '", uploaded_file_name, "')"),
    txt = paste0("readr::read_delim(file = '", uploaded_file_name, "', delim = ", uploaded_file_delim, ", col_names = ", uploaded_file_header, ")"),
    spss = paste0("Hmisc::spss.get(file = '", uploaded_file_name, "')")
    )
  
  # columns selected
  selected_columns <- paste0("c('", paste0(selected_columns, collapse = "', '"), "')")
  column_names <- paste0("c('", paste0(column_names, collapse = "', '"), "')")

  # add data type changes
  
  # TODO: add estimand
  
  # construct strings for each section
  log_head <- "
  library(tidyverse)
  library(bartCause)
  library(plotBart)
  source('clean_auto_convert_logicals.R')
  "
  
  log_data_munge <- paste0("
  # select columns and rename
  X <- ", file_readr, "
  X <- X[, ", selected_columns, "]
  colnames(X) <- ", column_names, "
  X <- clean_auto_convert_logicals(X)
  "
  )
  
  log_model <- paste0(
  "
  # run model  
  treatment_v <- X[, 1]
  response_v <- X[, 2]
  confounders_mat <- as.matrix(X[, 3:ncol(X)])
  model_results <- bartCause::bartc(
    response = response_v,
    treatment = treatment_v,
    confounders = confounders_mat,
    estimand = '", estimand, "',
    commonSup.rule = '", common_support, "'
  )
  "
  )
  
  log_plots <- paste0(
  "
  # plot results and diagnostics
  plot_ITE(X)
  plot_trace(X)
  plot_diagnostic_common_support(X, .rule = '", common_support, "')
  "
  )
  
  # combine into one string
  log <- paste0(log_head, log_data_munge, log_model, log_plots)
  
  return(log)
}
