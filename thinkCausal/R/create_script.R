#' Build a reproducible script which mimics the thinkCausal work-flow
#'
#' @return string
#' @export
#' 
#' @examples 
#' create_script(
#'  uploaded_file_name = 'test.csv',
#'  uploaded_file_type = 'csv',
#'  uploaded_file_header = 'TRUE',
#'  uploaded_file_delim = ',',
#'  selected_columns = c("Z", "Y", "X1", 'X2', "X3"),
#'  column_names = c("treatment", "response", "covariate1", "covariate2", "covariate3"),
#'  estimand = 'att',
#'  common_support = 'none'
#' )
create_script <- function(uploaded_file_name, uploaded_file_type, uploaded_file_header, uploaded_file_delim, selected_columns, column_names, change_data_type, estimand, common_support){

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

  
  
  # construct strings for each section
  script_head <- paste0(
  "library(tidyverse)", "\n",
  "library(bartCause)", "\n",
  "library(plotBart) #devtools::install_github('joemarlo/plotBart')", "\n",
  "source('clean_auto_convert_logicals.R')", "\n",
  "source('clean_dummies_to_categorical.R')",
  "\n\n"
  )
  
  script_data_munge <- paste0(
  "# select columns and rename", "\n",
  "X <- ", file_readr, "\n",
  "X <- X[, ", selected_columns, "]", "\n",
  "X <- clean_auto_convert_logicals(X)",
  "\n\n"
  )
  
  # add data type changes
  script <- c()
  options(useFancyQuotes = FALSE)
  for (i in 1:length(change_data_type)) {
    tmp <- paste0(
      "# find the indexes of dummy variables of the same group", "\n",
       gsub(" ",'', change_data_type[[i]][1]), ' <- c(', sapply(strsplit(paste0(change_data_type[[i]][-1], collapse = ', '), '[, ]+'), function(x) toString(dQuote(x))),  ')','\n',
       "X <- clean_dummies_to_categorical(X, ", gsub(" ",'', change_data_type[[i]][1]), ")", "\n")
    script <- c(script, tmp)
  }
  script_data_type <- paste0(script, collapse = "\n")
  
  script_rename <- paste0("colnames(X) <- ", column_names, "\n\n")
  
  script_model <- paste0(
  "# run model", "\n",
  "treatment_v <- X[, 1]", "\n",
  "response_v <- X[, 2]", "\n",
  "confounders_mat <- as.matrix(X[, 3:ncol(X)])", "\n",
  "model_results <- bartCause::bartc(
    response = response_v,
    treatment = treatment_v,
    confounders = confounders_mat,
    estimand = '", estimand, "',
    commonSup.rule = '", common_support, "'\n",
  ")",
  "\n\n"
  )
  
  script_plots <- paste0(
  "# plot results and diagnostics", "\n",
  "plot_ITE(model_results) + labs(title = 'My individual treatment effects')", "\n",
  "plot_trace(model_results)", "\n",
  "plot_diagnostic_common_support(model_results, .rule = '", common_support, "')",
  "\n"
  )
  
  # combine into one string
  script <- paste0(script_head, script_data_munge, script_data_type, script_rename, script_model, script_plots)
  
  return(script)
}
