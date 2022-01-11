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
create_script <- function(uploaded_file_name, uploaded_file_type, uploaded_file_header, uploaded_file_delim, selected_columns, column_names, change_data_type, descriptive_plot, overlap_plot, estimand, common_support){

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
  "source('clean_dummies_to_categorical.R')", "\n",
  "source('plot_exploration.R')", 
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
  first_group_dmmies <- 
  for (i in 1:length(change_data_type)) {
    # only if there are elements in a group, generate the code for converting the dummies of the group
    if(length(change_data_type[[i]][-1]) > 0 ){
      tmp <- paste0(
        "# find the indexes of dummy variables of the same group", "\n",
        gsub(" ",'', change_data_type[[i]][1]), ' <- c(', sapply(strsplit(paste0(change_data_type[[i]][-1], collapse = ', '), '[, ]+'), function(x) toString(dQuote(x))),  ')','\n',
        "X <- clean_dummies_to_categorical(X, ", gsub(" ",'', change_data_type[[i]][1]), ")", "\n")
      script <- c(script, tmp)
    }
  }
  script_data_type <- paste0(script, collapse = "\n")
  
  script_rename <- paste0("colnames(X) <- ", column_names, "\n\n")
 
  # change column names for the following eda and analysis 
  script_data_verified <- if(!is.null(overlap_plot)){
    paste0(
      "# new column names", "\n",
      "old_col_names <- colnames(X)", "\n",
      "new_col_names <- paste0(c('Z', 'Y', rep('X', length(old_col_names)-2)), '_', old_col_names)", "\n",
      "colnames(X) <- new_col_names", "\n",
      "treatment_col <- '", overlap_plot[1,2], "'\n",
      "response_col <- '", overlap_plot[1,3], "'\n",
      "column_types <- clean_detect_column_types(X)", "\n",
      "cols_continuous <- column_types$continuous", "\n",
      "confounder_cols <- grep('^X_', cols_continuous, value = TRUE)","\n\n"
    )
  }else{
    paste0(
      "# new column names", "\n",
      "old_col_names <- colnames(X)", "\n",
      "new_col_names <- paste0(c('Z', 'Y', rep('X', length(old_col_names)-2)), '_', old_col_names)","\n",
      "colnames(X) <- new_col_names", "\n\n"
    )
  }
    
  
  eda <- if(!is.null(descriptive_plot)){
    script <- c("# descriptive plots")
    for (i in 1:nrow(descriptive_plot)) {
      if (descriptive_plot[i,6] != "None"){ # plots that are specified .shape
        #  first convert the type of the variable specified for .shape argument
        tmp <- paste0('descriptive_plot', i, ' <- ',"X %>% mutate(", descriptive_plot[i,6], " = as.character(", descriptive_plot[i,6], ")) %>% ",'\n',
                      'plot_exploration(', '\n',
                      '.plot_type = "', descriptive_plot[i,1],'", \n',
                      '.x = "', descriptive_plot[i,2], '", \n',
                      '.y = "', descriptive_plot[i,3], '", \n',
                      '.fill = "', descriptive_plot[i,4], '", \n',
                      '.fill_static = "', descriptive_plot[i,5], '", \n',
                      '.shape = "', descriptive_plot[i,6], '", \n',
                      '.size = "', descriptive_plot[i,7], '", \n',
                      '.alpha = ', descriptive_plot[i,8], ', \n',
                      '.vars_pairs = ', descriptive_plot[i,9], ', \n',
                      '.n_bins = ', descriptive_plot[i,10], ', \n',
                      '.jitter = ', descriptive_plot[i,11], ', \n',
                      '.groups = "', descriptive_plot[i,12], '", \n',
                      '.facet = "', descriptive_plot[i,13], '", \n',
                      '.facet_second = "', descriptive_plot[i,14], '", \n',
                      '.include_regression = "', descriptive_plot[i,15], '"\n',
                      ')', '\n',
                      'descriptive_plot', i, '\n')
      }else{ # plots that aren't specified .shape
        tmp <- paste0('descriptive_plot', i, ' <- ',
          'plot_exploration(', '\n',
          '.data = X,', '\n',
          '.plot_type = "', descriptive_plot[i,1],'", \n',
          '.x = "', descriptive_plot[i,2], '", \n',
          '.y = "', descriptive_plot[i,3], '", \n',
          '.fill = "', descriptive_plot[i,4], '", \n',
          '.fill_static = "', descriptive_plot[i,5], '", \n',
          '.shape = "', descriptive_plot[i,6], '", \n',
          '.size = "', descriptive_plot[i,7], '", \n',
          '.alpha = ', descriptive_plot[i,8], ', \n',
          '.vars_pairs = ', descriptive_plot[i,9], ', \n',
          '.n_bins = ', descriptive_plot[i,10], ', \n',
          '.jitter = ', descriptive_plot[i,11], ', \n',
          '.groups = "', descriptive_plot[i,12], '", \n',
          '.facet = "', descriptive_plot[i,13], '", \n',
          '.facet_second = "', descriptive_plot[i,14], '", \n',
          '.include_regression = "', descriptive_plot[i,15], '"\n',
          ')', '\n',
          'descriptive_plot', i, '\n')
      }
      script <- c(script, tmp)
    }
  }else{
    script <- '\n'
  }
  
  script_eda <- paste0(script, collapse = "\n")
  
  overlap <- if(!is.null(overlap_plot)){
    script <- c("\n# common support plot")
    # if there is a plot of p-score, calcultae p-score
    if(2 %in% overlap_plot[,1]){
      tmp <- paste0('# calculate pscores', '\n',
                    'pscores <- plotBart::propensity_scores(', '\n',
                    '.data = X,', '\n',
                    'treatment = treatment_col,', '\n',
                    'response = response_col,', '\n',
                    'confounders = confounder_cols', '\n',
                    ')', '\n')
      script <- c(script, tmp)
    }
    
    for(i in 1:nrow(overlap_plot)){
      if(as.numeric(overlap_plot[i,1]) == 1){
        tmp <- paste0(
          'overlap_select_var <- c(', sapply(strsplit(paste0(as.character(overlap_plot[i,4]), collapse = ', '), '[, ]+'), function(x) toString(dQuote(x))), ')', '\n',
          'overlap_plot', i, ' <- ',
          'plotBart::plot_overlap_vars(', '\n',
          '.data = X,', '\n',
          'treatment = treatment_col,', '\n',
          'confounders = overlap_select_var,', '\n',
          'plot_type = "', overlap_plot[i,5], '"\n',
          ')', '\n',
          'overlap_plot', i, '\n')
      }else{
        tmp <- paste0('overlap_plot', i, ' <- ',
          'plotBart::plot_overlap_pScores(', '\n',
          '.data = X,', '\n',
          'treatment = treatment_col,', '\n',
          'response = response_col,', '\n',
          'confounders = confounder_cols,', '\n',
          'plot_type = "', overlap_plot[i,5], '",\n',
          'pscores = pscores', '\n',
          ')', '\n',
          'overlap_plot', i, '\n')
      }
      script <- c(script, tmp)
    }
  }else{
    script <- '\n'
  }
  
  script_overlap <- paste0(script, collapse = "\n")
  
  script_model <- paste0(
  "\n# run model", "\n",
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
  script <- paste0(script_head, script_data_munge, script_data_type, script_rename, 
                   script_data_verified, script_eda, script_overlap, script_model, script_plots)
  
  return(script)
}
