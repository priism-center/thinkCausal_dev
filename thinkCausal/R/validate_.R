
validate_data_uploaded <- function(store){
  # stop here if data hasn't been uploaded
  validate(need(nrow(store$uploaded_df) > 0,
                "Data must be first uploaded. Please see the 'Analyze-Data-Upload' page"))
}

validate_columns_assigned <- function(store){
  # stop here if columns haven't been assigned
  validate(need(nrow(store$col_assignment_df) > 0,
                "Columns must first be assigned. Please see 'Analyze-Data-Upload' page."))
}

validate_data_grouped <- function(store){
  # stop here if data hasn't been grouped
  validate(need(nrow(store$grouped_df) > 0,
                "Data must be first grouped. Please see 'Analyze-Data-Group' page."))
}

validate_data_verified <- function(store){
  # stop here if data hasn't been uploaded and verified
  validate(need(is.data.frame(store$verified_df),
                "Data must be first uploaded and verified. Please see 'Analyze-Data-Verify' page."))
}

validate_model_fit_ <- function(.model){
  # stop here if model isn't fit yet
  validate(need(inherits(.model, "bartcFit"), 
                "Model must first be fitted on the 'Analyze-Model' page"))
}

validate_model_fit <- function(store){
  validate_model_fit_(store$model_results)
}

validate_design <- function(store){
  # stop here if design hasn't been specified
  validate(need(store$analysis_design %in% c('Observational', 'Randomized treatment', 'Block randomized treatment'),
                "Study design must first be specified on the 'Analyze-Design' page"))
}

remove_downstream_data <- function(store, page = NULL){
  # remove downstream data so validate_* functions stop any processes naturally
  # usage: store <- remove_downstream_data(store, page = 'group)
  
  # page_order <- c('design', 'upload', 'group')
  # if (page %notin% page_order) stop(paste0('page must be one of ', page_order))
  
  if (page %in% c('design', 'upload')){
    store$grouped_df <- NULL
    store$user_modified_df <- NULL
    store$col_assignment_df <- NULL
    store$column_assignments <- NULL
  }
  
  if (page == 'group'){
    
  }
  
  if (page == 'verify'){
    
  }

  # always remove these
  store$verified_df <- NULL
  store$column_types <- NULL
  store$model_results <- NULL
  store$model_fit_good <- NULL
  
  return(store)
}
