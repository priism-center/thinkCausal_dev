validate_data_uploaded <- function(store, message = "Data must be first uploaded. Please see the 'Analyze - Upload Data' page."){
  # stop here if data hasn't been uploaded
  validate(need(nrow(store$analysis_data_uploaded_df) > 0,
                message))
}

validate_columns_assigned <- function(store){
  # stop here if columns haven't been assigned
  validate(need(nrow(store$analysis_data_assigned_df) > 0,
                "Columns must first be assigned. Please see 'Analyze - Upload Data' page."))
}

validate_data_verified <- function(store, req_only = FALSE){
  # stop here if data hasn't been uploaded and verified
  if (req_only) req(is.data.frame(store$verified_df))
  validate(need(is.data.frame(store$verified_df),
                "Data must be first uploaded and verified. Please see 'Analyze - Verify data types' page."))
}

validate_model_fit_ <- function(.model, req_only = FALSE){
  # stop here if model isn't fit yet
  if (req_only) req(inherits(.model, 'bartcFit'))
  validate(need(inherits(.model, "bartcFit"),
                "Model must first be fitted on the 'Analyze - Fit model' page"))
}

validate_model_fit <- function(store, req_only = FALSE){
  validate_model_fit_(store$analysis$model$model, req_only = req_only)
}

validate_design <- function(store){
  # stop here if design hasn't been specified
  validate(need(store$analysis_design %in% c('Observational', 'Randomized treatment', 'Block randomized treatment'),
                "Study design must first be specified on the 'Analyze - Describe data' page"))
}

validate_prespecifed_moderators <- function(store){
  validate(need(!rlang::is_null(store$analysis$subgroup$prespecified_subgroups),
                "No subgroup analyses were prespecifed"))
}

remove_downstream_data <- function(store, page = NULL){
  # remove downstream data so validate_* functions stop any processes naturally
  # usage: store <- remove_downstream_data(store, page = 'group)

  # page_order <- c('design', 'upload', 'group')
  # if (page %notin% page_order) stop(paste0('page must be one of ', page_order))

  if (page %in% c('design', 'upload')){
    store$user_modified_df <- NULL
    store$analysis$data$col_assignment_df <- NULL
    store$column_assignments <- NULL

    store$analysis$verify <- NULL
    store$analysis$model <- NULL
  }


  if (page == 'verify'){
    store$analysis$model
  }

  # always remove these
  store$verified_df <- NULL
  store$column_types <- NULL
  store$analysis$model$model <- NULL
  store$analysis$model$fit_good <- NULL

  return(store)
}
