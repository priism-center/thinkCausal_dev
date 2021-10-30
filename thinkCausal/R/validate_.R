
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
  validate(need(nrow(store$categorical_df) > 0,
                "Data must be first grouped. Please see 'Analyze-Data-Group' page."))
}

# validate_data_user <- function(store){
#   # stop here if no user data
#   validate(need(is.data.frame(store$user_modified_df),
#                 'Internal: Awaiting UI to render and create dataframe'))
# }

validate_data_selected <- function(store){
  # stop here if data hasn't been uploaded and selected
  validate(need(is.data.frame(store$selected_df),
                "Data must be first uploaded and selected. Please see 'Analyze-Data-Verify' page."))
}

validate_model_fit_ <- function(.model){
  # stop here if model isn't fit yet
  validate(need(inherits(.model, "bartcFit"), 
                "Model must first be fitted on the 'Analyze-Model' page"))
}

validate_model_fit <- function(store){
  validate_model_fit_(store$model_results)
}
