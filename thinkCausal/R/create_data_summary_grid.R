#' Create a UI displaying the role, variable name, variable type, and percent NA for each column in a dataframe
#'
#' @param .data a dataframe
#' @param default_data_types a vector of default data types. Usually from convert_data_type_to_simple()
#' @param ns_prefix a string denoting a prefix to use when creating inputIds
#'
#' @return HTML code
#' @export
#'
#' @examples
#' # shiny server
#' # renderUI({
#' #   create_data_summary_grid(
#' #     .data = .data,
#' #     default_data_types = default_data_types,
#' #     ns_prefix = 'analysis_data_'
#' #   )
#' # })
create_data_summary_grid <- function(ns, .data, default_data_types, ns_prefix, design, blocking_variables = NULL, survey_weight = NULL, random_effect = NULL){
  
  # set indices to map over
  all_col_names <- colnames(.data)
  indices <- seq_along(all_col_names)
  n_blocks <- length(blocking_variables)
  n_weight <- length(survey_weight)
  n_random_effects <- length(random_effect)
  n_covariates <- length(all_col_names)-(2 + n_blocks + n_weight + n_random_effects)
  
  
  # create vector of column type names and vector variable type choices (dependent on role)
  column_types <- c(
    'Treatment', 
    'Outcome', 
    rep('Block', n_blocks), 
    rep('Random Effect', n_random_effects), 
    rep('Survey Weight', n_weight), 
    rep('Covariate', n_covariates)
  )
  # order is very important see comment in verify_data_server.R
  variable_type_choices <- c(
    list(c('Categorical', 'Binary')),
    list(c('Continuous', 'Binary')),
    if(n_blocks > 0) lapply(1:n_blocks, function(x) c('Categorical', 'Binary')), 
    if(n_random_effects > 0) lapply(1:n_random_effects, function(x) c('Categorical')), 
    if(n_weight > 0) lapply(1:n_weight, function(x) c('Continuous')), 
    lapply(1:n_covariates, function(x) c('Continuous', 'Categorical', 'Binary'))
  )
  

  # render the header to the table
  UI_header <- fluidRow(
    column(2, h5(create_info_icon('Role', 'You already specified variable roles on the data upload page.'))),
    column(4, h5(create_info_icon('Rename variable', 'Type in a new variable name, the software will take care of the rest.'))),
    column(4, h5(create_info_icon('Verify variable type', 'Confirm that the assigned variable type matches your data. You can manually override the pre-selected type by clicking the dropdown.'))),
    column(2, h5(create_info_icon('Percent NA', 'The percentage of missing data within each. Some changes you make to your data may change the Percent NA.')))
  )
  
  # render the rows
  UI_grid <- lapply(indices, function(i){
    fluidRow(
      column(width = 2,
             shinyjs::disabled(
               textInput(
                 inputId = ns(paste0(ns_prefix, "_", i, '_type')),
                 label = NULL,
                 value = column_types[i])
             )
      ),
      column(width = 4, 
             textInput(
               inputId = ns(paste0(ns_prefix, "_", i, "_rename")),
               label = NULL,
               value = all_col_names[i])
      ),
      column(width = 4, 
             if (i == 1){
               shinyjs::disabled(
                 selectInput(
                   inputId = ns(paste0(ns_prefix, "_", i, "_changeDataType")),
                   label = NULL, 
                   choices = variable_type_choices[[i]],
                   selected = default_data_types[i])
               )
             } else {
               selectInput(
                 inputId = ns(paste0(ns_prefix, "_", i, "_changeDataType")),
                 label = NULL, 
                 choices = variable_type_choices[[i]],
                 selected = default_data_types[i])
             }
      ),
      column(width = 2, 
             shinyjs::disabled(
               textInput(
                 inputId = ns(paste0(ns_prefix, "_", i, "_percentNA")),
                 label = NULL, 
                 placeholder = 'TBD')
             )
      )
    )
  })
  
  # combine the header and the rows
  UI_table <- tagList(UI_header, UI_grid)
  
  return(UI_table)
}
