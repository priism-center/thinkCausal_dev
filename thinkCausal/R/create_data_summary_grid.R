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
create_data_summary_grid <- function(.data, default_data_types, ns_prefix){
  
  # set indices to map over
  all_col_names <- colnames(.data)
  indices <- seq_along(all_col_names)
  
  # create vector of column type names
  column_types <- c('Treatment', 'Response', rep('Covariate', length(all_col_names)-2))
  
  # render the header to the table
  UI_header <- fluidRow(
    column(2, h5('Role')),
    column(4, h5('Rename variable')),
    column(4, h5('Verify variable type')),
    column(2, h5('Percent NA'))
  )
  
  # render the rows
  UI_grid <- lapply(indices, function(i){
    fluidRow(
      column(width = 2,
             shinyjs::disabled(
               textInput(
                 inputId = paste0(ns_prefix, "_", i, '_type'),
                 label = NULL,
                 value = column_types[i])
             )
      ),
      column(width = 4, 
             textInput(
               inputId = paste0(ns_prefix, "_", i, "_rename"),
               label = NULL,
               value = all_col_names[i])
      ),
      column(width = 4, 
             selectInput(
               inputId = paste0(ns_prefix, "_", i, "_changeDataType"),
               label = NULL, 
               choices = c('Continuous', 'Categorical', 'Binary'),
               selected = default_data_types[i])
      ),
      column(width = 2, 
             shinyjs::disabled(
               textInput(
                 inputId = paste0(ns_prefix, "_", i, "_percentNA"),
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
