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
                 inputId = paste0(ns_prefix, i, '_type'),
                 label = NULL,
                 value = column_types[i])
             )
      ),
      column(width = 4, 
             textInput(
               inputId = paste0(ns_prefix, i, "_rename"),
               label = NULL,
               value = all_col_names[i])
      ),
      column(width = 4, 
             selectInput(
               inputId = paste0(ns_prefix, i, "_changeDataType"),
               label = NULL, 
               choices = c('Continuous', 'Categorical', 'Binary'),
               selected = default_data_types[i])
      ),
      column(width = 2, 
             shinyjs::disabled(
               textInput(
                 inputId = paste0(ns_prefix, i, "_percentNA"),
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