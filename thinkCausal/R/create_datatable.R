#' Create a pretty datatable
#' 
#' Create an HTML datatable using the JavaScript library DataTables. This is a wrapper around DT::datatable with commonly used arguments set as defaults.
#'
#' @param .data a dataframe
#' @param digits number of digits to round continuous columns to
#' @param ... arguments to be passed to DT::datatable. Typically a dataframe
#' @author Joe Marlo
#'
#' @return HTML datatable
#' @export 
#' 
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @importFrom DT datatable
#' 
#' @examples
#' X <- data.frame(x = 1:10, y = 1:10)
#' create_datatable(X)
#' 
#' ## within Shiny
#' # UI
#' #DT::dataTableOutput(outputId = tableId)
#' 
#' # server
#' #output$tableId <- DT::renderDataTable(create_datatable(X))
create_datatable <- function(.data, digits = 3, ...){
  
  .data %>%
    mutate(across(where(is.double), ~round(.x, digits = digits))) %>%
    DT::datatable(
      ...,
      rownames = FALSE,
      options = list(
        pageLength = 20, # sets n observations shown
        lengthChange = FALSE, #  removes option to change n observations shown
        sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
        scrollX = TRUE # enable side scroll so table doesn't overflow
      )
    )
}
