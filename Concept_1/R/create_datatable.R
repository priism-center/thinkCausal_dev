#' Create a pretty datatable
#' 
#' Create an HTML datatable using the JavaScript library DataTables with predefined defaults
#'
#' @param ... arguments to be passed to DT::datatable. Typically a dataframe
#' @author Joe Marlo
#'
#' @return HTML datatable
#' @export 
#' 
#' @examples
#' X <- data.frame(x = 1:10, y = 1:10)
#' create_datatable(X)
create_datatable <- function(...){
  # wrapper around DT::datatable so commonly used arguments
  # can be set as global defaults
  
  DT::datatable(..., 
                rownames = FALSE, 
                options = list(
                  pageLength = 20, # sets n observations shown
                  lengthChange = FALSE, #  removes option to change n observations shown
                  sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
                  scrollX = TRUE # enable side scroll so table doesn't overflow
                )
  )
}
