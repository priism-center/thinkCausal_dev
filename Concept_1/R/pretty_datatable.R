pretty_datatable <- function(...){
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