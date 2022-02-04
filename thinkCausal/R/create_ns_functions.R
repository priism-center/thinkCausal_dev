

#' Closure to create namespace functions
#'
#' This function creates other functions which help with namespace ids. This is similar to shiny::NS but it can be applied programmatically to create many functions at once.
#'
#' @param module_id the id of the namespace
#'
#' @return a function similar to shiny::ns
#' @export
#'
#' @examples
#' make_namespace_function("my_module_id")
#' ns_my_module_id("input_id")
make_namespace_function <- function(module_id){
  
  # define the function
  args <- alist('id' = NULL)
  fn_body <- quote(shiny::NS(module_id)(id))
  fn <- as.function(c(args, fn_body), envir = parent.frame())
  
  # move the function to the global environment
  fn_name <- paste0("ns_", module_id)
  assign(fn_name, fn, envir = .GlobalEnv)
}
