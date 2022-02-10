
#' Build the scaffolding for a new learning module
#'
#' Creates the sub
#'
#' @param article_name 
#'
#' @return
#' @export
#'
#' @examples
create_learning_module <- function(article_name = 'Lorem ipsum', directory_name){
  
  if (missing(directory_name)){
    directory_name <- paste0(tolower(gsub('-| ', '_', article_name)), 
                             sample(1000:9999, 1))
  }
  
  path_to_modules <- file.path('modules', 'analysis')
  
  # variables
  
  # quiz
  
  # ui
  
  # server
  
  # message reminders about where to put ids, how to update ui and server
  message(paste0("Learning module '", article_name, "' created in ", path_to_modules, "/", directory_name))
  # message('Remember to: \n', )
}
