#' Clean a string for use in column names
#' 
#' Cleans up strings for use as column names in dataframes. Makes the following changes: 
#' \itemize{
#'  \item replaces non-ASCII characters with ASCII counterparts  
#'  \item replaces spaces with underscores  
#'  \item replaces percent sign with '_percent'  
#'  \item removes all punctuation except underscores and periods  
#'  \item adds 'n' to the beginning of strings that start with numeric characters  
#' } 
#'
#' @param .names 
#' 
#' @author Joe Marlo
#'
#' @return character vector
#' @export
#'
#' @importFrom stringr str_replace_all
#' @examples
#' .names <- c("yes", "TRUE", "nope%", "98", 'Ábcdêãçoàúü', 'yep_-,.yep', 'hello goodbye')
#' clean_names(.names)
clean_names <- function(.names){
  
  .names <- as.character(.names)
  if (!isTRUE(is.character(.names))) stop(".names must be a character vector")
  
  # remove non-ASCII characters
  .names <- base::iconv(.names, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # replace spaces with underscore
  .names <- stringr::str_replace_all(string = .names, pattern = ' ', replacement = '_')
  
  # replace % with 'percent' depending on its location
  .names <- stringr::str_replace_all(string = .names, pattern = '%$', replacement = "_percent")
  .names <- stringr::str_replace_all(string = .names, pattern = '^%', replacement = "percent_")
  .names <- stringr::str_replace_all(string = .names, pattern = '%', replacement = "_percent_")
  
  # remove punctuation except underscore and period
  pat <- "(?![._])[[:punct:]]"
  .names <- stringr::str_replace_all(string = .names, pattern = pat, replacement = "")
  
  # add 'n' before numbers (names can't start with numbers)
  .names <- as.character(sapply(.names, function(string){
    is_first_num <- base::substr(string, 1, 1) %in% 0:9
    if (isTRUE(is_first_num)) string <- paste0('n', string)
    return(string)
  }))
  
  return(.names)
}
