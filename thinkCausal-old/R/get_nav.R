
get_nav_current_page <- function(store){
  store$page_history[length(store$page_history)]
}

get_nav_previous_page <- function(store){
  store$page_history[length(store$page_history)-1]
}

get_nav_previous_analysis_page <- function(store){
  pages <- store$page_history[stringr::str_detect(store$page_history, "^analysis")]
  pages[length(pages)]
}
