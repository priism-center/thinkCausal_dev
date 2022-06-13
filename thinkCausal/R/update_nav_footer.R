
#' Update the nav footer
#' 
#' Display the footer when in the nav section and highlight the current page
#' 
#' @param store 
#' @param input 
#'
#' @return
#' @export
#' 
#' @examples
#' #observeEvent(input$nav, update_nav_footer(store, input))
update_nav_footer <- function(store, input){
  
  # display footer when in analysis section
  current_page <- input$nav
  is_analysis <- current_page %in% store$module_ids$analysis
  if (isTRUE(is_analysis)) {
    shinyjs::runjs('$(".progress-footer-tab").show()')
  } else {
    shinyjs::runjs('$(".progress-footer-tab").hide()')
  }
  
  # highlight current footer item
  footer_id <- glue::glue('progress-footer-{current_page}')
  shinyjs::runjs(
    glue::glue(
      '$(".progress-footer-tab").css("font-weight", ""); $("#{footer_id}").css("font-weight", 600)'
    )
  )
}
