
#' Open the sidebar help menu to a specific section
#'
#' For use on the server-side
#'
#' @param store store object
#' @param section the title of the h3 section within the help markdown
#'
#' @return NULL; called for the JavaScript side effects
#' @noRd
open_help_sidebar <- function(store, section){

  # toggle side bar help menu
  bs4Dash::updateControlbar(id = "help-slideover", session = store$session_global)

  # scroll to section
  selector <- tolower(stringr::str_remove_all(section, ' '))
  selector <- glue::glue('help-{selector}')
  shinyjs::runjs(
    paste0(
      "
      let y = $('#", selector, "')[0].offsetTop
      setTimeout(function(){$('#help-slideover .os-viewport')[0].scrollTo({top: y, behavior: 'smooth'}); }, 600);
      "
    )
  )
  return(NULL)
}
