
#' Open the sidebar help menu to a specific section
#'
#' For use on the server-side
#'
#' @param store store object
#' @param section the title of the h3 section within the help markdown
#'
#' @return NULL; called for the JavaScript side effects
#' @author Joe Marlo
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
      setTimeout(function() {
        let section = $('#", selector, "')[0];
        let y = section.offsetTop;
        $('#help-slideover .os-viewport')[0].scrollTo({top: y, behavior: 'smooth'});
      }, 600);
      "
    )
  )
  return(NULL)
}

#' Add a red ribbon in the corner denoting beta status
#'
#' Must include corner-ribbon.css file in the /www folder
#'
#' @return html
#' @author Joe Marlo
#' @noRd
#'
#' @examples
#' # within app_UI
#' add_betta_ribbon()
add_beta_ribbon <- function(){

  htmltools::tags$div(
    class = 'cornerRibbon',
    'BETA',
    htmltools::tags$div(
      htmltools::tags$a(
        href = 'https://docs.google.com/forms/d/e/1FAIpQLSd7dZjpw4FtoVAFUmovNOgKeW-kxnJrs3zV2r3lJ8kvhdq8lA/viewform?usp=sf_link',
        target = "_blank",
        'Have feedback?'
      )
    )
  )
}
