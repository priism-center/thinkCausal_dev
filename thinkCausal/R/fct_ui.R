
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


#' Button to be used within Analysis flow to link to Learning articles
#'
#' @param tabName The tab name of the Learning article to link to
#' @param tabNameCurrent The tab name of the point in the Analysis flow this function is called.
#' @param label Button label
#'
#' @return html
#' @author Joe Marlo
#' @noRd
#'
#' @examples
#' create_go_to_learning_btn('learn_1', 'analysis_1', 'Article 1')
create_go_to_learning_btn <- function(tabName, tabNameCurrent, label){
  htmltools::tags$button(
    type = 'button',
    class = "btn btn-primary",
    onclick = glue::glue("show_back_button(); log_page('{tabNameCurrent}'); go_to_shiny_page('{tabName}', false);"),
    glue::glue("Learn more at {label}")
  )
}
#' @describeIn create_go_to_learning_btn Hovering button that returns user back to the latest Analysis page
create_return_btn <- function(){
  htmltools::tags$button(
    id = 'back_to_analysis',
    type = 'button',
    class = "btn btn-primary nav-path",
    style = "position: fixed; bottom: 30px; right: 20px; z-index: 100; display: none; max-width: 200px;",
    onclick = "go_to_shiny_page(last_page, false); hide_back_button();",
    glue::glue("Back to Analysis")
  )
}
