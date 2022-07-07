#' Create a new card on the learning landing page
#'
#' @param page_id the page id of the article
#' @param thumbnail_url the url of the thumbnail image
#' @param title title to display
#' @param description description to display under the title
#' @param width
#'
#' @return html
#' @export
#'
#' @noRd
#'
#' @examples
#' create_learning_card(
#'   page_id = 'concepts_link_causal_estimands',
#'   thumbnail_url = 'estimands.png',
#'   title = "Causal estimands",
#'   description = "BART allows ..."
#' )
create_learning_card <- function(page_id, thumbnail_url, title, description, width = 4){
  shiny::column(
    width = width,
    shiny::wellPanel(
      class = 'learning-card',
      shiny::actionLink(
        page_id,
        shiny::img(src = glue::glue("thumbnails/{thumbnail_url}"))
      ),
      shiny::br(),
      shiny::h3(title),
      shiny::p(description)
    )
  )
}

#' Create hover-able information icon
#'
#' Returns HTML code to create a little (i) icon that shows text when you hover.
#'
#' @param label any text to display next to the icon
#' @param text text to display when hovering over the icon
#'
#' @author Joe Marlo
#'
#' @return html
#' @export
#'
#' @noRd
#'
#' @examples
#' # within shiny UI pages
#' create_info_icon('Average Treatment Effect', 'The ATE is ...')
create_info_icon <- function(label, text){
  html <- paste0(
    '<div class="infoToolTip">',
    label,
    ' <a>&#9432;</a>',
    '<span class="infoToolTipText">',
    text,
    '</span></div>'
  )

  html <- HTML(html)
  return(html)
}
