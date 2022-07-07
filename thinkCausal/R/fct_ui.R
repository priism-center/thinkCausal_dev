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
