
show_popup <- function(session, ...){
  popup <- shiny::modalDialog(
    ...,
    title = NULL,
    footer = NULL,
    size = 's',
    easyClose = FALSE,
    fade = TRUE
  )
  shiny::showModal(ui = popup, session = session)
}

show_popup_waiting <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    img(src = file.path('img', 'tree.gif'),
        width = "50%"),
    h5("...sometimes this takes a while..."),
  )
  show_popup(session = session, content)
}

# create additional popup templates using this format
# show_popup_*

close_popup <- function(session){
  shiny::removeModal(session = session)
}
