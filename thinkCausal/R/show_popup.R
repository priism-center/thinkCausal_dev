
show_popup <- function(session, ..., size = 's'){
  popup <- shiny::modalDialog(
    ...,
    title = NULL,
    footer = NULL,
    size = size,
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

show_popup_common_support_warning <- function(session, common_support_check){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('Common Support Warning'),
    h5(HTML(common_support_check$message)),
    br(),
    h5('How would you like to proceed?'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = 'common_support_opt3',
                   label = 'See common support diagnostics'),
      br(), br(),
      actionButton(inputId = 'common_support_new_rule',
                   label = 'Change common support rule'),
      br(), br(),
      actionButton(inputId = 'common_support_opt2',
                   label = 'Learn more about common support rules'),
      br(), br(),
      actionButton(inputId = 'common_support_continue',
                   label = 'Continue to results')
    ),
    br(), br()
  )
  show_popup(session = session, content, size = 'l')
}

# create additional popup templates using this format
# show_popup_*

close_popup <- function(session){
  shiny::removeModal(session = session)
}

show_message <- function(content){
  # this is a smaller message that doesn't take over the whole screen
  # style and position controlled in CSS with class .shiny-notification
  shiny::showNotification(
    ui = content,
    duration = 15,
    closeButton = TRUE,
    id = NULL,
    type = 'default')
}
