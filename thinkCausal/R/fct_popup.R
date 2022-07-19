
### these are the shiny modal popups, just standardized and removed from server.R ###

show_popup <- function(session, ..., size = 's', close_button = NULL, easyClose = FALSE){
  popup <- shiny::modalDialog(
    ...,
    title = NULL,
    footer = close_button,
    size = size,
    easyClose = easyClose,
    fade = TRUE
  )
  shiny::showModal(ui = popup, session = session)
}

show_popup_waiting <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    img(src = file.path('www', 'img', 'tree.gif'),
        width = "50%"),
    h5("...sometimes this takes a while..."),
  )
  show_popup(session = session, content)
}

show_popup_common_support_warning <- function(session, common_support_check, ns){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('Lack of Overlap Warning'),
    h5(HTML(common_support_check$message)),
    br(),
    h5('How would you like to proceed?'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = ns('common_support_opt3'),
                   class = 'nav-btn-focus',
                   label = 'See common support diagnostics'),
      br(), br(),
      actionButton(inputId = ns('common_support_new_rule'),
                   label = 'Change common support rule'),
      br(), br(),
      actionButton(inputId = ns('common_support_opt2'),
                   label = 'Learn more about common support rules'),
      br(), br(),
      actionButton(inputId = ns('common_support_continue'),
                   label = 'Continue to results')
    ),
    br(), br()
  )
  show_popup(session = session, content, size = 'l')
}

# create additional popup templates using this format
# show_popup_*

show_popup_variable_assignment_warning <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3("Whoops, there's an issue with variable assignment"),
    h5("Make sure you assign one column to treatment and one to response. Treatment must also be binary. Any blocking variables must be binary or categorical.")
  )
  show_popup(session = session, content, close_button = shiny::modalButton("Close"))
}

show_popup_group_name_warning <- function(session, group_name_check){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3("Group names cannot be empty. Please rename the following group(s):"),
    h5(toString(group_name_check))
  )
  show_popup(session = session, content, close_button = shiny::modalButton("Close"))
}

show_popup_learn_estimand <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('I would like to learn more about causal estimands:'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = 'learn_estimand_yes',
                   class = 'nav-btn-focus',
                   label = 'Yes'),
      br(), br(),
      actionButton(inputId = 'learn_estimand_no',
                   label = 'No')
    )
  )
  show_popup(session = session, content)
}

show_popup_learn_common_support <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('I would like to learn more about common support:'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = 'learn_common_support_yes',
                   class = 'nav-btn-focus',
                   label = 'Yes'),
      br(), br(),
      actionButton(inputId = 'learn_common_support_no',
                   label = 'No')
    )
  )
  show_popup(session = session, content)
}

show_popup_model_no_data_warning <- function(session, ns){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('Data must be first uploaded and columns selected'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = ns('analysis_model_button_popup'),
                   class = 'nav-btn-focus',
                   label = 'Take me to the Data page')
    )
  )
  show_popup(session = session, content, easyClose = TRUE)
}

show_popup_fitting_BART_waiting <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    img(src = file.path('www', 'img', 'tree.gif'),
        width = "50%"),
    h3('Fitting BART model...'),
    h5("...sometimes this takes a while..."),
    # verbatimTextOutput('console_output')
  )
  show_popup(session = session, content)
}

show_popup_welcome <- function(session){
  content <- tags$div(
    h2(style = 'margin: auto; text-align: center',
       "Welcome!"),
    br(),
    div(
      style = 'font-size: 1.2em',
      HTML(
        "Here are some tips to get started:
         <ul>
          <li>We do not save your data so if you leave and come back, you'll have to start from the beginning</li>
          <li>The full analysis process is outlined at the bottom of the page</li>
          <li>thinkCausal is still in beta and we'll be making updates over the coming months</li>
          <li>Please feel free to reach out on the <a href='https://github.com/gperrett/thinkCausal_dev' target='_blank'>GitHub page</a> or email gp77@nyu.edu if you have any questions</li>
        </ul>
      ")
    )
  )
  show_popup(session = session,
             content,
             size = 'm',
             close_button = shiny::actionButton(
               inputId = 'analysis_design-analysis_design_button_closeModal',
               class = 'nav-btn-focus',
               `data-dismiss` = "modal",
               `data-bs-dismiss` = "modal",
               label = "Get started"
             ), #shiny::modalButton("Get started"),
             easyClose = TRUE)
}

show_popup_crash <- function(){
  content <- tags$div(
    div(
      style = 'margin: auto; text-align: center;',
      img(src = file.path('img', 'error.png'),
          width = "40%")
    ),
    br(),br(),
    h2(style = 'margin: auto; text-align: center',
       "Error!"),
    br(),
    div(style = 'margin: auto; text-align: center; font-size: 1.4em',
        HTML(
          "If this occurs again, please raise an issue on the <a href='https://github.com/gperrett/thinkCausal_dev/issues' target='_blank'>GitHub page</a> if you have any questions</li>"
        ))
  )

  show_popup(session = getDefaultReactiveDomain(),
             content,
             size = 'm',
             style = "z-index: 99999")
}


close_popup <- function(session){
  shiny::removeModal(session = session)
}



# updating overlay --------------------------------------------------------

#' Add an blurry overlay and message to user
#'
#' Blur out an element and add a loading message. This makes it obvious to the user that an element is updating.
#'
#' @param div div ID of the parent
#'
#' @export
#' @noRd
#'
#' @examples
#' \dontrun{
#' # shiny server
#' output$analysis_plot_overlap_plot <- renderPlot({
#'
#'   # add overlay
#'   show_message_updating('analysis_plot_overlap_plot')
#'
#'   # build plot
#'   p <- overlap_plot()
#'
#'   # remove overlay
#'   close_message_updating('analysis_plot_overlap_plot')
#'
#'   return(p)
#' })
#' }
show_message_updating <- function(div){
  # this adds a gray, blurry overlay and a message to the user notifying it is updating

  # add overlay and show message
  js_disable_div(div)
  show_message('Updating...', id = paste0(div, '-message'), closeButton = FALSE)
}

close_message_updating <- function(div){
  # remove overlay and close message
  Sys.sleep(0.5)
  js_enable_div(div)
  close_message(paste0(div, '-message'))
}

show_message <- function(content, id = NULL, closeButton = TRUE){
  # this is a smaller message that doesn't take over the whole screen
  # style and position controlled in CSS with class .shiny-notification
  shiny::showNotification(
    ui = content,
    duration = 60*5,
    closeButton = closeButton,
    id = id,
    type = 'default')
}

close_message <- function(id){
  shiny::removeNotification(id = id)
}

js_disable_div <- function(div){
  # turn off pointer events
  pointer <- paste0('document.getElementById("', div, '").style.pointerEvents = "none";')

  # add transparent gray overlay div
  transparency <- paste0("$('#", div, "').wrap(\"<div class='dimmed'></div>\");")

  # run js code
  js_code <- paste0(pointer, transparency, collapse = '')
  shinyjs::runjs(js_code)
}

js_enable_div <- function(div){
  # turn on pointer events
  pointer <- paste0('document.getElementById("', div, '").style.pointerEvents = "auto";')

  # remove transparent gray overlay div
  transparency <- paste0("$('#", div, "').unwrap()")

  # run js code
  js_code <- paste0(pointer, transparency, collapse = '')
  shinyjs::runjs(js_code)
}

