
show_popup <- function(session, ..., size = 's', close_button = NULL){
  popup <- shiny::modalDialog(
    ...,
    title = NULL,
    footer = close_button,
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

show_popup_variable_assignment_warning <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3("Whoops, there's an issue with variable assignment"),
    h5("Make sure you assign one column to treatment and one to response. Treatment must also be binary.")
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
                   label = 'Yes'),
      br(), br(),
      actionButton(inputId = 'learn_common_support_no',
                   label = 'No')
    )
  )
  show_popup(session = session, content)
}

show_popup_model_no_data_warning <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('Data must be first uploaded and columns selected'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = 'analysis_model_button_popup',
                   label = 'Take me to the Data tab')
    )
  )
  show_popup(session = session, content)
}

show_popup_fitting_BART_waiting <- function(session){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    img(src = file.path('img', 'tree.gif'),
        width = "50%"),
    h3('Fitting BART model...'),
    h5("...sometimes this takes a while...")
  )
  show_popup(session = session, content)
}


close_popup <- function(session){
  shiny::removeModal(session = session)
}
