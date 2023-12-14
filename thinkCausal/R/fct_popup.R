
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

show_popup_variable_selection_warning <- function(x, session, ns){
  content <- tags$div(
    style = 'margin: auto; text-align: left',
    h5(glue::glue('Currently, there are {x} avalable covaraites not included in the analysis.')),
    h5('For observational studies in thinkCausal, include all pre-treatment variables in the analysis.'),
    h5('The only reasons you should exclude a variable from the analysis are:'),
    h5('  1. The variable is a post-treatment variable (the variable could be effected by the treatment)'),
    h5('  2. The variable is an ID variable.'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = ns('analysis_model_variable_selection_popup_stop'),
                   class = 'nav-path',
                   label = 'Stay on page and update variable selection'),
      actionButton(inputId = ns('analysis_model_variable_selection_popup_continue'),
                   class = 'nav-btn-focus',
                   label = glue::glue('Continue: All {x} variables are either post-treatment variables or ID variables')),
      actionButton(inputId = ns('analysis_model_variable_selection_popup_posttreatment'),
                   class = 'nav-btn-focus',
                   label = 'Learn more about post-treatment variables')
    )
  )
  show_popup(session = session, content, size = 'l')

}

show_popup_model_no_estimand_warning <- function(session, ns){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('Before fitting a model, you need to select a causal estimand.'),
    br(),
    div(
      class = 'backNextContainer',
      style = "width:60%;display:inline-block;horizontal-align:center;",
      actionButton(inputId = ns('analysis_model_estimand_button_popup'),
                   class = 'nav-btn-focus',
                   label = 'Stay on page and choose an estimand')
    )
  )
  show_popup(session = session, content, easyClose = TRUE)
}

show_popup_model_no_data_warning <- function(session, ns){
  content <- tags$div(
    style = 'margin: auto; text-align: center',
    h3('Before fitting a model you need to upload Data. Right now you do not have any Data loaded into thinkCausal.'),
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
    h6("...sometimes this takes a while..."),
    h6("In the meantime, read up on ",
       a(
         href = 'https://www.tandfonline.com/doi/abs/10.1198/jcgs.2010.08162',
         target = "_blank",
         'BART'
       ))
    # verbatimTextOutput('console_output')
  )
  show_popup(session = session, content)
}

show_popup_mobile <- function(session){
  content <- HTML("<strong>thinkCausal</strong> is best experienced on a big screen! Please come back on a desktop browser.")
  show_popup(
    session = session,
    easyClose = FALSE,
    close_button = shiny::modalButton("I'll check it out anyway"),
    content
  )
}

# show_popup_welcome <- function(session){
#   content <- tags$div(
#     h2(style = 'margin: auto; text-align: center',
#        "Welcome!"),
#     br(),
#     div(
#       style = 'font-size: 1.2em',
#       HTML(
#         "Here are some tips to get started:
#          <ul>
#           <li>We do not save your data so if you leave and come back, you'll have to start from the beginning</li>
#           <li>The full analysis process is outlined at the bottom of the page</li>
#           <li>thinkCausal is still in beta and we'll be making updates over the coming months</li>
#           <li>Please feel free to reach out on the <a href='https://github.com/gperrett/thinkCausal_dev' target='_blank'>GitHub page</a> or email gp77@nyu.edu if you have any questions</li>
#         </ul>
#       ")
#     )
#   )
#   show_popup(session = session,
#              content,
#              size = 'm',
#              close_button = shiny::actionButton(
#                inputId = 'analysis_design-analysis_design_button_closeModal',
#                class = 'nav-btn-focus',
#                `data-dismiss` = "modal",
#                `data-bs-dismiss` = "modal",
#                label = "Get started"
#              ), #shiny::modalButton("Get started"),
#              easyClose = TRUE)
# }

# show_popup_crash <- function(){
#   content <- tags$div(
#     div(
#       style = 'margin: auto; text-align: center;',
#       img(src = file.path('img', 'error.png'),
#           width = "40%")
#     ),
#     br(),br(),
#     h2(style = 'margin: auto; text-align: center',
#        "Error!"),
#     br(),
#     div(style = 'margin: auto; text-align: center; font-size: 1.4em',
#         HTML(
#           "If this occurs again, please raise an issue on the <a href='https://github.com/gperrett/thinkCausal_dev/issues' target='_blank'>GitHub page</a> if you have any questions</li>"
#         ))
#   )
#
#   show_popup(session = getDefaultReactiveDomain(),
#              content,
#              size = 'm',
#              style = "z-index: 99999")
# }


close_popup <- function(session){
  shiny::removeModal(session = session)
}

