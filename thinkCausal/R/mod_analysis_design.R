#' analysis_design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_design_ui <- function(id){
  ns <- NS(id)
  tagList(
    # shinyjs::useShinyjs(),
    fluidRow(
      column(width = 6,
             bs4Dash::box(
               id = ns('box1'),
               width = 12,
               collapsible = FALSE,
               title = '1. Describe the study design',
               selectInput(
                 inputId = ns('analysis_design'),
                 label = 'Indicate the study design:',
                 choices = c(
                   "",
                   "Unsure",
                   'Observational',
                   'Randomized treatment',
                   'Block randomized treatment'
                 )
               ),
               selectInput(
                 inputId = ns('analysis_weights'),
                 label = 'Include survey weights:',
                 choices = c("", "Unsure", "No", "Yes")
               ),
               selectInput(
                 ns('analysis_random_intercept'),
                 label = create_info_icon(
                   'Account for clustered or nested data',
                   'Classes within schools or patients within medical practices are examples of clustered/nested data'
                 ),
                 choices = c("", "Unsure", "No", "Yes")
               ),
               uiOutput(outputId = ns('save_design_btn')),
               actionButton(inputId = ns('analysis_design_help'),
                            label = 'Help me'))),
      column(width = 6,
             conditionalPanel(
               condition = "output.render_box2 > '0'",
               ns = ns,
               bs4Dash::box(
                 id = ns('box2'),
                 width = 12,
                 collapsible = FALSE,
                 title = '2. Describe the inference (Optional)',
                 h5('Example:'),
                 htmlOutput(ns("analysis_design_text")),
                 br(),
                 # selectInput(ns('analysis_design_estimand'),
                 #             label = 'Select causal estimand',
                 #             choices = c("",
                 #                         'Unsure',
                 #                         'ATE - Average treatment effect' = 'ATE',
                 #                         'ATC - Average treatment effect on the control' = 'ATC',
                 #                         'ATT - Average treatment effect on the treated' = 'ATT')),
                 textInput(ns('treatment_name'),
                           label = 'What is the name of the treatment?',
                           placeholder = 'treatment condition'),
                 textInput(ns('treatment_units'),
                           label = 'What are the units of your outcome variable?',
                           placeholder = 'units'),
                 textInput(ns('treatment_participants'),
                           label = 'Describe the participants in this study',
                           placeholder = 'participants'),
                 br(),
                 actionButton(inputId = ns("analysis_design_button_next"),
                              class = "nav-path",
                              label = "Skip & continue to Upload data"),
                 actionButton(inputId = ns('analysis_design_help'),
                              label = 'Help me')
               )
             ))
      # column(width = 4,
      # conditionalPanel(condition = "output.render_box3 > '0'",
      #                  ns = ns,
      #                  bs4Dash::box(
      #                    width = 12,
      #                    collapsible = FALSE,
      #                    title = '3. Save design and continue',
      #                    actionButton(inputId = ns("analysis_design_button_next"),
      #                                 class = "nav-path",
      #                                 label = "Save & Continue to Upload data"),
      #                    actionButton(inputId = ns('analysis_design_help'),
      #                                 label = 'Help me')
      #                  ))
      #        )

    )
  )
}

#' analysis_design Server Functions
#'
#' @noRd
mod_analysis_design_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_design_help, {
      open_help_sidebar(store, 'Study Design')
    })

    active_save_design <- reactive({
      entered <-  sum(
        input$analysis_design %notin% c(''),
        input$analysis_weights %notin% c(''),
        input$analysis_random_intercept %notin% c('')
      )
      if (counter() == 0) {
        if (entered == 3)
          eval <- TRUE
        else
          eval <-  FALSE
      } else{
        if (update_design() != 3)
          eval <- TRUE
        else
          eval <- FALSE
      }
      return(eval)
    })

    update_design <- reactive({
      sum(
        store$analysis_design_design == input$analysis_design,
        store$analysis_design_weights == input$analysis_weights,
        store$analysis_design_random_effects == input$analysis_random_intercept
      )
    })

    output$save_design_btn <- renderUI({
      .class <- if (isFALSE(active_save_design())) 'btn-disabled' else 'nav-path'
      .label <- if(counter() == 0) "Save design and continue" else 'Update design'
      actionButton(
        inputId = ns("analysis_design_button_save_design"),
        class = .class,
        label = .label
      )
    })

    # release conditions for box 2
    counter <- reactiveVal(0)
    observeEvent(input$analysis_design_button_save_design, {
      counter(counter() + 1)
    })

    output$render_box2 <- reactive({
      counter()
    })
    outputOptions(output, "render_box2", suspendWhenHidden = FALSE)


    listen <- reactive({
      list(input$treatment_name, input$treatment_participants, input$treatment_participants)
    })
    observeEvent(listen(), {
      if(sum(unlist(listen()) == "") < 3){
        updateActionButton(inputId = 'analysis_design_button_next',
                           label = 'Save & continue to Upload data')
      }else{
        updateActionButton(inputId = 'analysis_design_button_next',
                           label = 'Skip & continue to Upload data')
      }
    })



    # render example language
    output$analysis_design_text <- renderText({

      # TODO: somehow clean these inputs
      name <- input$treatment_name
      units <- input$treatment_units
      participants <- input$treatment_participants

      # set defaults
      if (name == '') name <- 'treatment condition'
      if (units == '') units <- 'units'
      if (participants == '') participants <- 'participants'

      # create the text
      text_out <- paste0(
        'The <b>',
        name,
        '</b> led to an ',
        '<i>increase/decrease</i>',
        ' of X <b>',
        units,
        '</b> for <b>',
        participants,
        '</b> in this study'
      )
      text_out <- HTML(text_out)

      return(text_out)
    })


    # save input and remove downstream dataframes if study design changes
    observeEvent(input$analysis_design_button_save_design, {

      # make sure required inputs have values
      local({
        req_inputs <- c(
          #'analysis_design_estimand',
          'analysis_design',
          'analysis_weights',
          'analysis_random_intercept'
        )
        req_values <- reactiveValuesToList(input)[req_inputs]

        # trigger animation if any inputs is unsure or blank
        inputs_to_animate <- req_inputs[which(req_values == 'Unsure' | req_values == '')]
        inputs_to_animate_selectors <- paste0("#", ns(inputs_to_animate), " + div", collapse = ', ')
        shinyjs::runjs(glue::glue('$("<<inputs_to_animate_selectors>>").effect("shake", {times: 4, distance: 3})',
                                  .open = "<<", .close = ">>"))

        # stop here if any unsures or blank inputs
        all_complete <- !isTRUE(length(inputs_to_animate) > 0)
        req(all_complete)
      })

      # save input to store
      store$analysis_design_design <- input$analysis_design
      store$analysis_design_weights <- input$analysis_weights
      store$analysis_design_random_effects <- input$analysis_random_intercept
      store$analysis_design_treatment_name <- input$treatment_name
      store$analysis_design_treatment_units <- input$treatment_units
      store$analysis_design_treatment_participants <- input$treatment_participants


      # add to log
      ## '\t', 'Causal estimand: ', input$analysis_design_estimand, '\n',
      log_event <- paste0(
        'Set study design: \n',
        '\t', 'Treatment name: ', input$treatment_name, '\n',
        '\t', 'Outcome units: ', input$treatment_units, '\n',
        '\t', 'Participants name: ', input$treatment_participants, '\n',
        '\t', 'Study design: ', input$analysis_design, '\n',
        '\t', 'Survey weights: ', input$analysis_weights, '\n',
        '\t', 'Clustered or nested data: ', input$analysis_random_intercept
      )
      store$log <- append(store$log, log_event)

      # remove saved dataframes if they exist
      # TODO: error here if user goes back and changes the estimand then saves the design
      store <- remove_downstream_data(store, page = 'design')
    })

    observeEvent(input$analysis_design_button_next, {
      # update page
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_upload')
    })

    # # open slide over if answer is unsure
    # dropdown_inputs <- c("analysis_design_estimand", "analysis_design", "analysis_weights", "analysis_random_intercept")
    # purrr::map(dropdown_inputs, function(input_id){
    #   observeEvent(input[[input_id]], {
    #     if (input[[input_id]] == "Unsure") Sys.sleep(0.2); open_help_sidebar(store, 'Study Design') #shinyjs::runjs('openHelpSection("help-studydesign")')
    #   })
    # })


    return(store)
  })
}
