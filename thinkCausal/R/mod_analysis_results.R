#' analysis_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_analysis_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        bs4Dash::box(
          width = 12,
          collapsible = FALSE,
          title = 'Results',
          selectInput(inputId = ns('interpretation'),
                      label = "Results interpretation:",
                      choices = c('Causal', 'Non-Causal'),
                      selected = 'Causal'),
          selectInput(inputId = ns("plot_result_style"),
                      label = "Plot:",
                      choices = c('Density', 'Histogram'),
                      selected = 'Density'),
          checkboxGroupInput(inputId = ns('central_tendency'),
                            label = NULL,
                            inline = T,
                            choices = c('Mean', 'Median'),
                            selected = 0),
          checkboxGroupInput(inputId = ns('show_interval'),
                            label = NULL,
                            inline = T,
                            choices = list('80% ci' = .8, '95% ci' = .95),
                            selected = .95),
          radioButtons(inputId = ns('show_reference'),
                       label = 'Include reference line:',
                       choices = c('Yes', 'No'),
                       inline = T,
                       selected = 'No'),
          conditionalPanel(condition = "input.show_reference == 'Yes'", ns = ns,
                          numericInput(inputId = ns("reference_bar"),
                                       label = "Reference number",
                                       value = 0,
                                       step = 1)),
          conditionalPanel(condition = "output.overlap_flag", ns = ns,
                           checkboxGroupInput(inputId = ns('analysis_results_view_overlap'),
                                              label = 'View Overlap Rule:',
                                              choices = list('none' = 'none', 'standard deviation' = 'sd', 'chi-squared' = 'chisq'),
                                              selected = c('none', 'sd', 'chisq'))),

          br(),
          actionButton(inputId = ns('analysis_results_help'),
                       label = 'What is this plot telling me?'),
          downloadButton(ns('download_SATE_plot'), label = "Download plot"),
          actionButton(inputId = ns("analysis_results_button_back"),
                       label = "See diagnostics"),
          actionButton(inputId = ns("analysis_results_button_next"),
                       class = "nav-path",
                       label = "See results by subgroups")
         )
      ),
      column(
        width = 9,
        fluidRow(
          bs4Dash::box(
            width = 12,
            collapsible = FALSE,
            title = 'Model results',
            plotOutput(outputId = ns('analysis_results_plot_SATE'),
                       height = 400),
            reactable::reactableOutput(ns('analysis_results_table_summary'))
          ),
          bs4Dash::box(
            width = 12,
            collapsible = FALSE,
            title = 'Interpretation',
            textOutput(outputId = ns('results_text'))
        )
      )
      )
    )
  )
}

#' analysis_results Server Functions
#'
#' @noRd
mod_analysis_results_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_results_help, {
      open_help_sidebar(store, 'Results')
    })

    # go to subgroup page
    observeEvent(input$analysis_results_button_next, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_subgroup')
    })

    # go to diagnostics page
    observeEvent(input$analysis_results_button_back, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_diagnostics')
    })

    # create overlap checkbox if any of the two overlap rules are activated
    output$overlap_flag <- reactive({
      ifelse(store$analysis$model$overlap_checks$sum_chisq_removed > 0 | store$analysis$model$overlap_checks$sum_sd_removed > 0, TRUE, FALSE)
    })

    outputOptions(output, "overlap_flag", suspendWhenHidden = FALSE)


    # render the summary table
    output$analysis_results_table_summary <- reactable::renderReactable({

      # stop here if model isn't fit yet
      validate_model_fit(store, req_only = TRUE)

      # extract estimates and format
      sate <- bartCause::extract(store$analysis$model$model, 'sate')
      model_sum <- data.frame(estimate = mean(sate), sd = sd(sate), ci.lower = quantile(sate, .025), ci.upper = quantile(sate, .975))
      rownames(model_sum) <- store$analysis$model$model$estimand
      tab <- model_sum %>%
        rename(!!glue::glue('95% CI - lower bound') := ci.lower,
                !!glue::glue('95% CI - upper bound') := ci.upper) %>%
        rename_all(tools::toTitleCase) %>%
        mutate(across(where(is.numeric), round, 3)) %>%
        reactable::reactable()

      return(tab)
    })

    # TODO: render the interpretation text
    output$results_text <- renderText({
      # stop here if model isn't fit yet
      validate_model_fit(store)

      text_out <- create_interpretation(.model = store$analysis$model$model,
                                        type = input$interpretation,
                                        treatment = 'treatment', #store$analysis_design_treatment_name,
                                        units = 'units', #store$analysis_design_treatment_units,
                                        participants = 'participants' #store$analysis_design_treatment_participants
                                        )

      return(text_out)
    })

    # SATE plot
    analysis_results_plot_SATE <- reactive({
      # stop here if model isn't fit yet
      validate_model_fit(store)

      # Do we need to check overlap
      .check <- ifelse(
        store$analysis$model$overlap_checks$sum_chisq_removed > 0 |
          store$analysis$model$overlap_checks$sum_sd_removed > 0,
        TRUE,
        FALSE
      )
      # make sure a box is checked if applicable
      if(isTRUE(.check)){
        validate(need(input$analysis_results_view_overlap != '', 'Either the standard deviation rule or chi-squred rule detectd lack of overlap. Select an overlap rule to view results. Learning Module on Overlap rules will be avalale soon.'))
      }

      # get value for reference bar
      reference_bar <- NULL
      if (input$show_reference == 'Yes') reference_bar <- req(input$reference_bar)

      # create plot
      p <- plotBart::plot_SATE(
        .model = store$analysis$model$model,
        type = input$plot_result_style,
        ci_80 = sum(input$show_interval == 0.80) > 0,
        ci_95 = sum(input$show_interval == 0.95) > 0,
        .mean = sum(input$central_tendency == 'Mean') > 0,
        .median = sum(input$central_tendency == 'Median') > 0,
        reference = reference_bar,
        check_overlap = .check,
        overlap_rule = input$analysis_results_view_overlap
      )

      # add theme
      p <- p +
        store$options$theme_custom +
        theme(legend.position = c(0.1, 0.9),
              legend.title = element_blank())

      return(p)
    })


    output$analysis_results_plot_SATE <- renderPlot(analysis_results_plot_SATE())

    output$download_SATE_plot <- downloadHandler(
      filename = "SATE_plot.png",
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = analysis_results_plot_SATE(),
          height = store$options$settings_options_ggplotHeight,
          width = store$options$settings_options_ggplotWidth,
          units = 'in',
          device = 'png'
        )
      }
    )

    return(store)
  })
}

## To be copied in the UI
# mod_analysis_results_ui("analysis_results_1")

## To be copied in the server
# mod_analysis_results_server("analysis_results_1")
