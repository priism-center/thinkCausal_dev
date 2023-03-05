#' analysis_diagnostics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_diagnostics_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(3,
             bs4Dash::box(
               width = 12,
               collapsible = FALSE,
               title = 'Model diagnostics',
               br(),
               selectInput(inputId = "analysis_diagnostics_view",
                           label = 'Show diagnostics for:',
                           choices = c('General summary','Overlap diagnostics', 'Residual diagnostics')), #'MCMC convergence')),
               conditionalPanel("input.analysis_diagnostics_view == 'Overlap diagnostics'",
                                ns = ns,
                                selectInput(
                                  inputId = ns('overlap_rule'),
                                  label = 'Overlap rule:',
                                  choices = list('both' = 'both', 'standard deviation' = 'sd', 'chi-squared' = 'chi')
                                )
                                ),
               conditionalPanel("input.analysis_diagnostics_view == 'Overlap diagnostics'",
                                ns = ns,
                                selectInput(
                                  inputId = ns("overlap_type"),
                                  label = "Overlap diagnostic:",
                                  choices = c('Predict', 'Visualize')
                                )),
               conditionalPanel("input.analysis_diagnostics_view == 'Overlap diagnostics' & input.overlap_type == 'Predict'",
                                ns = ns,
                                sliderInput(
                                  inputId = ns('overlap_tree_depth'),
                                  label = 'Tree depth:',
                                  min = 1,
                                  max = 3,
                                  value = 2,
                                  step = 1
                                )
                                ),
               conditionalPanel("input.analysis_diagnostics_view == 'Residual diagnostics'",
                                ns = ns,
                                selectInput(
                                  inputId = ns('residual_type'),
                                  label = 'Residual diagnostic:',
                                  choices = c('Predicted vs Residual', 'Distribution of Residuals')
                                )
               ),
               conditionalPanel("input.analysis_diagnostics_view == 'Overlap diagnostics' & input.overlap_type == 'Visualize'",
                 ns = ns,
                 selectInput(
                   inputId = ns('overlap_x'),
                   label = 'X:',
                   choices = NULL,
                   selected = NULL
                 ),
                 selectInput(
                   inputId = ns('overlap_y'),
                   label = 'Y:',
                   choices = NULL,
                   selected = NULL
                 )
               ),
               conditionalPanel("input.analysis_diagnostics_view == 'Residual diagnostics' & input.residual_type == 'Predicted vs Residual'",
                                ns = ns,
                                selectInput(
                                  inputId = ns('residual_x'),
                                  label = 'X:',
                                  choices = NULL,
                                  selected = NULL
                                )
               ),
               # conditionalPanel("input.analysis_diagnostics_view == 'MCMC diagnostics'",
               #                  ns = ns,
               #                  selectInput(
               #                    inputId = ns('convergence_of'),
               #                    label = 'X:',
               #                    choices = NULL,
               #                    selected = NULL
               #                  )
               # ),
               actionButton(inputId = ns('analysis_diagnostics_help'),
                            label = 'What are these plots telling me?'),
               uiOutput(outputId = ns('analysis_diagnosis_buttons_ui'))
             )),
      column(9,
             conditionalPanel(
               "input.analysis_diagnostics_view == 'General summary'",
               ns = ns,
               fluidRow(
                 column(
                   6,
                   bs4Dash::box(
                     width = 12,
                     collapsible = FALSE,
                     title = 'Trace plot',
                     plotOutput(ns('analysis_diagnostics_plot_trace'),
                                height = 500),
                     downloadButton(ns('download_diagnostic_plot_trace'), label = "Download plot")
                   )
                 ),
                 column(
                   6,
                   bs4Dash::box(
                     width = 12,
                     collapsible = FALSE,
                     title = 'Overlap',
                     plotOutput(ns('analysis_diagnostics_plot_support'),
                                height = 500),
                     downloadButton(ns('download_diagnostic_plot_support'), label = "Download plot")
                   )
                 ),
                 column(
                   6,
                   bs4Dash::box(
                     width = 12,
                     collapsible = FALSE,
                     title = 'Residual vs fit',
                     plotOutput(ns('analysis_diagnostics_plot_residual'),
                                height = 500),
                     downloadButton(ns('download_diagnostic_plot_residual'), label = "Download plot")
                   )
                 ),
                 column(
                   6,
                   bs4Dash::box(
                     width = 12,
                     collapsible = FALSE,
                     title = 'Residual normality',
                     plotOutput(ns('analysis_diagnostics_plot_normal'),
                                height = 500),
                     downloadButton(ns('download_diagnostic_plot_normal'), label = "Download plot")
                   )
                 )
               )
             ),
             # if Overlap is selected
             conditionalPanel("input.analysis_diagnostics_view != 'General summary'",
                              ns = ns,
                              bs4Dash::box(
                                width = 12,
                                collapsed = FALSE,
                                title = 'Overlap diagnostics',
                                plotOutput(ns('analysis_diagnostic_overlap'))
                              )

                              )
      )
    )
  )
}

#' analysis_diagnostics Server Functions
#'
#' @noRd
mod_analysis_diagnostics_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_diagnostics_help, {
      open_help_sidebar(store, 'Diagnostics')
    })

    observeEvent(input$analysis_diagnostics_button_back , {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_model')
    })
    observeEvent(input$analysis_diagnostics_button_next , {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_results')
    })

    # render either both the back and next buttons or just the back if its a bad
    # model fit
    output$analysis_diagnosis_buttons_ui <- renderUI({

      validate_model_fit(store, req_only = TRUE)

      if (isTRUE(store$analysis$model$fit_good)){
        tagList(
          actionButton(inputId = ns("analysis_diagnostics_button_back"),
                       label = "Back to specify model"),
          actionButton(inputId = ns("analysis_diagnostics_button_next"),
                       class = "nav-path",
                       label = "Model results")
        )
      } else {
        tagList(
          actionButton(inputId = ns("analysis_diagnostics_button_back"),
                       class - 'nav-path',
                       label = "Back to specify model"),
          actionButton(inputId = ns("analysis_diagnostics_button_next"),
                       label = "Proceed to model results")
        )


      }
    })

    # update variables on the residual plot page once the model is fitted
    observe({
      # stop here if model isn't fit yet
      validate_model_fit(store)

      if(isTRUE(store$analysis$model$fit_good)){
        # new_col_names <- colnames(store$verified_df)
        # X_cols <- grep("^X_", new_col_names, value = TRUE)
        # cols_categorical <- store$column_types$categorical
        # cols_continuous <- store$column_types$continuous
        dat <- as.data.frame(store$analysis$model$model$data.rsp@x)
        col_names <- colnames(dat)[-ncol(dat)]

        updateSelectInput(
          session = session,
          inputId = "analysis_diagnostics_plot_residual_covariate",
          choices = c("None", col_names),
          selected = "None"
        )

        updateSelectInput(
          session = session,
          inputId = "analysis_diagnostics_plot_overlap_covariate",
          choices = c("Propensity Score", col_names),
          selected = "Propensity Score"
        )
      }
    })


    # trace plot
    analysis_diagnostics_plot_trace <- reactive({

      # stop here if model isn't fit yet
      validate_model_fit(store)

      # call function
      p <- plotBart::plot_trace(.model = store$analysis$model$model)

      # add theme
      p <- p + store$options$theme_custom

      return(p)
    })
    output$analysis_diagnostics_plot_trace <- renderPlot(analysis_diagnostics_plot_trace())

    # common support plot
    analysis_diagnostics_plot_support <- reactive({
      # stop here if model isn't fit yet
      validate_model_fit(store)

      bart_model <- store$analysis$model$model

        p <- plotBart::plot_common_support(
          .model = bart_model
        )

      # add theme
      p <- p +
        store$options$theme_custom +
        theme(legend.position = 'bottom',
              strip.text = element_text(hjust = 0))

      return(p)
    })
    output$analysis_diagnostics_plot_support <- renderPlot(analysis_diagnostics_plot_support())

    # residual plot
    analysis_diagnostics_plot_residual <- reactive({

      # stop here if model isn't fit yet
      validate_model_fit(store)

      bart_model <- store$analysis$model$model
      # p1 <- plot_residual_observed_predicted(.model = bart_model,
      #                                        covariate = covariates_selection)
      p3 <- plot_residual_predicted_residual(.model = bart_model,
                                             covariate = NULL)

      # patchwork package to combine the plots
      p <- p3

      # add theme
      p <- p + store$options$theme_custom

      return(p)
    })
    output$analysis_diagnostics_plot_residual <- renderPlot(analysis_diagnostics_plot_residual())

    analysis_diagnostics_plot_normal <- reactive({

      # stop here if model isn't fit yet
      validate_model_fit(store)

      bart_model <- store$analysis$model$model
      p <- plot_residual_density(.model = bart_model,
                                 covariate = covariates_selection)

      p <- p + store$options$theme_custom + theme(legend.position = 'top', legend.title = element_blank())

      return(p)
    })

    output$analysis_diagnostics_plot_normal <- renderPlot(analysis_diagnostics_plot_normal())


    # download plot
    output$download_diagnostic_plot_trace <- downloadHandler(
      filename = function() 'diagnostic_trace_plot.png',
      content = function(file){
        ggsave(file,
               plot = analysis_diagnostics_plot_trace(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    )
    output$download_diagnostic_plot_support <- downloadHandler(
      filename = function() 'diagnostic_common_support_plot.png',
      content = function(file){
        ggsave(file,
               plot = analysis_diagnostics_plot_support(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    )
    output$download_diagnostic_plot_residual <- downloadHandler(
      filename = function() 'diagnostic_residual_fit_plot.png',
      content = function(file){
        ggsave(file,
               plot = analysis_diagnostics_plot_residual(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    )
    output$download_diagnostic_plot_normal <- downloadHandler(
      filename = function() 'diagnostic_residual_normal_plot.png',
      content = function(file){
        ggsave(file,
               plot = analysis_diagnostics_plot_normal(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    )



# focus in on specific diagnostics -----------------------------------------
    observeEvent(input$overlap_rule,{
      req(store$analysis$model$model)
      # column names for overlap and residual
      X <- c(grep("^X_", names(store$verified_df), value = TRUE), 'Propensity Score')
      y <- grep("^Y_", names(store$verified_df), value = TRUE)
      overlapX <- c(X, y)


      # get default (.x) based on overlap plots
      .x <- plotBart:::predicted_common_support(store$analysis$model$model)
      .x$var <- ifelse(is.nan(.x$complexity), 'Propensity Score', .x$var)

      if(input$overlap_rule == 'sd') .x <- .x[1,1]
      if(input$overlap_rule == 'chi') .x <- .x[2,1]
      if(input$overlap_rule == 'both') {
        if (length(unique(.x[['var']])) == 1) {
          .x <-  .x[1, 1]
        } else{
          .x[is.nan(.x$complexity), 'complexity'] <-  0
          .x <- .x[.x$complexity == max(.x$complexity), 1]
        }
      }


      updateSelectInput(
        inputId = 'overlap_x',
        choices = overlapX,
        selected = .x
      )

      updateSelectInput(
        inputId = 'overlap_y',
        choices = overlapX,
        selected = y
      )

    })

    overlap_diagnostics_plot <- reactive({
      bart_model <- store$analysis$model$model
      x <- input$overlap_x
      y <- ifelse(input$overlap_y == grep("^Y_", names(store$verified_df), value = TRUE), 'y', input$overlap_y)

      if(input$overlap_type == 'Predict'){
        p <- plotBart::plot_predicted_common_support(.model = bart_model,
                                                     max_depth = input$overlap_tree_depth,
                                                     rule = input$overlap_rule)
      }else{
        p <- plotBart::plot_common_support(.model = bart_model,
                                           rule = input$overlap_rule,
                                           .x = x,
                                           .y = y
                                           )
      }

      p <- p + store$options$theme_custom

      return(p)
    })

    observeEvent(store$analysis$model$fit_good,{
      X <- c(grep("^X_", names(store$verified_df), value = TRUE), 'Propensity Score')
      residualX <- c('Predicted', X)
      updateSelectInput(inputId = 'residual_x',
                        choices = residualX,
                        selected = 'Predicted'
                        )
    })

    residual_diagnostics_plot <- reactive({
      bart_model <- store$analysis$model$model
      x <- input$residual_x
      x <- ifelse(x == 'Propensity Score', 'ps', x)

      if(input$residual_type == 'Predicted vs Residual' & x == 'Predicted'){
        p <- plot_residual_predicted_residual(bart_model)
      } else if(input$residual_type == 'Predicted vs Residual' & x != 'Predicted'){
        p <- plot_residual_predicted_residual(bart_model, x)
      }else{
        p <- plotBart::plot_residual_density(bart_model)
      }

      p <- p + store$options$theme_custom
      return(p)
    })

    output$analysis_diagnostic_overlap <- renderPlot({

      p <- switch(input$analysis_diagnostics_view,
       'Overlap diagnostics'= overlap_diagnostics_plot(),
       'Residual diagnostics' = residual_diagnostics_plot())

      return(p)
      })

    # output$download_diagnostic_plot <- downloadHandler(
    #   filename = function() {
    #     switch(
    #       req(input$analysis_diagnostics_tabs),
    #       "Trace plot" = 'diagnostic_trace_plot.png',
    #       "Common support" = 'diagnostic_common_support_plot.png',
    #       "Residual vs fit" = 'diagnostic_residual_fit_plot.png',
    #       "Residual normality" = 'diagnostic_residual_normal_plot.png'
    #     )
    #   },
    #   content = function(file) {
    #
    #     # get the plot that is on the active tab
    #     active_plot <- switch(
    #       req(input$analysis_diagnostics_tabs),
    #       'Trace plot' = analysis_diagnostics_plot_trace(),
    #       'Common support' = analysis_diagnostics_plot_support(),
    #       "Residual vs fit" = analysis_diagnostics_plot_residual(),
    #       'Residual normality' = analysis_diagnostics_plot_normal()
    #     )
    #
    #     # write out plot
    #     ggsave(file,
    #            plot = active_plot,
    #            height = store$options$settings_options_ggplotHeight,
    #            width = store$options$settings_options_ggplotWidth,
    #            units = 'in',
    #            device = 'png')
    #   }
    # )

    return(store)
  })
}
