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
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
               br(),
               conditionalPanel("input.analysis_diagnostics_tabs == 'Overlap'", ns = ns,
                                selectInput(
                                  inputId = ns("analysis_diagnostics_plot_overlap_covariate"),
                                  label = "By covariate:",
                                  choices = NULL,
                                  selected = NULL
                                )),
               conditionalPanel("input.analysis_diagnostics_tabs == 'Residual vs fit'", ns = ns,
                                selectInput(
                                  inputId = ns("analysis_diagnostics_plot_residual_covariate"),
                                  label = "By covariate: ",
                                  multiple = FALSE,
                                  choices = NULL,
                                  selected = NULL
                                )),
               actionButton(inputId = ns('analysis_diagnostics_help'),
                            label = 'What are these plots telling me?'),
               uiOutput(outputId = ns('analysis_diagnosis_buttons_ui'))
             )),
      column(9,
             fluidRow(
               column(6,
                      bs4Dash::box(
                        width = 12,
                        collapsible = FALSE,
                        title = 'Trace plot',
                        plotOutput(ns('analysis_diagnostics_plot_trace'),
                                   height = 500),
                        downloadButton(ns('download_diagnostic_plot_trace'), label = "Download plot")
                      )),
               column(6,
                      bs4Dash::box(
                        width = 12,
                        collapsible = FALSE,
                        title = 'Overlap',
                        plotOutput(ns('analysis_diagnostics_plot_support'),
                                   height = 500),
                        downloadButton(ns('download_diagnostic_plot_support'), label = "Download plot")
                      )),
               column(6,
                      bs4Dash::box(
                        width = 12,
                        collapsible = FALSE,
                        title = 'Residual vs fit',
                        plotOutput(ns('analysis_diagnostics_plot_residual'),
                                   height = 500),
                        downloadButton(ns('download_diagnostic_plot_residual'), label = "Download plot")
                      )),
               column(6,
                      bs4Dash::box(
                        width = 12,
                        collapsible = FALSE,
                        title = 'Residual normality',
                        plotOutput(ns('analysis_diagnostics_plot_normal'),
                                   height = 500),
                        downloadButton(ns('download_diagnostic_plot_normal'), label = "Download plot")
                      ))))
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
#
#       inference_group <- switch (bart_model$estimand,
#                                  ate = length(bart_model$sd.obs[!bart_model$missingRows]),
#                                  att = length(bart_model$sd.obs[!bart_model$missingRows] & bart_model$trt == 1),
#                                  atc = length(bart_model$sd.obs[!bart_model$missingRows] & bart_model$trt == 0)
#       )
#
#       # calculate summary stats
#       sd.cut = c(trt = max(bart_model$sd.obs[!bart_model$missingRows & bart_model$trt > 0]), ctl = max(bart_model$sd.obs[!bart_model$missingRows & bart_model$trt <= 0])) + sd(bart_model$sd.obs[!bart_model$missingRows])
#       total_sd <- switch (bart_model$estimand,
#                           ate = sum(bart_model$sd.cf[bart_model$trt==1] > sd.cut[1]) + sum(bart_model$sd.cf[bart_model$trt==0] > sd.cut[2]),
#                           att = sum(bart_model$sd.cf[bart_model$trt==1] > sd.cut[1]),
#                           atc = sum(bart_model$sd.cf[bart_model$trt==0] > sd.cut[2])
#       )
#
#
#       # calculate chisqr rule
#       total_chi <-  switch (bart_model$estimand,
#                             ate = sum((bart_model$sd.cf / bart_model$sd.obs) ** 2 > 3.841),
#                             att = sum((bart_model$sd.cf[bart_model$trt ==1] / bart_model$sd.obs[bart_model$trt ==1]) ** 2 > 3.841),
#                             atc = sum((bart_model$sd.cf[bart_model$trt ==0] / bart_model$sd.obs[bart_model$trt ==0]) ** 2 > 3.841)
#       )
#
#       # calculate sd rule
#       prop_sd <- round((total_sd / inference_group)*100 , 2)
#       prop_chi <- round((total_chi / inference_group)*100, 2)

        # plot it
        p <- plotBart::plot_common_support(
          .model = bart_model,
          rule = 'both'
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

      # if "none" is selected, change it to NULL, otherwise remain the same
      covariates_selection <- switch(input$analysis_diagnostics_plot_residual_covariate, "None" = NULL, input$analysis_diagnostics_plot_residual_covariate)

      bart_model <- store$analysis$model$model
      # p1 <- plot_residual_observed_predicted(.model = bart_model,
      #                                        covariate = covariates_selection)
      p3 <- plot_residual_predicted_residual(.model = bart_model,
                                             covariate = covariates_selection)

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
