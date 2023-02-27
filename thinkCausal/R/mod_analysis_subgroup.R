#' analysis_subgroup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_subgroup_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             bs4Dash::box(
               width = 12,
               collapsible = FALSE,
               title = 'Subgroup analysis',
               shinyWidgets::radioGroupButtons(
                 inputId = ns('analysis_subgroup_type'),
                 label = 'Subgroup analysis step',
                 # choices = c('Step 1: ICATE variation',
                 #             'Step 2: Predictors of ICATE variation',
                 #             'Step 3: Exploratory subgroup analysis'),
                 choices = c('Treatment effect variation',
                             'Exploratory subgroup analysis'),
                 selected = NULL,direction = 'vertical',
               ),
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Prespecified subgroup analysis'",
                 ns = ns,
                 selectInput(
                   inputId = ns('analysis_subgroup_prespecified'),
                   label = 'Define pre-specified subgroups by:',
                   choices = NULL,
                   selected = NULL
                 ),
                 selectInput(
                   inputId = ns('analysis_subgroup_prespecified_type'),
                   label = 'Plot type:',
                   choices = c('Histogram' = 'histogram',
                               'Density' = 'density',
                               'Error bar' = 'errorbar')
                 )
               ),
               # make ICATE plots
               # conditionalPanel(
               #   condition = "input.analysis_subgroup_type == 'Step 1: ICATE variation' || input.analysis_subgroup_type == 'Step 2: ICATE variation'",
               #   ns = ns,
               #   selectInput(
               #     inputId = ns('analysis_subgroup_icate'),
               #     label = 'Plot type:',
               #     choices = c('Waterfall', 'Histogram')
               #   ),
                 # conditionalPanel(
                 #   condition = "input.analysis_subgroup_icate == 'Histogram'",
                 #   ns = ns,
                 #   sliderInput(
                 #     inputId = ns('analysis_subgroup_icate_bins'),
                 #     label = 'number of bins:',
                 #     value = 20,
                 #     min = 5,
                 #     max = 50,
                 #     step = 1
                 #   )
                 # )
               #),
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Treatment effect variation'",
                 ns = ns,
                 sliderInput(
                   inputId = ns('analysis_subgroup_treedepth'),
                   label = 'Max tree depth:',
                   value = 2,
                   min = 1,
                   max = 3
                 ),
                 HTML('<details><summary>Advanced options (ICATE plots)</summary>'),
                 selectInput(
                   inputId = ns('analysis_subgroup_icate'),
                   label = create_info_icon(
                     'Show ICATE plot:',
                     'ICATEs are an individuals predicted causal effect. ICATE plots can be used to understand if there substantial variation in the treatment effect between people'
                   ),
                   selectize = TRUE,
                   choices = c('Select an ICATE plot' = '', 'ICATE Waterfall', 'ICATE Histogram')
                 ),
                 HTML('</details><br>')
               ),
               conditionalPanel(
                 condition = "input.analysis_subgroup_type == 'Exploratory subgroup analysis'",
                 ns = ns,
                 selectInput(inputId = ns('analysis_subgroup_exploratory'),
                             label = 'Define exploratory subgroups by:',
                             choices = NULL,
                             selected = NULL),
                 uiOutput(outputId = ns('render_analysis_subgroup_x_levels')),
                 selectInput(inputId = ns('analysis_subgroup_exploratory_type'),
                             label = 'Plot type:',
                             choices = c('Histogram' = 'histogram',
                                         'Density' = 'density',
                                         'Error bar' = 'errorbar')),
                 conditionalPanel(condition = "input.analysis_subgroup_exploratory_type != 'errorbar'", ns = ns,
                                  checkboxInput(inputId = ns('analysis_subgroup_facet'),
                                                label = 'Overlay Plots',
                                                value = FALSE,
                                  ),
                                  numericInput(inputId = ns('analysis_subgroup_ncol'),
                                               label = 'Number of Columns:',
                                               value = 1,
                                               min = 1,
                                               max = 5)
                 )
               ),
               br(), br(),
               # create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
               actionButton(inputId = ns('analysis_subgroup_help'),
                            label = 'What is this plot telling me?'),
               downloadButton(ns('download_subgroup_plot'), label = "Download plot"),
               actionButton(inputId = ns('analysis_moderator_analyses_button_reproduce'),
                            label = 'View analysis log')

             )),
      column(9,
             bs4Dash::box(
               id = ns('subgroup_info'),
               width = 12,
               collapsible = TRUE,
               title = 'Treatment Effect Variation',
               p('The regression tree below identifies covariates that explain the most variation in the treatment effect between units in your study. Variable at the top of the tree explain more variation in the treatment effect than variables at the bottom or not included in the tree. Researchers find this plot useful to help inform a starting point for exploratory sub-group analyses. If you do not theory or priors about sources of variation in the treatment effect it is recomended to start with variables included in the regression tree below.',
               )),
             bs4Dash::box(
               width = 12,
               collapsible = FALSE,
               # bs4Dash::tabBox(id = ns("analysis_subgroup_tabs"),
               title = textOutput(ns('analysis_subgroup_plot_title')),
               br(),
               plotOutput(
                 outputId = ns('analysis_subgroup_plot'),
                 height = 500
               )
             ))
    ))
}

#' analysis_subgroup Server Functions
#'
#' @noRd
mod_analysis_subgroup_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_subgroup_help, {
      open_help_sidebar(store, 'Subgroup analyses')
    })

    # nav buttons
    observeEvent(input$analysis_moderator_analyses_button_reproduce, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'reproduce')
    })

    output$analysis_subgroup_plot_title <- renderText({
      out <- switch(input$analysis_subgroup_type,
                    'Treatment effect variation' = 'Predictors of treatment effect variation',
                    'Exploratory subgroup analysis' = 'Exploratory subgroup analysis',
      )

      return(out)
    })

    # include pre-specifed or not
    observeEvent(store$analysis$model$fit_good, {
      if (!is.null(store$analysis$subgroup$prespecified_subgroups)) {
        subgroup_options <- c(
          'Prespecified subgroup analysis',
          'Treatment effect variation',
          'Exploratory subgroup analysis'
        )
      } else{
        subgroup_options <- c('Treatment effect variation',
                              'Exploratory subgroup analysis')
      }

      shinyWidgets::updateRadioGroupButtons(
        inputId = 'analysis_subgroup_type',
        choices = subgroup_options,
        selected = NULL)

      X <- grep("^X_", names(store$verified_df), value = TRUE)
      R <- grep("^R_", names(store$verified_df), value = TRUE)
      B <- grep("^B_", names(store$verified_df), value = TRUE)
      X <- c(X, R, B)
      X <- substr(X, 3, nchar(X))
      X <- c('', X)
      updateSelectInput(
        inputId = 'analysis_subgroup_exploratory',
        choices = X,
        selected = NULL

      )

      updateSelectInput(
        inputId = 'analysis_subgroup_prespecified',
        choices = store$analysis$subgroup$prespecified_subgroups,
        selected = store$analysis$subgroup$prespecified_subgroups[1]
      )
    })

    find_levels <- reactive({
      # identify all logicals and indicators
      cols_indicators <- names(store$verified_df)[-c(1:2)][store$current_simple_column_types[-c(1,2)] == 'Binary']

      validate_model_fit(store)

      if(paste0('X_', input$analysis_subgroup_exploratory) %in%  cols_indicators){
        levels <- identify_indicators(
          x = paste0('X_', input$analysis_subgroup_exploratory), # level to test
          store$verified_df[, cols_indicators] # possible categorical variables
        )

        levels$best <- substr(levels$best, 3, nchar(levels$best))

        return(levels$best)
      } else return(NULL)
    })


    output$render_analysis_subgroup_x_levels <- renderUI({
      levels_ui <- find_levels()
      if(length(levels_ui) > 1) {
        selectInput(
          inputId = ns("analysis_subgroup_x_levels"),
          label = paste0("Contrast ", input$analysis_subgroup_exploratory, " with:"),
          multiple = TRUE,
          # removing selected .x from options
          choices = levels_ui[which(levels_ui != input$analysis_subgroup_exploratory)],
          selected = levels_ui[which(levels_ui != input$analysis_subgroup_exploratory)]
        )
      }else NULL

    })


    # run analysis
    icate_plot <- reactive({
      validate_model_fit(store)

      if (input$analysis_subgroup_icate == 'Histogram') {
        p <- plotBart::plot_ICATE(store$analysis$model$model)
      } else{
        p <- plotBart::plot_waterfall(store$analysis$model$model)
      }

      p <- p + store$options$theme_custom

      return(p)
    })

    predict_icate_plot <- reactive({
      validate_model_fit(store)

      p <- plotBart::plot_moderator_search(store$analysis$model$model,
                                           max_depth = input$analysis_subgroup_treedepth)

      p <- p + store$options$theme_custom

      if(input$analysis_subgroup_icate == 'ICATE Histogram'){
        p2 <- plotBart::plot_ICATE(store$analysis$model$model) + store$options$theme_custom
        p <- p/p2
      }

      if(input$analysis_subgroup_icate == 'ICATE Waterfall'){
        p2 <- plotBart::plot_waterfall(store$analysis$model$model) + store$options$theme_custom
        p <- p/p2
      }

      return(p)
    })

    exploratory_plot <- reactive({
      validate_model_fit(store)
      validate(need(input$analysis_subgroup_exploratory, 'Select a variable to define subgroups'))

      if (input$analysis_subgroup_exploratory_type != 'errorbar') {
        .facet <- !input$analysis_subgroup_facet
      } else{
        .facet <- FALSE
      }

      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      cols_continuous <- gsub('X_', '', store$column_types$continuous)

      if(input$analysis_subgroup_exploratory %in% cols_categorical){
        if (length(find_levels()) > 1) {
          .moderator <-
            clean_dummies_to_categorical(
              df = store$verified_df,
              group_names =  paste0('X_', find_levels()),
              new_name = 'moderator'
            )[['moderator']]

          .moderator <- gsub('X_', '', .moderator)
        }else{
          .moderator <- store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]
        }

        p <- plotBart::plot_moderator_d(store$analysis$model$model,
                                        type = input$analysis_subgroup_exploratory_type,
                                        moderator = .moderator,
                                        facet = .facet)
      }else{
        .moderator <- store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]]
        p <- plotBart::plot_moderator_c_bin(store$analysis$model$model,
                                            moderator = .moderator,
                                            .name = input$analysis_subgroup_exploratory,
                                            type = input$analysis_subgroup_exploratory_type,
                                            facet = .facet)

        # p <- plotBart::plot_moderator_c_loess(store$analysis$model$model,
        #                                       moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_exploratory)]])
      }

      p <- p + store$options$theme_custom

      return(p)
    })


    prespecifed_plot <- reactive({
      validate_model_fit(store)

      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      cols_continuous <- gsub('X_', '', store$column_types$continuous)

      if (input$analysis_subgroup_prespecified %in% cols_categorical) {
        p <- plotBart::plot_moderator_d(store$analysis$model$model,
                                        type = input$analysis_subgroup_prespecified_type,
                                        moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecified)]])
      } else{
        p <- plotBart::plot_moderator_c_loess(store$analysis$model$model,
                                              moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecified)]])
      }

      p <- p + store$options$theme_custom

      return(p)
    })




    output$analysis_subgroup_plot <- renderPlot({

      validate_model_fit(store)

      p <- switch (input$analysis_subgroup_type,
                   'Treatment effect variation' = predict_icate_plot(),
                   'Pre-specified subgroup analysis' = prespecifed_plot(),
                   'Exploratory subgroup analysis' = exploratory_plot()
      )

      return(p)

    })

    # download plot
    output$download_subgroup_plot <- downloadHandler(
      filename = function() {
        switch(
          req(input$analysis_subgroup_tabs),
          "Pre-specified Subgroup Analysis" = 'pre_specified_subgroup_plot.png',
          "Search" = 'subgroup_regression_tree.png',
          "Exploratory Subgroup Analysis" = 'exploratory_subgroup_plot.png',
          "ICATE" = 'ICATE_plot.png'
        )
      },
      content = function(file) {

        # get the plot that is on the active tab
        active_plot <- switch(
          req(input$analysis_subgroup_tabs),
          'Pre-specified Subgroup Analysis' = analysis_pre_specified_moderators(),
          'Search' = analysis_subgroup_search_plot(),
          "Exploratory Subgroup Analysis" = analysis_explore_moderators(),
          'ICATE' = analysis_moderators_icate_plot()
        )

        # write out plot
        ggsave(file,
               plot = active_plot,
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
      }
    )



    return(store)
  })
}
