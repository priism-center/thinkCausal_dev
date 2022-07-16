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
      bs4Dash::box(
        width = 3,
        collapsible = FALSE,
        title = 'Subgroup analysis',
         conditionalPanel(condition = "input.analysis_subgroup_tabs == 'Pre-specified Subgroup Analysis'", ns = ns,
                          h6("Pre-specified Subgroup Analysis"),
                          p("Examine the treatment effect conditional on the selected pre-specifed subgroup."),
                          br(),
                          selectInput(inputId = ns('analysis_subgroup_prespecifed'),
                                      label = 'Subgroup results by:',
                                      choices = NULL,
                                      selected = NULL
                          )
         ),
         conditionalPanel(condition = "input.analysis_subgroup_tabs == 'Search'", ns = ns,
                          h6("Search covariates for predictors of treatment effect heterogeneity"),
                          p('This regression tree uses covariates as predictors of ICATEs, predictors shown in the tree are helpful in informing exploratory subgroup analyses'),
                          sliderInput(inputId = ns('plotBart_tree_depth'),
                                      label = "Choose a Tree depth:",
                                      value = 2,
                                      min = 1, max = 3, step = 1)
         ),
         conditionalPanel(condition = "input.analysis_subgroup_tabs == 'Exploratory Subgroup Analysis'", ns = ns,
                          h6("Exploratory Subgroup Analyses"),
                          p("These results are exploratory in nature. Examine the treatment effect conditional on the selected subgroup."),
                          br(),
                          selectInput(inputId = ns('analysis_subgroup_explore'),
                                      label = 'Subgroup results by:',
                                      choices = NULL,
                                      selected = NULL)
         ),
         conditionalPanel(condition = "input.analysis_subgroup_tabs == 'ICATE'", ns = ns,
                          h6("ICATE plots"),
                          p('Check for heterogenety in the treatment effect by exploring the variation of ICATEs.'),
                          selectInput(inputId = ns('icate_plot_type'),
                                      label = 'Plot type:',
                                      choices = c('Histogram', 'Waterfall'),
                                      selected = 'Histogram'),
                          conditionalPanel(condition = "input.icate_plot_type == 'Histogram'", ns = ns,
                                           sliderInput(
                                             inputId = ns("plotBart_ICATE_n_bins"),
                                             label = "Number of bins: ",
                                             min = 5,
                                             max = 100,
                                             value = 30,
                                             step = 5
                                           ))
         ),
         br(), br(),
         # create_link_to_help('Subgroup analyses', button_label = 'What is this plot telling me?'),
        actionButton(inputId = ns('analysis_subgroup_help'),
                     label = 'What is this plot telling me?'),
         downloadButton(ns('download_subgroup_plot'), label = "Download plot"),
         actionButton(inputId = ns('analysis_moderator_analyses_button_reproduce'),
                      label = 'View analysis log')

       ),

       bs4Dash::tabBox(
         width = 9,
         collapsible = FALSE,
         id = ns("analysis_subgroup_tabs"),
         # bs4Dash::tabBox(id = ns("analysis_subgroup_tabs"),
         tabPanel(title = 'Pre-specified Subgroup Analysis',
                              br(),
                              plotOutput(outputId = ns('analysis_subgroup_prespecified_plot'),
                                         height = 500)
                     ),
         tabPanel(title = 'Search',
                              br(),
                              plotOutput(outputId = ns('analysis_subgroup_search_plot'),
                                         height = 500)
                     ),
         tabPanel(title = 'Exploratory Subgroup Analysis',
                              br(),
                              plotOutput(outputId = ns('analysis_subgroup_explore_plot'),
                                         height = 500)
                     ),
         tabPanel(title = 'ICATE',
                              br(),
                              plotOutput(outputId = ns('analysis_moderators_icate_plot'),
                                         height = 500))
       )
    )
  )
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

    # update pre-specifed moderators
    observeEvent(store$analysis$subgroup$prespecified_subgroups, {
      options <- store$analysis$subgroup$prespecified_subgroups
      updateSelectInput(session = store$session_global,
                        inputId = ns('analysis_subgroup_prespecifed'),
                        choices = options,
                        selected = options[1])
    })



    # pre-specifed subgroups
    analysis_pre_specified_moderators <- reactive({
      validate_model_fit(store)
      validate_prespecifed_moderators(store)

      # clean input text
      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      cols_continuous <- gsub('X_', '', store$column_types$continuous)

      # add overlay
      div_id <- 'analysis_results_plot_prespecifed'
      show_message_updating(div_id)

      if(input$analysis_subgroup_prespecifed %in% cols_categorical){
        p <- plotBart::plot_moderator_d_density(store$analysis$model$model,
                                      moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecifed)]])
      }else{
        p <- plotBart::plot_moderator_c_loess(store$analysis$model$model,
                                    moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_prespecifed)]])
      }

      p <- p + store$options$theme_custom

      # remove overlay
      close_message_updating(div_id)
      return(p)

    })

    output$analysis_subgroup_prespecified_plot <- renderPlot(analysis_pre_specified_moderators())

    # explore subgroups
    observeEvent(store$analysis$model$model, {
      options <- gsub("X_", '',grep("^X_", colnames(store$verified_df), value = TRUE))
      updateSelectInput(session = store$session_global,
                        inputId = ns('analysis_subgroup_explore'),
                        label = 'Subgroup results by:',
                        choices = c('', options),
                        selected = 1)
    })



    analysis_explore_moderators <- reactive({
      validate_model_fit(store)
      validate(need(input$analysis_subgroup_explore  != '', "Choose a variable for exploratory subgroup analysis"))


      # add overlay
      div_id <- 'analysis_results_plot_exploratory'
      show_message_updating(div_id)

      cols_categorical <- gsub('X_', '',store$column_types$categorical)
      cols_continuous <- gsub('X_', '', store$column_types$continuous)

      if(input$analysis_subgroup_explore %in% cols_categorical){
        p <- plotBart::plot_moderator_d_density(store$analysis$model$model,
                                      moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_explore)]])
      }else{
        p <- plotBart::plot_moderator_c_loess(store$analysis$model$model,
                                    moderator = store$verified_df[[paste0('X_', input$analysis_subgroup_explore)]])
      }

      p <- p + store$options$theme_custom

      # remove overlay
      close_message_updating(div_id)
      return(p)

    })

    output$analysis_subgroup_explore_plot <- renderPlot(analysis_explore_moderators())


    # ICATE plots
    analysis_moderators_icate_plot <- reactive({

      # stop here if model isn't fit yet
      validate_model_fit(store)

      if(input$icate_plot_type == 'Histogram'){
        p <- plotBart::plot_ICATE(
          store$analysis$model$model,
          n_bins = input$plotBart_ICATE_n_bins)
        # add theme
        p <- p + store$options$theme_custom
      }

      if(input$icate_plot_type == 'Waterfall'){
        # plot it
        p <- plotBart::plot_waterfall(
          .model = store$analysis$model$model
        )

        # add theme
        p <- p + store$options$theme_custom
      }

      return(p)
    })
    output$analysis_moderators_icate_plot <- renderPlot(analysis_moderators_icate_plot())

    # plot explore tab regression tree
    analysis_subgroup_search_plot <- reactive({
      validate_model_fit(store)
      p <- plotBart::plot_moderator_search(store$analysis$model$model, max_depth = input$plotBart_tree_depth)
      return(p)
    })

    output$analysis_subgroup_search_plot <- renderPlot(analysis_subgroup_search_plot())


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

## To be copied in the UI
# mod_analysis_subgroup_ui("analysis_subgroup_1")

## To be copied in the server
# mod_analysis_subgroup_server("analysis_subgroup_1")
