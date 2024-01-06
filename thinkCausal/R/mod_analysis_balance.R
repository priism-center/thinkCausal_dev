#' analysis_balance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_balance_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        width = 3,
        collapsible = FALSE,
        title = 'Visualize balance between treatment and control',
        selectInput(
          inputId = ns("analysis_balance_estimand"),
          label = 'Check balance for the:',
          choices = c('ATE', 'ATT', 'ATC'),
          selected = 'ATE'
        ),
        selectInput(
          inputId = ns("analysis_balance_type"),
          label = "Plot balance of:",
          choices = c('means', 'variance', 'covariance'),
          selected = 'means'
        ),
        selectInput(inputId = ns('analysis_balance_select'),
                    label = 'Filter variables in balance plot:',
                    choices = c('Plot varibables with most imbalance',
                                'Manually select variables to plot')
                    ),
        # checkboxInput(ns('analysis_balance_show_table'),
        #               label = 'Show balance table',
        #               value = TRUE
        #               ),
        conditionalPanel(condition = "input.analysis_balance_select == 'Plot varibables with most imbalance'",
                         ns = ns,
                         sliderInput(ns('analysis_balance_cat'),
                                     label = 'Categorical variables in balance plot:',
                                     value = 10,
                                     min = 0,
                                     max = 25),
                         sliderInput(ns('analysis_balance_cont'),
                                     label = 'Continuous variables in balance plot:',
                                     value = 10,
                                     min = 0,
                                     max = 25 )
                          ),
        conditionalPanel(condition = "input.analysis_balance_select == 'Manually select variables to plot'",
                         ns = ns,
                         selectInput(
                           inputId = ns("analysis_balance_select_var"),
                           label = "Select variables included in balance plot:",
                           multiple = TRUE,
                           choices = NULL,
                           selected = NULL
                         )
        ),
        br(),
        actionButton(
          inputId = ns('analysis_balance_help'),
          label = 'What is this plot telling me?'
        ),
        downloadButton(ns('download_balance_plot'), label = "Download plot"),
        actionButton(
          inputId = ns("analysis_balance_button_next"),
          class = "nav-path",
          label = "Next"
        )
      ),
      bs4Dash::box(
        width = 9,
        collapsible = FALSE,
        title = 'Balance plot',
        plotOutput(
          outputId = ns("analysis_balance_plot"),
          height = 500
        ),
        br()#,
        # conditionalPanel(condition = 'input.analysis_balance_show_table',
        #                  ns = ns,
        #                  reactable::reactableOutput(outputId = ns('analysis_balance_table'))
        #                  )
      )
      # column(3),
      #   bs4Dash::box(
      #     width = 9,
      #     collapsible = FALSE,
      #     title = 'Balance table',
      #     reactable::reactableOutput(outputId = ns('analysis_balance_table'))
      # )
    )
  )
}

#' analysis_balance Server Functions
#'
#' @noRd
mod_analysis_balance_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # open help on button click
    observeEvent(input$analysis_balance_help, {
      open_help_sidebar(store, 'Balance')
    })

    # next button
    observeEvent(input$analysis_balance_button_next, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_overlap')
    })




    # render default values when verified data is saved
    observeEvent(store$analysis$data$verify$analysis_verify_data_save,{
      X <- store$verified_df
      X <- clean_to_indicator(X)
      treatment_col <- grep("^Z_", names(X), value = TRUE)

      # get covariates
      new_col_names <- colnames(clean_to_indicator(store$verified_df))
      X_cols <- grep("^X_", new_col_names, value = TRUE)

      # send them off to the UI
      updateSelectInput(session = session,
                        inputId = 'analysis_balance_select_var',
                        choices = X_cols,
                        selected = NULL
      )

      updateSliderInput(session = session,
                        inputId = 'analysis_balance_cat',
                        max = ncol(X) - length(store$column_types$continuous)
                        )

      updateSliderInput(session = session,
                        inputId = 'analysis_balance_cont',
                        max = length(store$column_types$continuous)
      )

    })


    # create the balance plot
    balance_plot <- reactive({

      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      # plot it
      X <- store$verified_df
      X <- clean_to_indicator(X)
      treatment_col <- grep("^Z_", names(X), value = TRUE)
      outcome_col <-   grep("^Y_", names(X), value = TRUE)

      if(input$analysis_balance_select == 'Plot varibables with most imbalance'){
        .confounders <- colnames(X)[colnames(X)%notin% c(treatment_col, outcome_col)]
      }else{
        .confounders <- input$analysis_balance_select_var
      }

      # stop here if there are no columns selected
      validate(need(length(.confounders) > 0,
                    "No columns available or currently selected"))
      p <- plotBart::plot_balance(.data = X,
                                treatment = treatment_col,
                                confounders = .confounders,
                                compare = input$analysis_balance_type,
                                estimand = input$analysis_balance_estimand,
                                limit_catagorical = input$analysis_balance_cat,
                                limit_continuous = input$analysis_balance_cont
                                )

      # add theme
      p <- p & store$options$theme_custom + ggplot2::theme(legend.position = 'none')

      return(p)
    })

    output$analysis_balance_plot <- renderPlot({
      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      # build plot
      p <- balance_plot()

      return(p)
    })

    # create the balance table
    # balance_table <- reactive({
    #
    #   # stop here if data hasn't been uploaded and selected
    #   validate_data_verified(store)
    #
    #   # stop here if there are no numeric columns selected
    #   validate(need(length(input$analysis_balance_select_var) > 0,
    #                 "No columns available or currently selected"))
    #
    #   # create table
    #   X <- clean_to_indicator(store$verified_df)
    #   col_names <- colnames(X)
    #   treatment_col <- grep("^Z_", col_names, value = TRUE)
    #   confounder_cols <- input$analysis_balance_select_var
    #   if(input$analysis_balance_type == 'covariance'){
    #     p <- plotBart::print_covariance(.data = X,
    #                                  treatment = treatment_col,
    #                                  confounders = col_names[3:length(col_names)],
    #                                  estimand = input$analysis_balance_estimand)
    #   }else{
    #     p <- plotBart::print_balance(.data = X,
    #                                  treatment = treatment_col,
    #                                  confounders = col_names[3:length(col_names)],
    #                                  estimand = input$analysis_balance_estimand)
    #   }
    #
    #
    #   return(p)
    # })
    #
    # output$analysis_balance_table <-  reactable::renderReactable({
    #
    #   # stop here if data hasn't been uploaded and selected
    #   validate_data_verified(store)
    #
    #   # build table
    #   p <- balance_table() %>%
    #     reactable::reactable(defaultColDef = reactable::colDef(align = 'center'))
    #
    #   return(p)
    # })

    # to save the parameters of downloaded balance plots for reproducible script
    downloaded_balance_plot_parameters <- reactiveValues(df = list())

    # parameters of current balance plot
    balance_plot_parameters <- reactive({
      # get variables
      X <- store$verified_df
      col_names <- colnames(X)
      treatment_col <- grep("^Z_", col_names, value = TRUE)
      confounder_cols <- input$analysis_balance_select_var

      list(treatment = treatment_col,
           confounders = paste(confounder_cols, collapse = ','))

    })

    output$download_balance_plot <- downloadHandler(
      filename = 'balance_plot.png',
      content = function(file) {
        ggsave(file,
               plot = balance_plot(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
        # save the parameters of the downloaded overlap plot
        downloaded_balance_plot_parameters$df <- rbind(downloaded_balance_plot_parameters$df, balance_plot_parameters())
      })

    # save into store for reproducible script
    downloaded_balance_plot_parameters_df <- reactive(downloaded_balance_plot_parameters$df)

    observeEvent(input$analysis_balance_button_next, {
      store$analysis$eda$downloaded_balance_plot_parameters <- downloaded_balance_plot_parameters_df()
    })

    return(store)
  })
}
