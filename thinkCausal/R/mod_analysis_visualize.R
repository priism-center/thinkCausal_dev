#' analysis_visualize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_visualize_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 3,
        collapsible = FALSE,
        title = "Explore your data visually",
         selectInput(
           inputId = ns("analysis_eda_select_plot_type"),
           label = "Plot type:",
           multiple = FALSE,
           choices = c("Balance", "Scatter", "Histogram", "Barplot", "Density", "Boxplot")
         ),
         conditionalPanel("input.analysis_eda_select_plot_type == 'Balance'",
                          ns = ns,
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
                                      choices = c('Plot variables with most imbalance',
                                                  'Manually select variables to plot')
                          ),
                          conditionalPanel(condition = "input.analysis_balance_select == 'Plot variables with most imbalance'",
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
                          )
                          ),
         conditionalPanel(
           condition = "input.analysis_eda_select_plot_type != 'Balance'",
           ns = ns,
           # selectInput(
           #   inputId = ns("analysis_eda_variable_x"),
           #   label = "X: ",
           #   multiple = NULL,
           #   choices = NULL,
           #   selected = NULL
           # ),
           uiOutput(ns("render_analysis_eda_variable_x")),
           uiOutput(ns("render_analysis_eda_x_levels")),
           conditionalPanel(
             condition = "input.analysis_eda_select_plot_type == 'Scatter'",
             ns = ns,
             selectInput(
               inputId = ns("analysis_eda_variable_y"),
               label = "Y: ",
               multiple = FALSE,
               choices = NULL,
               selected = NULL
             ),
             selectInput(
               inputId = ns("analysis_eda_variable_fill"),
               label = "Fill color: ",
               multiple = FALSE,
               choices = NULL,
               selected = NULL
             ),
             selectInput(
               inputId = ns("analysis_eda_variable_shape"),
               label = "Shape: ",
               multiple = FALSE,
               choices = NULL,
               selected = NULL
             ),
             conditionalPanel(
               condition = "input.analysis_eda_variable_fill == 'Cluster'",
               ns = ns,
               selectInput(
                 inputId = ns("analysis_eda_variable_cluster"),
                 label = "Clustering algorithm: ",
                 multiple = FALSE,
                 choices = c('k-means', 'Hierarchical'),
                 selected = 'k-means'
               ),
               sliderInput(
                 inputId = ns("analysis_eda_variable_n_clusters"),
                 label = "Number of clusters: ",
                 min = 2,
                 max = 10,
                 value = 4,
                 step = 1
               ),
               HTML(
                 'Clustering using only selected X and Y variables. Not recommended when faceting.<br><br>'
               )
             ),
             selectInput(
               inputId = ns("analysis_eda_variable_size"),
               label = "Size: ",
               multiple = FALSE,
               choices = NULL,
               selected = NULL
             ),
             selectInput(
               inputId = ns("analysis_eda_variable_regression"),
               label = "Linear regression: ",
               multiple = FALSE,
               choices = c('None', 'Include'),
               selected = 'None'
             ),
           ),
           conditionalPanel(
             condition = "input.analysis_eda_select_plot_type == 'Histogram'",
             ns = ns,
             sliderInput(
               inputId = ns("analysis_eda_variable_n_bins"),
               label = "Number of bins: ",
               min = 5,
               max = 50,
               value = 20,
               step = 1
             )
           ),
           conditionalPanel(
             condition = "input.analysis_eda_select_plot_type == 'Boxplot'",
             ns = ns,
             selectInput(
               inputId = ns("analysis_eda_variable_group"),
               label = "Grouping: ",
               multiple = FALSE,
               choices = NULL
             )
           ),
           selectInput(
             inputId = ns("analysis_eda_variable_facet"),
             label = "Group by: ",
             multiple = FALSE,
             choices = c("None", NULL),
             selected = "None"
           ),
           conditionalPanel(
             condition = "input.analysis_eda_variable_facet != 'None'",
             ns = ns,
             selectInput(
               inputId = ns("analysis_eda_variable_facet_second"),
               label = "Group by second variable: ",
               multiple = FALSE,
               choices = c("None"),
               selected = "None"
             )
           ),
           conditionalPanel(
             condition = "input.analysis_eda_select_plot_type == 'Scatter'",
             ns = ns,
             sliderInput(
               inputId = ns("analysis_eda_variable_alpha"),
               label = "Opacity: ",
               min = 0.1,
               max = 1,
               value = 0.5,
               step = 0.1
             )
           )
         ),
         HTML('<details><summary>Advanced options</summary>'),
         checkboxInput(
           inputId = ns("analysis_eda_check_jitter"),
           label = "Jitter the points?",
           value = FALSE
         ),
         HTML('</details><br>'),
         br(), br(),
         # create_link_to_help('EDA', button_label = 'What should I be looking for?'),
         downloadButton(ns('download_descriptive_plot'), label = "Download plot"),
         actionButton(
           inputId = ns("analysis_plots_descriptive_button_next"),
           class = "nav-path",
           label = "Next"
         )
       ),

      bs4Dash::box(
        width = 9,
        collapsible = FALSE,
        title = "Plots",
        plotOutput(
          outputId = ns('analysis_eda_plot'),
          height = 600,
          brush = brushOpts(id = ns("analysis_eda_plot_brush"))
          ),
        br(),
        htmlOutput(outputId = ns("analysis_eda_brush_text")),
        reactable::reactableOutput(outputId = ns("analysis_eda_brush_info"))
      )
    )
  )
}

#' analysis_visualize Server Functions
#'
#' @noRd
mod_analysis_visualize_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # next button
    observeEvent(input$analysis_plots_descriptive_button_next, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_overlap')
    })


    # dynamic updates for balance plot
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

    # update variables on the eda page once the save button on the verify data page is clicked
    observeEvent(store$analysis$data$verify$analysis_verify_data_save, {
      new_col_names <- colnames(store$verified_df)
      cols_categorical <- store$column_types$categorical
      cols_continuous <- store$column_types$continuous

      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_pairs_vars",
        choices = new_col_names,
        selected = new_col_names
      )
      # updateSelectInput(
      #   session = session,
      #   inputId = "analysis_eda_select_plot_type",
      #   selected = store$analysis$data$verify$plot_vars$plot_type
      # )
      # updateSelectInput(
      #   session = session,
      #   inputId = "analysis_eda_variable_x",
      #   choices = new_col_names,
      #   selected = store$analysis$data$verify$plot_vars$X
      # )
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_y",
        choices = new_col_names,
        selected = store$analysis$data$verify$plot_vars$Y
      )
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_fill",
        choices = c("None", new_col_names),
        selected = store$analysis$data$verify$plot_vars$fill
      )
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_shape",
        choices = c("None", cols_categorical),
        selected = store$analysis$data$verify$plot_vars$shape
      )
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_size",
        choices = c("None", new_col_names),
        selected = store$analysis$data$verify$plot_vars$size
      )
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_group",
        choices = c("None", cols_categorical),
        selected = store$analysis$data$verify$plot_vars$grouping
      )
      updateSelectInput(
        session = session,
        inputId = "analysis_eda_variable_facet",
        choices = c("None", cols_categorical)
      )
      # # update selects on balance plots
      # X_cols <- grep("^X_", new_col_names, value = TRUE)
      # X_cols_continuous <- grep("^X_", cols_continuous, value = TRUE)
      #
      # # update options for balance
      # updateSelectInput(session = session,
      #                   inputId = 'analysis_plot_balance_select_var',
      #                   choices = X_cols_continuous,
      #                   selected = X_cols_continuous
      # )
      # updateSelectInput(session = session,
      #                   inputId = 'analysis_plot_overlap_select_var',
      #                   choices = X_cols_continuous,
      #                   selected = X_cols_continuous
      # )
    })

    # only show continuous variables if histogram, density, or boxplot is selected
    # only show categorical if barplot
    observeEvent(input$analysis_eda_select_plot_type, {

      plot_type <- input$analysis_eda_select_plot_type
      selection_current <- input$analysis_eda_variable_x

      if (plot_type %in% c("Histogram", "Density", "Boxplot")){

        # update the available variables to just continuous and keep the current
        # selection if its continuous
        vars_continuous <- store$column_types$continuous
        selection_new <- ifelse(selection_current %in% vars_continuous,
                                selection_current,
                                vars_continuous[1])

        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_x",
          choices = vars_continuous,
          selected = selection_new
        )
      } else if (plot_type == "Barplot") {

        # update the available variables to just categorical and keep the current
        # selection if its categorical
        vars_categorical <- store$column_types$categorical
        selection_new <- ifelse(selection_current %in% vars_categorical,
                                selection_current,
                                vars_categorical[1])
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_x",
          choices = vars_categorical,
          selected = selection_new
        )
      } else {
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_x",
          choices = colnames(store$verified_df),
          selected = selection_current
        )
      }
    })


    output$render_analysis_eda_variable_x <- renderUI({
      new_col_names <- colnames(store$verified_df)
      cols_categorical <- store$column_types$categorical
      cols_continuous <- store$column_types$continuous
      selectInput(
        inputId = ns("analysis_eda_variable_x"),
        label = "X: ",
        multiple = FALSE,
        choices = new_col_names,
        selected = store$analysis$data$verify$plot_vars$X
      )
    })


    find_levels <- reactive({
      # identify all logicals and indicators
      cols_indicators <- names(store$verified_df)[-c(1:2)][store$current_simple_column_types[-c(1,2)] == 'Binary']

      #req(input$analysis_eda_variable_x) # to prevent rendering error
      req(input$analysis_eda_variable_x)

      if(input$analysis_eda_variable_x %in%  cols_indicators){
        levels <- identify_indicators(
          x = input$analysis_eda_variable_x, # level to test
          store$verified_df[, cols_indicators] # possible categorical variables
        )

        return(levels$best)
      } else return(NULL)
    })


    output$render_analysis_eda_x_levels <- renderUI({
      levels_ui <- find_levels()
      if(length(levels_ui) > 1) {
        selectInput(
          inputId = ns("analysis_eda_x_levels"),
          label = paste0("Contrast ", input$analysis_eda_variable_x, " with:"),
          multiple = TRUE,
          # removing selected .x from options
          choices = levels_ui[which(levels_ui != input$analysis_eda_variable_x)],
          selected = levels_ui[which(levels_ui != input$analysis_eda_variable_x)]
        )
      }else NULL

    })


    # create the descriptive plots
    # build the exploration plots
    descriptive_plot <- reactive({

      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store)

      if(input$analysis_eda_select_plot_type == 'Balance'){
        # plot it
        X <- store$verified_df
        X <- clean_to_indicator(X)
        treatment_col <- grep("^Z_", names(X), value = TRUE)
        outcome_col <-   grep("^Y_", names(X), value = TRUE)

        if(input$analysis_balance_select == 'Plot variables with most imbalance'){
          .confounders <- colnames(X)[colnames(X) %notin% c(treatment_col, outcome_col)]
          # stop here if there are no columns selected
          validate(need(length(.confounders) > 0,
                        "No columns available or currently selected"))
          p <- plotBart::plot_balance(.data = X,
                                      treatment = treatment_col,
                                      confounders = .confounders,
                                      compare = input$analysis_balance_type,
                                      estimand = input$analysis_balance_estimand,
                                      limit_catagorical = input$analysis_balance_cat + 1,
                                      limit_continuous = input$analysis_balance_cont + 1
          )
        }else{
          .confounders <- input$analysis_balance_select_var
          # stop here if there are no columns selected
          validate(need(length(.confounders) > 0,
                        "No columns available or currently selected"))
          p <- plotBart::plot_balance(.data = X,
                                      treatment = treatment_col,
                                      confounders = .confounders,
                                      compare = input$analysis_balance_type,
                                      estimand = input$analysis_balance_estimand
          )
        }



        # add theme
        p <- p & store$options$theme_custom + ggplot2::theme(legend.position = 'none')

      }else{
      p <- tryCatch({
        plot_exploration(
          .data = store$verified_df,
          .plot_type = input$analysis_eda_select_plot_type,
          .x = input$analysis_eda_variable_x,
          .y = input$analysis_eda_variable_y,
          .levels = find_levels(),
          .fill = input$analysis_eda_variable_fill,
          .fill_static = 'grey20', #"#5c5980",
          .shape = input$analysis_eda_variable_shape,
          .size = input$analysis_eda_variable_size,
          .alpha = input$analysis_eda_variable_alpha,
          .vars_pairs = input$analysis_eda_variable_pairs_vars,
          .n_bins = input$analysis_eda_variable_n_bins,
          .jitter = input$analysis_eda_check_jitter,
          .groups = input$analysis_eda_variable_group,
          .facet = input$analysis_eda_variable_facet,
          .facet_second = input$analysis_eda_variable_facet_second,
          .include_regression = input$analysis_eda_variable_regression
        )
      }
      # warning = function(e) NULL,
      # error = function(e) NULL
      )

      # add theme
      p <- p + store$options$theme_custom
      }

      return(p)
    })
    output$analysis_eda_plot <- renderPlot(descriptive_plot())

    # to save the parameters of downloaded descriptive plots for reproducible script
    downloaded_descriptive_plot_parameters <- reactiveValues(df = list())

    # parameters of current descriptive plot
    descriptive_plot_parameters <-  reactive({
      list(.plot_type = input$analysis_eda_select_plot_type,
           .x = input$analysis_eda_variable_x,
           .y = input$analysis_eda_variable_y,
           .fill = input$analysis_eda_variable_fill,
           .fill_static = 'grey20', #"#5c5980",
           .shape = input$analysis_eda_variable_shape,
           .size = input$analysis_eda_variable_size,
           .alpha = input$analysis_eda_variable_alpha,
           .vars_pairs = input$analysis_eda_variable_pairs_vars,
           .n_bins = input$analysis_eda_variable_n_bins,
           .jitter = input$analysis_eda_check_jitter,
           .groups = input$analysis_eda_variable_group,
           .facet = input$analysis_eda_variable_facet,
           .facet_second = input$analysis_eda_variable_facet_second,
           .include_regression = input$analysis_eda_variable_regression)
    })

    output$download_descriptive_plot <- downloadHandler(
      filename = 'descriptive_plot.png',
      content = function(file) {
        ggsave(file,
               plot = descriptive_plot(),
               height = store$options$settings_options_ggplotHeight,
               width = store$options$settings_options_ggplotWidth,
               units = 'in',
               device = 'png')
        # save the parameters of the downloaded descriptive plot
        downloaded_descriptive_plot_parameters$df <- rbind(downloaded_descriptive_plot_parameters$df, descriptive_plot_parameters())
      })

    # text above the brush table
    output$analysis_eda_brush_text <- renderText({

      if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {
        txt <- "<h6>Highlight data points on the above plot to view their information below</h6>"
      } else {
        txt <- NULL
      }

      return(txt)
    })

    # table of brushed data points from plot
    output$analysis_eda_brush_info <- reactable::renderReactable({

      # stop here if data hasn't been uploaded and selected
      validate_data_verified(store, req_only = TRUE)

      # show only if there isn't faceting
      if (input$analysis_eda_variable_facet == "None" & input$analysis_eda_select_plot_type == 'Scatter') {

        reactable::reactable(
          brushedPoints(store$verified_df, input$analysis_eda_plot_brush)
        )
      }
    })

    # update second facet options so user cannot double facet on the same variable
    # b/c that causes an error
    observeEvent(input$analysis_eda_variable_facet, {
      if (input$analysis_eda_variable_facet != "None") {
        updateSelectInput(
          session = session,
          inputId = "analysis_eda_variable_facet_second",
          choices = setdiff(c("None", store$column_types$categorical), input$analysis_eda_variable_facet)
        )
      }
    })

    # save into store for reproducible script
    downloaded_descriptive_plot_parameters_df <- reactive(downloaded_descriptive_plot_parameters$df)


    observeEvent(input$analysis_plots_descriptive_button_next, {
      store$analysis$eda$downloaded_descriptive_plot_parameters <- downloaded_descriptive_plot_parameters_df()
    })

    return(store)
  })
}

## To be copied in the UI
# mod_analysis_visualize_ui("analysis_visualize_1")

## To be copied in the server
# mod_analysis_visualize_server("analysis_visualize_1")
