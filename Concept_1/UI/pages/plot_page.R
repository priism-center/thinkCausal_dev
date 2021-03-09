plot_page <- tabPanel(
  title = "Exploratory Plots", #htmlOutput("exploration_tab_name"),
  tabsetPanel(
    id = "analysis_plot_tabs", 
    tabPanel(
      title = "Descriptive Plots",
      absolutePanel(id = "analysis_plots_descriptive_loading_message",
                    br(),
                    HTML("Data must be first uploaded and columns selected."),
                    style = "z-index: -2;"),
      uiOutput(outputId = 'analysis_plots_descriptive_eda_module')
    ), 
   
# Common Support EDA UI -------------------------------------------------------
      tabPanel(
      title = "Common Support Plots",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Check Common Support"),
          selectInput(
            inputId = "analysis_plot_overlap_select_var",
            label = "Select Variables for Overlap Check:",
            multiple = TRUE,
            choices = X_names,
            selected = X_names
          ),
          awesomeRadio(inputId = "dim.red",
                       label = "View:", inline = T, 
                       choices = c("By Variables" = 1, 
                                   "One Number Summary" = 2), 
                       selected = 1),
          br(),
          awesomeRadio(inputId = "overlap.type",
                       label = "Plot Type:", inline = T, 
                       choices = c('Histogram', 'Density'), 
                       selected = 'Histogram'),
          br(),
          tags$button(type = 'button',
                      class = 'btn btn-default help',
                      onclick = "openConceptsPage('Concept3')",
                      'What is this plot telling me?'),
          br(),br(),
          div(
            class = 'backNextContainer',
            actionButton(inputId = "analysis_plots_support_button_back",
                         label = "Back"),
            actionButton(inputId = "analysis_plots_support_button_next",
                         label = "Next")
          ),
          br(),
          create_progress_bar(3/7*100)
        ),
        mainPanel(
          width = 8,
          br(),
          plotOutput(outputId = "analysis_plot_overlap_plot", height = 800)
          
        )
      )
    ), 

# Balance EDA UI ----------------------------------------------------------
tabPanel(title = "Balance Plots",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             h4("Visualize Balance Between Treatment and Control"),
             selectInput(
               inputId = "analysis_plot_balance_select_var",
               label = "Select Variables for Balance Check:",
               multiple = TRUE,
               choices = X_names,
               selected = X_names
             ),
             br(),
             tags$button(type = 'button',
                         class = 'btn btn-default help',
                         onclick = "openConceptsPage('Concept3')",
                         'What is this plot telling me?'),
             br(),br(),
             div(
               class = 'backNextContainer',
               actionButton(inputId = "analysis_plots_balance_button_back",
                            label = "Back"),
               actionButton(inputId = "analysis_plots_balance_button_next",
                            label = "Next")
             ),
             br(),
             create_progress_bar(4/7*100)
             # add advanced option to remove scale
           ),
           mainPanel(
             width = 8,
             br(),
             plotOutput(outputId = "analysis_plot_balance_plot", 
                        height = 500)
           )
         ))
    )
  )


