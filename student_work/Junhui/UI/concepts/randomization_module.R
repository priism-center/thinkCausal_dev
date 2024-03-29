# this defines the randomization page under Concepts

randomizationUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 5,
        h3("Why do we randomize?"),
        "Randomization balances groups on both observed and unobserved characteristics. See this for yourself. First set the x and y variables to observe.",
        br(), br(),
        selectInput(
          inputId = ns("randomization_variable_x"),
          label = "(Observed) x variable: ",
          multiple = FALSE,
          choices = setdiff(colnames(randomization_df), "treat"),
          selected = setdiff(colnames(randomization_df), "treat")[1]
        ),
        selectInput(
          inputId = ns("randomization_variable_y"),
          label = "(Observed) y variable: ",
          multiple = FALSE,
          choices = setdiff(colnames(randomization_df), "treat"),
          selected = setdiff(colnames(randomization_df), "treat")[3]
        ),
        "Now select which datapoints to include in the treatment group by clicking on points in the plot. Assign only younger people to treatment. How do the unobserved univariate densities compare between treatment and control?",
        br(), br(),
        "Now randomize the selections using the below button. How do the densities compare now?",
        br(), br(),
        actionButton(inputId = ns('randomize_button'),
                     label = "Randomize the treatment assignment"),
        br(), br(),
        actionButton(inputId = ns('randomize_reset_button'),
                     label = "Reset the treatment assignment"),
        br(), br(),
        HTML('<details><summary>What is this data?</summary>'),
        HTML('<br>The data are simulated. The values and correlations are reasonable but please do not make any material conclusions from the data.</a'),
        HTML('</details><br>')
      ),
      mainPanel(
        width = 7,
        plotOutput(ns('randomization_plot'),
                   click = ns("randomization_plot_click")),
        br(),
        plotOutput(ns('randomization_tc_plot'), height = 500),
        absolutePanel(id = ns("randomization_floating_box"), 
                      class = "floating_message",
                      top = 50, left = "auto", right = 50, bottom = "auto",
                      width = "30%", height = "auto", draggable = FALSE,
                      "Click on points to assign them to treatment!")
      )
    )
             )
}

randomizationServer <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # initiate list of treatment observations
      selected_points <- reactiveValues(row_names = NULL)
      
      observeEvent(input$randomization_plot_click, {
        
        # for each click on the plot, add or remove that data point from the list
        #   of selected data points
        tryCatch({
          # capture the point form the user click
          event_row <- rownames(
            nearPoints(
              randomization_df,
              input$randomization_plot_click,
              threshold = 15,
              maxpoints = 1
            )
          )
          
          if (event_row %in% selected_points$row_names) {
            # remove point from selected list
            selected_points$row_names <- setdiff(selected_points$row_names, event_row)
          } else if (event_row %in% rownames(randomization_df)) {
            # add point to selected list
            selected_points$row_names <- c(selected_points$row_names, event_row)
            # remove floating UI box
            removeUI(selector = paste0("#", ns("randomization_floating_box")))
          }
        },
        error = function(e) e)
        
      })
      
      # randomize assignment when user clicks button
      observeEvent(input$randomize_button, {
        selected_points$row_names <- sample(rownames(randomization_df), size = round(nrow(randomization_df)/2))
      })
      
      # remove assignment when user clicks button    
      observeEvent(input$randomize_reset_button, {
        selected_points$row_names <- c()
      })
      
      # top plot    
      output$randomization_plot <- renderPlot({
        # create plot
        p <- randomization_df %>%
          dplyr::select(-treat) %>%
          rownames_to_column() %>% 
          mutate(Group = if_else(rowname %in% selected_points$row_names,
                                 'Treatment', 'Control')) %>% 
          ggplot(aes_string(x = sym(input$randomization_variable_x), 
                            y = sym(input$randomization_variable_y))) +
          geom_point(aes(fill = Group), alpha = 0.7, size = 7, 
                     color = 'black', pch = 21, stroke = 1) +
          labs(fill = NULL)
        
        # add theme
        p <- p + plot_theme()
        
        return(p)
      })
      
      # bottom plot
      output$randomization_tc_plot <- renderPlot({
        
        # create plot
        p <- randomization_df %>%
          dplyr::select(-treat) %>%
          rownames_to_column() %>%
          mutate(Group = if_else(rowname %in% selected_points$row_names,
                                 'Treatment', 'Control')) %>%
          pivot_longer(cols = where(is.numeric)) %>%
          ggplot(aes(x = value, group = Group, fill = Group)) +
          geom_density(alpha = 0.5) +
          scale_y_continuous(labels = NULL) +
          facet_wrap(~name, scales = 'free', ncol = 3) +
          labs(title = "Univariate densities of the observed and unobserved variables",
               x = NULL,
               y = NULL) +
          theme(legend.position = 'none')
        
        # add theme
        p <- p + plot_theme()
        
        return(p)
      })
    }
  )
}

