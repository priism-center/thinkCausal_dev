library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # for button click
    shinyjs::useShinyjs(),
    
    # load first chunk of text
    includeMarkdown("regression_trees/text_1.md"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 4,
            actionButton(inputId = 'button_generateData',
                         label = "Generate data"),
            br(), br(),
            tableOutput(outputId = "table_confusionMatrix"),
            br(),
            sliderInput(inputId = 'slider11',
                        label = 'X1',
                        min = -1,
                        max = 1,
                        step = 0.01,
                        value = 0,
                        ticks = FALSE),
            radioButtons(inputId = 'radio11',
                         label = NULL,
                         choices = c("Left", "Right")),
            sliderInput(inputId = 'slider12',
                        label = NULL,
                        min = -1,
                        max = 1,
                        step = 0.01,
                        value = 0,
                        ticks = FALSE),
            radioButtons(inputId = 'radio12',
                         label = NULL,
                         choices = c("Left", "Right"),
                         selected = 'Right'),
            br(),
            sliderInput(inputId = 'slider21',
                        label = 'X2',
                        min = -1,
                        max = 1,
                        step = 0.01,
                        value = 0,
                        ticks = FALSE),
            radioButtons(inputId = 'radio21',
                         label = NULL,
                         choices = c("Top", "Bottom")),
            sliderInput(inputId = 'slider22',
                        label = NULL,
                        min = -1,
                        max = 1,
                        step = 0.01,
                        value = 0,
                        ticks = FALSE),
            radioButtons(inputId = 'radio22',
                         label = NULL,
                         choices = c("Top", "Bottom"),
                         selected = "Bottom")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            width = 8,
           plotOutput("plot_regressionTree")
        )
    ),
    
    # load second chunk of text
    includeMarkdown("regression_trees/text_2.md")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # initiate store
    store_regression_trees <- reactiveValues(.data = NULL)
    
    # click button on load so data appears automatically
    shinyjs::click('button_generateData')
    
    # generate data on click
    observeEvent(input$button_generateData, {
        n <- 1000
        store_regression_trees$.data <- tibble(
            X1 = runif(n, -1, 1),
            X2 = runif(n, -1, 1),
            Y = (round(X1 * runif(n, 0.9, 1.1)) == round(X2)) * 1
        )
    })
    
    # maintain a reactive vector of the predictions
    preds <- reactive({
        
        # retrieve data from store
        dat <- store_regression_trees$.data
        
        # switch between greater than or less than
        comparison11 <- switch(input$radio11, 
                              "Left" = .Primitive("<="), 
                              "Right" = .Primitive(">"))
        comparison12 <- switch(input$radio12, 
                              "Left" = .Primitive("<="), 
                              "Right" = .Primitive(">"))
        comparison21 <- switch(input$radio21, 
                              "Top" = .Primitive(">"), 
                              "Bottom" = .Primitive("<="))
        comparison22 <- switch(input$radio22, 
                               "Top" = .Primitive(">"), 
                               "Bottom" = .Primitive("<="))
        
        # calculate predictions and accuracy
        # TODO: this needs work
        # TODO: add ability to add or remove splits
        preds <- (comparison11(dat$X1, input$slider11) | 
            comparison12(dat$X2, input$slider12)) &
            (comparison21(dat$X2, input$slider21) |
            comparison22(dat$X2, input$slider22))
        preds <- as.numeric(preds)
        
        return(preds)
    })
    
    # render confusion matrix
    output$table_confusionMatrix <- renderTable({
        
        # retrieve data from store
        dat <- store_regression_trees$.data
        
        # calculate confusion matrix
        conf_mat <- table(Y = dat$Y, Y_hat = preds())
        
        # convert to dataframe for printing
        conf_mat <- conf_mat %>% 
            as_tibble() %>% 
            pivot_wider(values_from = n, names_from = Y_hat) %>% 
            t() %>% 
            as_tibble() %>% 
            slice(2:3) %>% 
            mutate(tmp = c('True class 0', 'True class 1')) %>% 
            select(tmp, V1, V2) %>% 
            rename(' ' = tmp,
                   'Predicted\nclass 0' = V1,
                   'Predicted\nclass 1' = V2)
        
        return(conf_mat)
    })
    
    # plot the output
    output$plot_regressionTree <- renderPlot({
        
        # retrieve data from store
        dat <- store_regression_trees$.data
        
        # plot it
        p <- dat %>% 
            mutate(pred = preds(),
                   correct = Y == pred,
                   correct = recode(as.character(correct), 
                                    'FALSE' = 'Incorrect',
                                    'TRUE' = 'Correct prediction'),
                   Y = paste0("Class ", Y)) %>% 
            ggplot(aes(x = X1, y = X2, shape = Y, color = correct)) +
            geom_point(size = 3, alpha = 0.5) +
            geom_vline(xintercept = input$slider11, color = 'blue') +
            geom_vline(xintercept = input$slider12, color = 'blue') +
            geom_hline(yintercept = input$slider21, color = 'blue') +
            geom_hline(yintercept = input$slider22, color = 'blue') +
            scale_shape_manual(values = c(1, 19)) +
            scale_color_manual(values = c('#5f7d71', 'red')) +
            labs(shape = NULL,
                 color = NULL)
        
        return(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
