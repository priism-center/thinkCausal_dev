library(shiny)
library(shinyWidgets)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(1), 
        column(10,
    h2('All Confounders Measured'),
    h4('Choose an example: '),
    wellPanel(
        awesomeRadio(
            inputId = 'example',
            label = 'Options',
            choices = c('health', 'education', 'economics', 'exercise science'),
            selected = 'health'
        )
    ), 
    
    p("You've been tasked with determining whether or not a 6 month exerise program 
      was able to sucessfully decrease participants blood sugar. 
      Besides blod sugar and treatments status (exercise program or no exercise program), 
      there are measurements on baseline blood sugar, baseline exercise levels, 
      baseline water consumption and baseline salt consumption. How these covaraites influence your causal 
      analysis depends on wether participation in the exercse program was done 
      through random assigment or if the study is an observational study and 
      particpants decided themsevels wehtehr to enrol in the program or not"), 
    br(), 
    
    p("Use the buttons below to switch between a randomized design and an observational design.
      Look for how the relationship between covaraites and participation in the exercise program 
      differs between the two designs"),
    
    wellPanel(
        awesomeRadio(inputId = 'world.1', 
                     label = 'Study Design:', 
                     choices = c('Randomized', 'Observational')),
        
        selectInput(inputId = 'confounder.plt.input', 
                    label = 'Select a Covariate:', 
                    choices = c('baseline_blood_sugar',
                                   'baseline_exercise', 
                                   'baseline_salt', 
                                   'baseline_water'), 
                    )
    ),
    
    plotOutput('rct.plt.1')
    
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    set.seed(2)
    rct <- tibble(baseline_water = rnorm(200), 
                  baseline_salt = rnorm(200), 
                  baseline_exercise = rnorm(200), 
                  baseline_blood_sugar = rnorm(200), 
                  Z = rbinom(200, 1, .5))
    
    rct$Z <- ifelse(rct$Z == 1, 'exercise program', 'no exercise program')
    
    rct <- rct %>% mutate(y1 = 110 + 1*baseline_blood_sugar + 
                              .5*baseline_exercise + 
                              rnorm(200) - 10)
    
    rct <- rct %>% mutate(y0 = 110 + 1*baseline_blood_sugar + 
                              .5*baseline_exercise + 
                              rnorm(200))
    
    rct <- rct %>% mutate(y = if_else(Z ==1, y1, y0))
    

    
    output$rct.plt.1 <- renderPlot({
      if(input$world.1 == 'Randomized'){
        p <- ggplot(rct, aes_string(input$confounder.plt.input)) + 
          geom_density(aes(fill = Z), alpha = .5)
      }
        return(p)

    })
    
    
}


# Run the application
shinyApp(ui = ui, server = server)
