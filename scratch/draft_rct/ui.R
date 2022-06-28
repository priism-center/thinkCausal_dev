
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Randomized Experements Draft"),
      p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Hac habitasse platea dictumst quisque. Dictum at tempor commodo ullamcorper a lacus vestibulum. Curabitur gravida arcu ac tortor dignissim convallis aenean et tortor. Vitae turpis massa sed elementum tempus egestas sed sed. Egestas dui id ornare arcu odio. Malesuada nunc vel risus commodo viverra maecenas accumsan lacus. Aliquet risus feugiat in ante metus dictum at tempor commodo. Dignissim cras tincidunt lobortis feugiat vivamus. Id neque aliquam vestibulum morbi. Donec ac odio tempor orci dapibus ultrices in iaculis nunc. Aliquam eleifend mi in nulla posuere sollicitudin aliquam ultrices sagittis.'),
      br(),
      p('In nulla posuere sollicitudin aliquam ultrices sagittis orci. Mauris in aliquam sem fringilla ut morbi tincidunt. Praesent semper feugiat nibh sed pulvinar. Donec adipiscing tristique risus nec feugiat in fermentum. Mi tempus imperdiet nulla malesuada pellentesque. Tortor id aliquet lectus proin nibh nisl condimentum id venenatis. Varius vel pharetra vel turpis nunc eget lorem. Platea dictumst quisque sagittis purus sit amet volutpat consequat mauris. Non blandit massa enim nec dui nunc. Est lorem ipsum dolor sit amet consectetur. Sed felis eget velit aliquet sagittis id consectetur.')
    ,
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Sample Size:",
                        min = 50,
                        max = 10000,
                        value = 2000, 
                        step = 50, 
                        ticks = FALSE), 
            checkboxGroupInput('rct', 
                         label = 'Assignment Mechanism:', 
                         choices = c('Random', 
                                     'Non-Random'), 
                         selected = c('Random', 'Non-Random'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("balance")
        )
    ), 
    p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Hac habitasse platea dictumst quisque. Dictum at tempor commodo ullamcorper a lacus vestibulum. Curabitur gravida arcu ac tortor dignissim convallis aenean et tortor. Vitae turpis massa sed elementum tempus egestas sed sed. Egestas dui id ornare arcu odio. Malesuada nunc vel risus commodo viverra maecenas accumsan lacus. Aliquet risus feugiat in ante metus dictum at tempor commodo. Dignissim cras tincidunt lobortis feugiat vivamus. Id neque aliquam vestibulum morbi. Donec ac odio tempor orci dapibus ultrices in iaculis nunc. Aliquam eleifend mi in nulla posuere sollicitudin aliquam ultrices sagittis.'),
    br(),
    p('In nulla posuere sollicitudin aliquam ultrices sagittis orci. Mauris in aliquam sem fringilla ut morbi tincidunt. Praesent semper feugiat nibh sed pulvinar. Donec adipiscing tristique risus nec feugiat in fermentum. Mi tempus imperdiet nulla malesuada pellentesque. Tortor id aliquet lectus proin nibh nisl condimentum id venenatis. Varius vel pharetra vel turpis nunc eget lorem. Platea dictumst quisque sagittis purus sit amet volutpat consequat mauris. Non blandit massa enim nec dui nunc. Est lorem ipsum dolor sit amet consectetur. Sed felis eget velit aliquet sagittis id consectetur.')
    
))
