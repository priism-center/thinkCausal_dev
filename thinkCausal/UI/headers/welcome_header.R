welcome_header <- tabPanel(
  title = 'Welcome', 
  mainPanel(
    width = 12,
    #h2("thinkCausal"),
    h3('Getting Started'),
    p("Welcome to thinkCausal! Learn the foundations of causal inference through interactive tools. Analyze causal inference data through a point and click interface."),
    br(),
    div(id = 'conceptsGridHome',
        class = 'conceptsGrid',
        fluidRow(
          column(6, 
                 wellPanel(
                   actionLink("welcome_link_concepts", img(src = 'thumbnails/randomization.png')),
                   br(),
                   h3("Learn"),
                   p("Interactively learn the foundational concepts of casual inference.")
                 )
          ),
          # column(4, 
          #        wellPanel(
          #          actionLink("concepts_link_Fundamental problem", img(src = 'thumbnails/fundamental_problem.png')),
          #          br(),
          #          h3("Practice"),
          #          p("Test your understanding of causal inference. Build confidence and identify opurtunities for growth.")
          #        )
          # ),
          column(6, 
                 wellPanel(
                   actionLink("welcome_link_Analysis", img(src = 'thumbnails/assumptions.png')),
                   br(),
                   h3("Analyze"),
                   p("Unlock modern causal inference methods. Easily implement Bayesian Additive Regression Trees")
                 )
          ),
          column(12,
                 # load custom css
                 includeCSS(file.path('www', 'learning', 'estimands', 'css', 'pairing.css')),
                 
                 # load d3
                 # tags$script(src = "https://code.jquery.com/jquery-3.3.1.slim.min.js",
                 #             integrity = "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
                 #             crossorigin = "anonymous"),
                 # tags$script(src = "https://d3js.org/d3.v5.js"),
                 # tags$script(src = "https://cdn.jsdelivr.net/jstat/latest/jstat.min.js"),
                 
                 # load custom javascript
                 tags$script(src = file.path('learning', 'estimands', 'js', 'buildPlot.js')),
                 tags$script(src = file.path('learning', 'estimands', 'js', 'showData.js')),
                 div(
                   class = 'estimands-d3-container',
                   div(id = 'plot-container',
                       div(id = 'plot-scatter'))
                 )
          )
        )
        ),
    # actionButton('test_popup', 'pop up'),
    # verbatimTextOutput('testytest'),
    # HTML(interactive_table),
    br()
    # h3("What is thinkCausal?"),
    # includeMarkdown('UI/markdowns/landing.md')
    )
)