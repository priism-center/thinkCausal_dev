welcome_header <- tabPanel(
  title = 'Welcome', 
  mainPanel(
    width = 12,
    h2("thinkCausal"),
    h3('Getting Started'),
    p("Welcome to thinkCausal! Learn the fondations of causal inference through interactive tools. Analyze causal inference data through a point and click interface."),
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
          )
        )
        ),
    verbatimTextOutput('testytest'),
    HTML(interactive_table)
    # h3("What is thinkCausal?"),
    # includeMarkdown('UI/markdowns/landing.md')
    )
)