welcome_dropdown <- tabPanel(
  title = 'Welcome', 
  mainPanel(
    width = 12,
    h2("thinkCausal"),
    h3('Getting Started'),
    p('Lorem ipsum dolor sit amet consectetur adipiscing elit netus posuere quisque ultricies, tempor habitasse ac risus ultrices egestas auctor eu aliquam sodales. Tempor molestie nisi nec venenatis tristique dui, semper tortor at eu mauris cubilia etiam, ligula diam consequat dictum pellentesque. In auctor sagittis etiam consequat potenti eget per scelerisque dictumst nascetur turpis vulputate, viverra cursus integer class dis posuere mollis pellentesque urna risus.'),
    br(),
    div(id = 'conceptsGridHome',
        class = 'conceptsGrid',
        fluidRow(
          column(4, 
                 wellPanel(
                   actionLink("welcome_link_concepts", img(src = 'thumbnails/randomization.png')),
                   br(),
                   h3("Learn"),
                   p("Interactivly learn the foundational concepts of casual inference and new methods for casual inference.")
                 )
          ),
          column(4, 
                 wellPanel(
                   actionLink("concepts_link_Fundamental problem", img(src = 'thumbnails/fundamental_problem.png')),
                   br(),
                   h3("Practice"),
                   p("Test your understanding of causal inference. Build confidence and identify opurtunities for growth.")
                 )
          ),
          column(4, 
                 wellPanel(
                   actionLink("welcome_link_Analysis", img(src = 'thumbnails/assumptions.png')),
                   br(),
                   h3("Fit"),
                   p("Unlock modern causal inference methods. Easily implement Bayesian Additive Regression Trees")
                 )
          )
        )
        ),
    h3("What is thinkCausal?"),
    includeMarkdown('UI/markdowns/landing.md')
    )
)