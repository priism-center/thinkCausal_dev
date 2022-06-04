welcome_header <- tabPanel(
  title = icon('home'),  
  value = 'Welcome',
  mainPanel(
    width = 12,
    br(),
    div(id = 'conceptsGridHome',
        class = 'conceptsGrid',
        fluidRow(
          column(
            5,
            h2('Welcome to thinkCausal'),
            p("Learn the foundations of causal inference through interactive tools. Analyze your causal inference data through a point and click interface."),
            # p("thinkCausal is "),
            p("Explore the learning modules to gain a deeper understanding of the methods and underlying assumptions -- from causal estimands to the Bayesian Additive Regression Tree (BART) algorithm. Our library of learning modules provides the foundational concepts of causal inference and is expanding monthly."),
            p("Or conduct your own analysis in a scaffolded and visual process. Estimate the treatment effects of your own study by fitting a BART model to your data. Leverage the built-in diagnostics to understand the fit and performance.")
          ),
          column(
            7,
            fluidRow(
              create_learning_card(
                width = 12,
                page_id = 'welcome_link_concepts',
                thumbnail_url = 'randomization.png',
                title = "Learn",
                description = "Interactively learn the foundational concepts of casual inference."
              )
            ),
            fluidRow(
              create_learning_card(
                width = 12,
                page_id = 'welcome_link_Analysis',
                thumbnail_url = 'assumptions.png',
                title = "Analyze",
                description = "Utilize modern causal inference methods. Easily implement Bayesian Additive Regression Trees.")
            )
          )
        )
    )
  )
)
