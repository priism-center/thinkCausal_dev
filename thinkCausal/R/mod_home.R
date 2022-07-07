#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        4,
        h2('Welcome to thinkCausal'),
        p("Learn the foundations of causal inference through interactive tools. Analyze your causal inference data through a point and click interface."),
        # p("thinkCausal is "),
        p("Explore the learning modules to gain a deeper understanding of the methods and underlying assumptions -- from causal estimands to the Bayesian Additive Regression Tree (BART) algorithm. Our library of learning modules provides the foundational concepts of causal inference and is expanding monthly."),
        p("Or conduct your own analysis in a scaffolded and visual process. Estimate the treatment effects of your own study by fitting a BART model to your data. Leverage the built-in diagnostics to understand the fit and performance.")
      ),
      column(
        8,
        bs4Dash::box(
          width = 6,
          tagList(
            img(src = 'www/img/thumbnails/randomization.png',
                width = '100%'),
            h3('Learn'),
            "Interactively learn the foundational concepts of casual inference."
          ),
          collapsible = FALSE
        ),
        bs4Dash::box(
          width = 6,
          tagList(
            img(src = 'www/img/thumbnails/assumptions.png',
                width = '100%'),
            h3('Analyze'),
            "Utilize modern causal inference methods. Easily implement Bayesian Additive Regression Trees."
          ),
          collapsible = FALSE
        )

        # fluidRow(
        #   create_learning_card(
        #     width = 12,
        #     page_id = 'welcome_link_concepts',
        #     thumbnail_url = 'randomization.png',
        #     title = "Learn",
        #     description = "Interactively learn the foundational concepts of casual inference."
        #   )
        # ),
        # fluidRow(
        #   create_learning_card(
        #     width = 12,
        #     page_id = 'welcome_link_Analysis',
        #     thumbnail_url = 'assumptions.png',
        #     title = "Analyze",
        #     description = "Utilize modern causal inference methods. Easily implement Bayesian Additive Regression Trees."
        #   )
        # )
      )
    )

  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")