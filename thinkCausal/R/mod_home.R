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

    div(
      class = 'home-grid',
      fluidRow(
        column(
          4,
          h2('Welcome to thinkCausal'),
          p("Learn the foundations of causal inference through interactive tools. Analyze your causal inference data through a point and click interface."),
          p("Explore the learning modules to gain a deeper understanding of the methods and underlying assumptions -- from causal estimands to the Bayesian Additive Regression Tree (BART) algorithm. Our library of learning modules provides the foundational concepts of causal inference and is expanding monthly."),
          p("Or conduct your own analysis in a scaffolded and visual process. Estimate the treatment effects of your own study by fitting a BART model to your data. Leverage the built-in diagnostics to understand the fit and performance.")
        ),
        column(
          4,
          bs4Dash::box(
            width = 12,
            collapsible = FALSE,
            title = 'Learn',
            tagList(
              shiny::actionLink(
                inputId = ns('learn_img'),
                img(src = 'www/img/thumbnails/randomization.png',
                    width = '100%'),
              ),
              "Interactively learn the foundational concepts of casual inference."
            )
          ),
        ),
        column(
          4,
          bs4Dash::box(
            width = 12,
            collapsible = FALSE,
            title = 'Analyze',
            tagList(
              shiny::actionLink(
                inputId = ns('analysis_img'),
                img(src = 'www/img/thumbnails/assumptions.png',
                    width = '100%'),
              ),
              "Utilize modern causal inference methods. Easily implement Bayesian Additive Regression Trees."
            )
          )
        )
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # TODO: this prevents a bug in the observeEvent that somehow overrides store with ns()
    store

    # links from home to learn home page and analysis
    observeEvent(input$learn_img, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'learn')
    })
    observeEvent(input$analysis_img, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_describe')
    })

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
