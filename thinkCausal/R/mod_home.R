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
            title = 'Learn about causal inference',
            tagList(
              shiny::actionLink(
                inputId = ns('learn_img'),
                img(src = 'www/img/thumbnails/randomization.png',
                    width = '100%'),
              ),
              "Interactively learn the foundational concepts of casual inference."
            ),
            actionButton(inputId = ns('learn_start'),
                         label = 'Start learning!',
                         class = 'nav-path')
          ),
        ),
        column(
          4,
          bs4Dash::box(
            width = 12,
            collapsible = FALSE,
            title = 'Analyze your own data',
            tagList(
              shiny::actionLink(
                inputId = ns('analysis_img'),
                img(src = 'www/img/thumbnails/assumptions.png',
                    width = '100%'),
              ),
              "Easily implement Bayesian Additive Regression Trees to estimate average causal effects."
            ),
            actionButton(inputId = ns('analysis_start'),
                         label = 'Start analyzing!',
                         class = 'nav-path')
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

    # links from home to learn home page and analysis
    observeEvent(input$learn_img, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'learn')
    })
    observeEvent(input$analysis_img, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_upload')
    })
    observeEvent(input$analysis_start, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'analysis_upload')
    })
    observeEvent(input$learn_start, {
      bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = 'learn')
    })



  })
}
