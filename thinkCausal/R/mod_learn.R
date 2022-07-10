#' learn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),

    div(id = 'conceptsGrid1',
        class = 'conceptsGrid',

        fluidRow(
          bs4Dash::box(
            width = 4,
            tagList(
              shiny::actionLink(
                inputId = ns('learn_estimands_img'),
                img(src = 'www/img/thumbnails/estimands-cloud.png',
                    width = '90%'),
              ),
              h3('Causal estimands'),
              "BART allows for robust estimation of a wide variety of estimands. Learn how they differ and how to choose one."
            ),
            collapsible = FALSE
          ),
          bs4Dash::box(
            width = 4,
            tagList(
              shiny::actionLink(
                inputId = ns('learn_post_treatment_img'),
                img(src = 'www/img/thumbnails/post-treatment.png',
                    width = '90%'),
              ),
              h3('Post treatment variables'),
              "Post-treatment variables are a class of variables that can be affected by the treatment and should be removed prior to modeling. Learn how to identify them to make sure you are not biasing your treatment effect estimates."
            ),
            collapsible = FALSE
          ),
          bs4Dash::box(
            width = 4,
            tagList(
              img(src = 'www/img/thumbnails/potential-outcomes.png',
                  width = '90%'),
              h3('Potential outcomes'),
              "The potential outcomes framework is a methodology to estimate causal effects. Learn the theory foundations here."
            ),
            collapsible = FALSE
          )
        ),

        fluidRow(
          bs4Dash::box(
            width = 4,
            tagList(
              shiny::actionLink(
                inputId = ns('learn_randomization_img'),
                img(src = 'www/img/thumbnails/randomization.png',
                    width = '90%'),
              ),
              h3('Post treatment variables'),
              "Randomization balances groups on both observed and unobserved characteristics. Learn how this mechanism is exploited for causal inference."
            ),
            collapsible = FALSE
          ),
          bs4Dash::box(
            width = 4,
            tagList(
              shiny::actionLink(
                inputId = ns('learn_balance_img'),
                img(src = 'www/img/thumbnails/balance.png',
                    width = '90%'),
              ),
              h3('Bias and efficiency'),
              "Coming soon"
            ),
            collapsible = FALSE
          ),
          bs4Dash::box(
            width = 4,
            tagList(
              shiny::actionLink(
                inputId = ns('learn_bart_img'),
                img(src = 'www/img/thumbnails/decision_tree.png',
                    width = '90%'),
              ),
              h3('Bayesian Additive Regression Trees'),
              "Coming soon"
            ),
            collapsible = FALSE
          )
        )
    )

  )
}

#' learn Server Functions
#'
#' @noRd
mod_learn_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # TODO: this prevents a bug in the observeEvent that somehow overrides store with ns()
    store

    # links from learn home page to each learn article
    selectors <- c('learn_estimands', 'learn_post_treatment')
    purrr::map(selectors, function(sel){
      observeEvent(input[[glue::glue('{sel}_img')]], {
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = sel)
      })
    })

  })
}

## To be copied in the UI
# mod_learn_ui("learn_1")

## To be copied in the server
# mod_learn_server("learn_1")
