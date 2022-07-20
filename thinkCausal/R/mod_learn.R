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

    div(
      class = 'learn-grid',
      fluidRow(
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Causal estimands',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_estimands_img'),
              img(src = 'www/img/thumbnails/estimands-cloud.png',
                  width = '100%'),
            ),
            "BART allows for robust estimation of a wide variety of estimands. Learn how they differ and how to choose one."
          )
        ),
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Post treatment variables',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_post_treatment_img'),
              img(src = 'www/img/thumbnails/post-treatment.png',
                  width = '100%'),
            ),
            "Post-treatment variables are a class of variables that can be affected by the treatment and should be removed prior to modeling. Learn how to identify them to make sure you are not biasing your treatment effect estimates."
          )
        ),
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Potential outcomes',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_potential_outcomes_img'),
              img(src = 'www/img/thumbnails/potential-outcomes.png',
                  width = '100%'),
            ),
            "The potential outcomes framework is a methodology to estimate causal effects. Learn the theory foundations here."
          )
        )
      ),

      fluidRow(
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Randomization',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_randomization_img'),
              img(src = 'www/img/thumbnails/randomization.png',
                  width = '100%'),
            ),
            "Coming soon"
            # "Randomization balances groups on both observed and unobserved characteristics. Learn how this mechanism is exploited for causal inference."
          )
        ),
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Bias and efficiency',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_balance_img'),
              img(src = 'www/img/thumbnails/balance.png',
                  width = '100%'),
            ),
            "Coming soon"
          )
        ),
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Bayesian Additive Regression Trees',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_bart_img'),
              img(src = 'www/img/thumbnails/decision_tree.png',
                  width = '100%'),
            ),
            "Coming soon"
          )
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

    # links from learn home page to each learn article
    selectors <- c('learn_estimands', 'learn_post_treatment', 'learn_potential_outcomes')
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
