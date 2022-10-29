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
    # shinyjs::useShinyjs(),

    div(
      class = 'learn-grid',
      fluidRow(
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
        ),
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
        )
      ),

      fluidRow(
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Randomized experiments',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_randomization_img'),
              img(src = 'www/img/thumbnails/randomization.png',
                  width = '100%'),
            ),
            "Randomization balances groups on both observed and unobserved characteristics. Learn how this mechanism is exploited for causal inference."
          )
        ),
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Observational studies',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_observational_img'),
              img(src = 'www/img/thumbnails/observational.png',
                  width = '100%'),
            ),
            'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut enim ad minim veniam, quis nostrud exercitation.'
          )
        ),
        bs4Dash::box(
          width = 4,
          collapsible = FALSE,
          title = 'Coming soon: Bias and efficiency',
          class = 'learning-content-blur',
          tagList(
            shiny::actionLink(
              inputId = ns('learn_balance_img'),
              img(src = 'www/img/thumbnails/balance.png',
                  width = '100%'),
            ),
            'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua'
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
    selectors <- c(
      'learn_estimands',
      'learn_post_treatment',
      'learn_potential_outcomes',
      'learn_randomization',
      'learn_observational'
    )
    purrr::map(selectors, function(sel){
      observeEvent(input[[glue::glue('{sel}_img')]], {
        bs4Dash::updateTabItems(store$session_global, inputId = 'sidebar', selected = sel)
      })
    })

  })
}
