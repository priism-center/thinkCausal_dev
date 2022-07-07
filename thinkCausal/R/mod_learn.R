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

    div(id = 'conceptsGrid1',
        class = 'conceptsGrid',

        fluidRow(
          bs4Dash::box(
            width = 4,
            tagList(
              img(src = 'www/img/thumbnails/estimands-cloud.png',
                  width = '90%'),
              h3('Causal estimands'),
              "BART allows for robust estimation of a wide variety of estimands. Learn how they differ and how to choose one."
            ),
            collapsible = FALSE
          ),
          bs4Dash::box(
            width = 4,
            tagList(
              img(src = 'www/img/thumbnails/post-treatment.png',
                  width = '90%'),
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
          # create_learning_card(
          #   page_id = 'concepts_link_causal_estimands',
          #   thumbnail_url = 'estimands-cloud.png',
          #   title = "Causal estimands",
          #   description = "BART allows for robust estimation of a wide variety of estimands. Learn how they differ and how to choose one."
          # ),
          # create_learning_card(
          #   page_id = 'concepts_link_post_treatment_variables',
          #   thumbnail_url = 'post-treatment.png',
          #   title = "Post-treatment variables",
          #   description = "Post-treatment variables are a class of variables that can be affected by the treatment and should be removed prior to modeling. Learn how to identify them to make sure you are not biasing your treatment effect estimates."
          # ),
          # create_learning_card(
          #   page_id = 'concepts_link_potential_outcomes',
          #   thumbnail_url = 'potential-outcomes.png',
          #   title = "Potential outcomes",
          #   description = "The potential outcomes framework is a methodology to estimate causal effects. Learn the theory foundations here."
          # )
        ),

        fluidRow(
          create_learning_card(
            page_id = 'concepts_link_randomization',
            thumbnail_url = 'randomization.png',
            title = "Randomization",
            description = "Randomization balances groups on both observed and unobserved characteristics. Learn how this mechanism is exploited for causal inference."
          ),
          create_learning_card(
            page_id = 'concepts_link_bias_and_efficiency',
            thumbnail_url = 'balance.png',
            title = "Bias and efficiency",
            description = "Coming soon"
          ),
          create_learning_card(
            page_id = 'concepts_link_lorem_ipsum',
            thumbnail_url = 'decision_tree.png',
            title = "Bayesian Additive Regression Trees",
            description = "Coming soon"
          )
        )
    )

  )
}

#' learn Server Functions
#'
#' @noRd
mod_learn_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_learn_ui("learn_1")

## To be copied in the server
# mod_learn_server("learn_1")
