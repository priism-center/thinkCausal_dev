#' learn_variable_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_variable_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
bs4Dash::tabsetPanel(
            id = ns('variable_selection_tabs'),
           type = 'hidden',
            tabPanel(title = 'Variable Selection',
                     div(class = 'learning-page',
                         div(
                           class = ns('learning-content'), # required
                           class = 'learning-content',  # required
                           style = 'display: block;',
                           includeMarkdown(app_sys("app", "www", "learn", "variable_selection", "markdowns", 'intro.md')),
                           br(),
                     fluidRow(
                       bs4Dash::box(
                         width = 6,
                         collapsible = FALSE,
                         title = 'Co-linearity',
                         tagList(
                           shiny::actionLink(
                             inputId = ns('learn_colinearity_img'),
                             img(src = 'www/img/thumbnails/potential-outcomes.png',
                                 width = '100%'),
                           ),
                           "How I learned to stop worrying about colinearity and include all variables that predict the outcome"
                         )),
                       bs4Dash::box(
                         width = 6,
                         collapsible = FALSE,
                         title = 'Overfitting',
                         tagList(
                           shiny::actionLink(
                             inputId = ns('learn_potential_outcomes_img'),
                             img(src = 'www/img/thumbnails/potential-outcomes.png',
                                 width = '100%'),
                           ),
                           "Learn about how thinkCausal automatically protects against overfitting."
                         )
                       )
                     )

                         )
                     )

            ),
            tabPanelBody(
                         value = 'learn_colinearity',
                         mod_learn_colinearity_ui(id = ns('learn_colinearity'))
                     ),
            tabPanelBody(value = 'learn_potential_outcomes')
    )
  )
}

#' learn_variable_selection Server Functions
#'
#' @noRd
mod_learn_variable_selection_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$learn_colinearity_img, {
     updateTabsetPanel(
        inputId = "variable_selection_tabs",
        selected = "learn_colinearity"
      )

      mod_learn_colinearity_server(id = 'learn_colinearity')
    })


#
#     selectors <- c(
#       'learn_colinearity',
#       'learn_potential_outcomes'
#     )
#     purrr::map(selectors, function(sel){
#       observeEvent(input[[glue::glue('{sel}_img')]], {
#         bs4Dash::updatebs4TabItems(store$session_global, inputId = 'sidebar', selected = sel)
#       })
#     })

  })
}

## To be copied in the UI
# mod_learn_variable_selection_ui("learn_variable_selection_1")

## To be copied in the server
# mod_learn_variable_selection_server("learn_variable_selection_1")
