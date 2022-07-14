#' learn_estimands UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_estimands_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(
      class = 'learning-page',

      # load custom css
      # includeCSS(file.path('www', 'learning', 'estimands', 'css', 'estimands.css')),

      # load custom javascript
      # tags$script(src = file.path('js', 'libraries', 'd3.v5.js')),
      # tags$script(src = file.path('js', 'libraries', 'jstat.min.js')),

      # tags$script(src = file.path('learning', 'estimands', 'js', 'namespace.js')),
      # tags$script(src = file.path('learning', 'estimands', 'js', 'helpers.js')),
      # tags$script(src = file.path('learning', 'estimands', 'js', 'scrollPlot.js')),
      # tags$script(src = file.path('learning', 'estimands', 'js', 'buildTable.js')),
      # tags$script(src = file.path('learning', 'estimands', 'js', 'buildPlot.js')),
      # tags$script(src = file.path('learning', 'estimands', 'js', 'init.js')),

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        # includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_1.md')),
        br(),br(),br(),br(),br(),br(),
      ),

      div(
        class = 'scrollytell-container',

        # text content
        div(
          class = 'estimands-text-along-d3',
          # includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_ate.md'))
        ),

        # d3js content
        div(id = 'estimands-plot-container', # TODO: should prefix the div ids
            class = 'estimands-d3-container',
            div(id = 'estimands-plot-ATE')
        )
      ),

      br(),br(),br(),br(),br(),

      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        br(),br(),br(),
        # includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_2.md')),
        br()
      ),

      # the quiz UI
      # ui_quiz(id = ns('quiz')),

      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        class = ns('learning-content-blur'), # required for blur
        class = 'learning-content-blur', # required for blur
        br(),
        # includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_attatc.md')),
        br(),br(),br(),br(),br(),
        # wellPanel(includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_related.md'))),
        # includeMarkdown(file.path(store_l_estimands$path_to_here, "markdowns", 'estimands_citations.md'))
      )
    )
  )
}

#' learn_estimands Server Functions
#'
#' @noRd
mod_learn_estimands_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_learn_estimands_ui("learn_estimands_1")

## To be copied in the server
# mod_learn_estimands_server("learn_estimands_1")
