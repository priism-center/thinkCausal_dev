#' settings_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_options_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Plot settings',
         radioButtons(
           inputId = ns("ggplot_theme"),
           label = "Plot aesthetic",
           choices = c("Minimal", "Simple", "Classic", "Gray")
         ),
         br(),
         sliderInput(
           inputId = ns("ggplot_text_size"),
           label = "Plot font size",
           min = 8,
           max = 20,
           value = 14,
           step = 1
         ),
         br(),
         sliderInput(
           inputId = ns("ggplot_point_size"),
           label = "Plot point size",
           min = 1,
           max = 10,
           value = 3,
           step = 1
         ),
         br(),
         numericInput(
           inputId = ns('ggplot_height'),
           label = 'Plot download height',
           min = 1,
           max = 15,
           value = 8,
           step = 1
         ),
         numericInput(
           inputId = ns('ggplot_width'),
           label = 'Plot download width',
           min = 1,
           max = 15,
           value = 8,
           step = 1
         )
         # br(),
         # radioButtons(
         #   inputId = "ggplot_color",
         #   label = "Plot colors",
         #   choices = c("Color blind friendly", "Standard", "Grayscale")
         # ),
      ),
      bs4Dash::box(
        width = 6,
        collapsible = FALSE,
        title = 'Example plot',
        plotOutput(outputId = ns('ggplot_preview'),
                   height = '400px')
      )
    )

  )
}

#' settings_options Server Functions
#'
#' @noRd
#' @import ggplot2 dplyr
mod_settings_options_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # change plot theme, font size, and point size
    theme_custom <- reactive({

      # change theme
      theme_custom <- switch(
        input$ggplot_theme,
        "Minimal" = theme_minimal_no_transparency,
        "Simple" = ggplot2::theme_bw,
        "Classic" = ggplot2::theme_classic,
        "Gray" = ggplot2::theme_gray
      )

      # change point and font size
      update_geom_defaults("point", list(size = input$ggplot_point_size))
      theme_custom <- theme_custom(base_size = input$ggplot_text_size)

      # change colors
      # theme_custom <- theme_custom %+replace% ggplot2::scale_color_brewer

      # store it (this gets passed to the modules)
      store$options$theme_custom <- theme_custom

      return(theme_custom)
    })

    # update plot theme preview
    output$ggplot_preview <- renderPlot({

      # create dummy plot
      p <- ggplot(
        tibble(x = c(-19.0, 10.3, 8.4, 0.3, -1.8, 11.7, 9.6, 7.5, -13.0, 2.3),
               y = c(2.1, -7.5, 0.9, 2.8, -0.8, -1.2, 6.7, 8.1, 4.0, 18.9),
               shape = rep(LETTERS[1:5], 2)),
        aes(x = x, y = y, color = x, shape = shape)) +
        geom_point() +
        labs(title = "thinkCausal",
             color = 'color')

      # add theme
      p <- p + theme_custom()

      return(p)
    })

    # store plot height and width settings
    observeEvent(input$ggplot_height, {
      store$options$settings_options_ggplotHeight <- isolate(input$ggplot_height)
    })
    observeEvent(input$ggplot_width, {
      store$options$settings_options_ggplotWidth <- isolate(input$ggplot_width)
    })

    return(store)
  })
}

## To be copied in the UI
# mod_settings_options_ui("settings_options_1")

## To be copied in the server
# mod_settings_options_server("settings_options_1")
