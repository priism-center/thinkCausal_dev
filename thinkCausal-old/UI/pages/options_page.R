options_page <- tabPanel(
  title = "Options",
  style = "padding-left: 3rem;",
  fluidRow(
    column(width = 6,
           h3("Plot settings"),
           radioButtons(
             inputId = "settings_options_ggplotTheme",
             label = "Plot aesthetic",
             choices = c("Minimal", "Simple", "Classic", "Gray")
           ),
           br(),
           sliderInput(
             inputId = "settings_options_ggplotTextSize",
             label = "Plot font size",
             min = 8,
             max = 20,
             value = 14,
             step = 1
           ),
           br(),
           sliderInput(
             inputId = "settings_options_ggplotPointSize",
             label = "Plot point size",
             min = 1,
             max = 10,
             value = 3,
             step = 1
           ),
           br(),
           numericInput(
             inputId = 'settings_options_ggplotHeight',
             label = 'Plot download height',
             min = 1,
             max = 15,
             value = 8,
             step = 1
           ),
           numericInput(
             inputId = 'settings_options_ggplotWidth',
             label = 'Plot download width',
             min = 1,
             max = 15,
             value = 8,
             step = 1
           )
           # br(),
           # radioButtons(
           #   inputId = "settings_options_ggplot_color",
           #   label = "Plot colors",
           #   choices = c("Color blind friendly", "Standard", "Grayscale")
           # ),
          ),
    column(width = 6,
           br(),
           plotOutput(outputId = 'settings_options_ggplot_preview',
                      height = '400px')
    )
  )
  # br(),br(),
  # hr(style = "height: 2px; border-width: 0; background-color: #e3e3e3; max-width: 400px; margin-left: 0;"),
  # h3("More"),
  # checkboxGroupInput(
  #   inputId = "temp",
  #   label = "Another example...",
  #   choices = LETTERS[1:5],
  #   selected = LETTERS[3]
  # ),
  # br(),br(),
  # hr(style = "height: 2px; border-width: 0; background-color: #e3e3e3; max-width: 400px; margin-left: 0;"),
  # h3("Other"),
  # sliderInput(
  #   inputId = "temp2",
  #   label = "Another example...",
  #   min = 1,
  #   max = 10,
  #   value = 5,
  #   step = 1
  # )
)
