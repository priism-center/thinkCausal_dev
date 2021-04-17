options_page <- tabPanel(
  title = "Options",
  style = "padding-left: 3rem;",
  fluidRow(
    column(width = 6,
           h3("Plot settings"),
           radioButtons(
             inputId = "settings_options_ggplot_theme",
             label = "Plot aesthetic",
             choices = c("Minimal", "Simple", "Classic", "Gray")
             # choiceValues = c('theme_minimal()', 'theme_bw()', 'theme_classic()', 'theme_gray()')
           ),
           br(),
           sliderInput(
             inputId = "settings_options_ggplot_size",
             label = "Plot font size",
             min = 8,
             max = 20,
             value = 14,
             step = 1
           ),
          ),
    column(width = 6,
           br(),
           plotOutput(outputId = 'settings_options_ggplot_preview',
                      height = '300px')
    )
  ),
  br(),br(),
  hr(style = "height: 2px; border-width: 0; background-color: #e3e3e3; max-width: 400px; margin-left: 0;"),
  h3("More"),
  checkboxGroupInput(
    inputId = "temp",
    label = "Another example...",
    choices = LETTERS[1:5],
    selected = LETTERS[3]
  ),
  br(),br(),
  hr(style = "height: 2px; border-width: 0; background-color: #e3e3e3; max-width: 400px; margin-left: 0;"),
  h3("Other"),
  sliderInput(
    inputId = "temp2",
    label = "Another example...",
    min = 1,
    max = 10,
    value = 5,
    step = 1
  )
)
