options_page <- tabPanel(
  title = "Options",
  style = "padding-left: 3rem;",
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
  )
)
