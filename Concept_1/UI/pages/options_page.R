options_page <- tabPanel(
  title = "Options",
  style = "padding-left: 3rem;",
  radioButtons(
    inputId = "settings_options_ggplot_theme",
    label = "Plot themes",
    choiceNames = c("Minimal", "Simple", "Classic", "Gray"),
    choiceValues = c('theme_minimal()', 'theme_bw()', 'theme_classic()', 'theme_gray()')
  ),
  radioButtons(
    inputId = "settings_options_temp",
    label = "Another set of inputs",
    choiceNames = c("Option1", "Option2", "Option3", "Option4"),
    choiceValues = c(theme_minimal, theme_bw, theme_classic, theme_gray)
  )
)
