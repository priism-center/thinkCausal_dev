script_page <- tabPanel(
  title = "Script",
  style = "padding-left: 3rem;",
  br(),
  downloadButton(
    outputId = 'analysis_results_button_download',
    label = 'Download script',
    style = 'max-width: 300px'
  ),
  br(), br()
)