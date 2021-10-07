log_page <- tabPanel(
  title = "Log",
  style = "padding-left: 3rem;",
  br(),
  downloadButton(
    outputId = 'settings_log_download',
    label = 'Download log',
    style = 'max-width: 300px'
  ),
  br(), br(), 
  verbatimTextOutput(
    outputId = 'settings_log_text'
  )
)