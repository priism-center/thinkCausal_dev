log_page <- tabPanel(
  title = "Log",
  style = "padding-left: 3rem;",
  downloadButton(
    outputId = 'settins_log_download',
    label = 'Download log',
    style = 'max-width: 300px'
  ),
  br(), br(), 
  h4("Log: "),
  verbatimTextOutput(
    outputId = 'settings_log_text'
  )
)