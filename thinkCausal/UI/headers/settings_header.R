settings_header <- tabPanel(
  title = icon('gear', verify_fa = FALSE), 
  value = 'Settings',
  navlistPanel(
    widths = c(2, 10),
    options_page,
    contact_page,
    software_page
    # terms_page
  )
)
