# TODO: convert to https://codepen.io/william-goldsworthy/pen/JzVajj? doesn't work great in practice

progress_footer <- tagList(
  tags$div(
    id = 'progress-footer',
    tags$div(
      class = 'left-align',
      a(href="https://steinhardt.nyu.edu/priism", "New York University")
    ),
    purrr::map2(module_ids$analysis, seq_along(module_ids$analysis), function(tab, i){
      # display_name <- glue::glue('{i}. {stringr::str_to_sentence(names(module_ids$analysis)[i])}')
      display_name <- stringr::str_to_sentence(names(module_ids$analysis)[i])
      if (i < length(module_ids$analysis)) display_name <- glue::glue('{display_name} &nbsp ➙') #→ ➝ ➔ ➔
      tags$div(
        id = glue::glue('progress-footer-{tab}'),
        class = 'progress-footer-tab',
        onclick = glue::glue('go_to_shiny_page("{tab}")'),
        htmltools::HTML(display_name)
      )
    }),
    tags$div(
      class = 'right-align',
      a(onclick = 'openHelp()', style = 'cursor: pointer', "Help")
    )
  )
)
