# TODO: convert to https://codepen.io/william-goldsworthy/pen/JzVajj

progress_footer <- tagList(
  tags$div(
    id = 'progress-footer',
    tags$div(
      class = 'left-align',
      a(href="https://steinhardt.nyu.edu/priism", "New York University")
    ),
    purrr::map2(module_ids$analysis, seq_along(module_ids$analysis), function(tab, i){
      display_name <- glue::glue('{i}. {stringr::str_to_sentence(names(module_ids$analysis)[i])}')
      tags$div(
        id = paste0('progress-footer-', tab),
        class = 'progress-footer-tab',
        onclick = paste0("go_to_shiny_page('", tab, "')"),
        display_name
      )
    }),
    tags$div(
      class = 'right-align',
      a(onclick = 'openHelp()', style = 'cursor: pointer', "Help")
    )
  )
)
