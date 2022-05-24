# TODO: convert to https://codepen.io/william-goldsworthy/pen/JzVajj

progress_footer <- tagList(
  tags$div(
    id = 'progress-footer',
    tags$div(
      class = 'left-align',
      a(href="https://steinhardt.nyu.edu/priism", "New York University")
    ),
    tags$div(
     id = paste0('progress-footer-', module_ids$analysis$design),
     class = 'progress-footer-tab',
     onclick = paste0("go_to_shiny_page('", module_ids$analysis$design, "')"),
     '1. Design'
    ),
    tags$div(
      id = paste0('progress-footer-', module_ids$analysis$data),
      class = 'progress-footer-tab',
      onclick = paste0("go_to_shiny_page('", module_ids$analysis$data, "')"),
      '2. Data'
    ),
    tags$div(
      id = paste0('progress-footer-', module_ids$analysis$eda),
      class = 'progress-footer-tab',
      onclick = paste0("go_to_shiny_page('", module_ids$analysis$eda, "')"),
      '3. EDA'
    ),
    tags$div(
      id = paste0('progress-footer-', module_ids$analysis$model),
      class = 'progress-footer-tab',
      onclick = paste0("go_to_shiny_page('", module_ids$analysis$model, "')"),
      '4. Model'
    ),
    tags$div(
      id = paste0('progress-footer-', module_ids$analysis$diagnostic),
      class = 'progress-footer-tab',
      onclick = paste0("go_to_shiny_page('", module_ids$analysis$diagnostic, "')"),
      '5. Diagnostics'
    ),
    tags$div(
      id = paste0('progress-footer-', module_ids$analysis$results),
      class = 'progress-footer-tab',
      onclick = paste0("go_to_shiny_page('", module_ids$analysis$results, "')"),
      '6. Results'
    ),
    tags$div(
      id = paste0('progress-footer-', module_ids$analysis$subgroup),
      class = 'progress-footer-tab',
      onclick = paste0("go_to_shiny_page('", module_ids$analysis$subgroup, "')"),
      '7. Subgroup'
    ),
    tags$div(
      class = 'right-align',
      a(onclick = 'openHelp()', style = 'cursor: pointer', "Help")
    )
  )
)
