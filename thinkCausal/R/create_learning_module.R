
#' Build the scaffolding for a new learning module
#'
#' Creates the a new learning module from the /modules/learning/_template/ and reminds you of the steps to take to update the app to make it integrate in thinkCausal.
#'
#' @param article_name The title of the article
#' @param include_quiz TRUE/FALSE. Quiz is embedded by default but can be adjusted within the created _module.R file.
#' @param short_name A short, unique name for the article to be used for R functions. If not provided, then created from `article_name`.
#' @param directory_name The name of the new directory to be created. If not provided, then created from `article_name`.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' create_learning_module('My new article')
#' }
create_learning_module <- function(article_name = 'Lorem ipsum', include_quiz = TRUE, short_name, directory_name){
  
  # checks for directory name
  if (!stringr::str_detect(getwd(), "/thinkCausal$")) stop('Working directory must be the /thinkCausal subdirectory')
  if (missing(directory_name)){
    directory_name <- paste0(tolower(gsub('_| ', '-', article_name)))
    directory_name <- tolower(directory_name)
  }
  if (directory_name == '_template') stop('directory_name cannot be "_template"')
  path_to_modules <- file.path('modules', 'learning')
  full_dir <- file.path(path_to_modules, directory_name)
  if (dir.exists(full_dir)) stop('directory already exists: please choose a new directory_name')
  
  # copy template
  dir.create(full_dir)
  file.copy(
    from = list.files(file.path(path_to_modules, '_template'), full.names = TRUE),
    to = full_dir,
    recursive = TRUE,
    copy.mode = TRUE
  )
  
  # TODO: validate short_name
  if (missing(short_name)){
    short_name <- paste0(tolower(gsub('-| ', '_', article_name)))
    short_name <- tolower(short_name)
  }
  
  # replace text at top of module
  path_to_module <- file.path(full_dir, 'template_module.R')
  replace_template_text(path_to_module,
                        "# this defines the template for a learning module",
                        glue::glue("# this defines the {short_name} module"))
  replace_template_text(path_to_module, 
                        "# `create_learning_module` function creates new modules based on this template",
                        glue::glue("# this script was created by `create_learning_module()`"))
  replace_template_text(path_to_module,
                        '# any text "template" is replaced with the new module short_name',
                        glue::glue("# the article name is '{article_name}' and the short name is '{short_name}'"))
  
  
  # replace all <template> with short_name
  replace_template_text(file.path(full_dir, 'R', 'template_quiz.R'))
  replace_template_text(file.path(full_dir, 'template_module.R'))
  replace_file_names(full_dir)
  
  # if no quiz then comment out the correct lines
  if (isFALSE(include_quiz)) comment_out_quiz(file.path(full_dir, glue::glue("{short_name}_module.R")))

  # messages and reminders
  cli::cli_alert_success("Learning module '{article_name}' created in {path_to_modules}/{directory_name}")
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_h1('{.emph Did you remember to?}')
  cli::cli_h3('Update global.R?')
  cli::cli_alert_warning('Add {short_name} = "learning_{short_name}" to the module_ids list')
  cli::cli_h3('Update server.R?')
  cli::cli_alert_warning('Add "server_learning_{short_name}(id = isolate(store$module_ids$learning${short_name}), ...)"')
  cli::cli_alert_warning('Add "{article_name}" to tab_titles')
  cli::cli_h3('Update /UI/headers/concepts_header.R?')
  cli::cli_alert_warning('Add "tabPanel(title = \'{article_name}\', ui_learning_{short_name}(id = module_ids$learning${short_name}))"')
  cli::cli_h3('Update /UI/concepts_page.R?')
  link <- paste0("concepts_link_", tolower(gsub('-| ', '_', article_name)))
  cli::cli_alert_warning("Add a new column() containing actionLink('{link}', img(src = 'thumbnails/{short_name}.png')")
  cli::cli_h3('Other items to consider')
  cli::cli_alert_warning("Add a thumbnail image '{short_name}.png' to www/thumbnails")
  cli::cli_alert_warning("Store your R functions in {file.path(full_dir, 'R')} subdirectory")
  cli::cli_alert_warning("Store your data in {file.path(full_dir, 'data')} subdirectory")
}

replace_template_text <- function(file_path, text_to_replace = 'template', replacement_text = short_name){
  file_orig <- readLines(file_path)
  file_new <- stringr::str_replace_all(file_orig, text_to_replace, replacement_text)
  conn <- file(file_path)
  writeLines(file_new, conn)
  close(conn)
}

replace_file_names <- function(file_path, text_to_replace = 'template', replacement_text = short_name){
  names_old <- list.files(file_path, recursive = TRUE)
  names_new <- stringr::str_replace_all(names_old, text_to_replace, replacement_text)
  
  names_old <- file.path(file_path, names_old)
  names_new <- file.path(file_path, names_new)
  purrr::walk2(names_old, names_new, ~file.rename(.x, .y))
}

comment_out_quiz <- function(path_to_module){
  moduleR <- readLines(path_to_module)
  
  moduleR <- stringr::str_replace(moduleR, 
                                  stringr::fixed("ui_quiz(id = ns('quiz'))"), 
                                  "# ui_quiz(id = ns('quiz'))")
  # quiz_start <- which(stringr::str_detect(moduleR, '# run the quiz'))
  # quiz_end <- quiz_start - 1 + which(stringr::str_detect(moduleR[quiz_start:length(moduleR)], "\\)$"))[1]
  # moduleR[quiz_start:quiz_end] <- paste0("# ",  moduleR[quiz_start:quiz_end])

  # write out
  conn <- file(path_to_module)
  writeLines(moduleR, conn)
  close(conn)
}

# https://github.com/JohnCoene/waiter