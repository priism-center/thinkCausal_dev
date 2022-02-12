
#' Build the scaffolding for a new learning module
#'
#' Creates the sub
#'
#' @param article_name The title of the article
#' @param short_name A short, unique name for the article to be used for R functions. If not provided, then created from `article_name`.
#' @param directory_name The name of the new directory to be created. If not provided, then created from `article_name`.
#' @param include_quiz One of c('no', 'embed', 'at launch')
#'
#' @return
#' @export
#'
#' @examples
create_learning_module <- function(article_name = 'Lorem ipsum', short_name, directory_name, include_quiz = 'no'){
  
  match.arg(include_quiz, c('no', 'embed', 'at launch'))
  
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
  
  # TODO: replace all <template> with short_name


  # TODO: quiz
  # if (include_quiz != 'no'){ comment out line; change function call}
  # if (include_quiz == 'embed'))
  # if (include_quiz == 'at launch'))
  
  
  # TODO: change file names
  
  
  # message
  cli::cli_alert_success("Learning module '{article_name}' created in {path_to_modules}/{directory_name}")
  
  # reminders about where to put ids, how to update ui and server
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
}

# full_dir <- file.path('modules/learning/_template')
# moduleR <- readLines(file.path(full_dir, 'template_module.R'))
# conn <- file(file.path(full_dir, 'test.R'))
# writeLines(moduleR, conn)
# close(conn)

# https://github.com/JohnCoene/waiter