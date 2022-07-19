#' reproduce
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# extract source code of a local function
extract_code <- function(.function){
  func <- eval(parse(text = .function))
  .code <- attributes(attributes(func)$srcref)$srcfile$lines
  return(.code)
}

# write individual files containing each functions code
write_function_files <- function(files, .functions){
  for (func in .functions){
    filename <- paste0('R/', func, ".R")
    functionFile <- file(filename)
    writeLines(extract_code(func), functionFile)
    close(functionFile)
    files <- c(filename, files)
  }
  return(files)
}

edit_reproducible_script <- function(files, store){
  script <- readLines('thinkCausal_script.R')

  # add filename
  script <- add_data_file_(script, store)

  script <- remove_section_(script, section = 'test')

  # write out the now edited script
  file <- file("thinkCausal_script.R")
  writeLines(script, file)
  close(file)
}

add_data_file_ <- function(script, store){

  filename <- file.path('inputs', store$analysis$data$filename)

  # replace file name
  # i <- which(script == '#!! file <- filename')
  # script[i] <- glue::glue('file <- "{filename}"')

  # adjust data reading code
  # TODO: error in here
  all_file_types <- c('csv', 'dta', 'xlsx', 'txt', 'sav')
  filetype <- tools::file_ext(filename)
  i <- which(stringr::str_detect(script, glue::glue("#!! {filetype}")))
  script[i] <- stringr::str_remove(script[i], glue::glue('^(#!! {filetype} )'))
  script[i] <- stringr::str_replace(script[i], 'file', glue::glue("'{filename}'"))
  tryCatch(script[i] <- stringr::str_replace(script[i], 'my_delim', as.character(store$analysis$data$delim)), error = function(e) NULL)
  tryCatch(script[i] <- stringr::str_replace(script[i], 'my_col_names', as.character(store$analysis$data$header)), error = function(e) NULL)

  # delete other data reading lines
  other_file_types <- setdiff(all_file_types, filetype)
  i_other <- purrr::map_int(glue::glue("#!! {other_file_types}"),
                        ~stringr::str_which(script, .x))
  script <- script[-i_other]

  return(script)
}



remove_section_ <- function(script, section){
  if (section == 'test') script_edited <- script[-2]

  return(script_edited)
}
