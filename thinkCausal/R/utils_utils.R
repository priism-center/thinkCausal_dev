`%notin%` <- Negate(`%in%`)

coerce_to_logical <- function(x){
  out <- NA
  out[x %in% c(1, '1', TRUE, T, 'TRUE', 'T', 'true', 't')] <- TRUE
  out[x %in% c(0, '0', FALSE, F, 'FALSE', 'F', 'false', 'f')] <- FALSE
  return(out)
}

is_categorical <- function(x){
  is_int <- isTRUE(clean_detect_integers(x))
  is_binary <- isTRUE(clean_detect_logical(x))
  is_cont <- isTRUE(is.double(x))
  is_cat <- !any(is_int, is_binary, is_cont)
  return(is_cat)
}

is_cat_or_logical <- function(x){
  is_log <- isTRUE(clean_detect_logical(x))
  is_cat <- isTRUE(is_categorical(x))
  is_either <- is_log | is_cat
  return(is_either)
}

# extract source code of a local function
extract_code <- function(.function){
  func <- eval(parse(text = .function))
  .code <- attributes(attributes(func)$srcref)$srcfile$lines
  return(.code)
}

# write individual files containing each functions code
write_function_files <- function(files, .functions){
  for (func in .functions){
    filename <- paste0(func, ".R")
    functionFile <- file(filename)
    writeLines(extract_code(func), functionFile)
    close(functionFile)
    files <- c(filename, files)
  }
  return(files)
}
