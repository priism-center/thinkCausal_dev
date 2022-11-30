#' @title Clean a string for use in column names
#'
#' @description {Cleans up strings for use as column names in dataframes. Makes the following changes:
#' \itemize{
#'  \item replaces non-ASCII characters with ASCII counterparts
#'  \item replaces spaces with underscores
#'  \item replaces percent sign with '_percent'
#'  \item removes all punctuation except underscores and periods
#'  \item adds 'n' to the beginning of strings that start with numeric characters
#' }
#' }
#'
#' @param .names
#'
#' @author Joe Marlo
#'
#' @return character vector
#' @noRd
#'
#' @importFrom stringr str_replace_all
#' @examples
#' .names <- c("yes", "TRUE", "nope%", "98", 'Ábcdêãçoàúü', 'yep_-,.yep', 'hello goodbye', '', 'no', 'no')
#' clean_names(.names)
clean_names <- function(.names){

  .names <- as.character(.names)
  if (!isTRUE(is.character(.names))) stop(".names must be a character vector")

  # remove non-ASCII characters
  .names <- base::iconv(.names, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  # remove any whitespace
  .names <- stringr::str_trim(.names, 'both')

  # replace spaces with underscore
  .names <- stringr::str_replace_all(string = .names, pattern = ' ', replacement = '_')

  # replace % with 'percent' depending on its location
  .names <- stringr::str_replace_all(string = .names, pattern = '%$', replacement = "_percent")
  .names <- stringr::str_replace_all(string = .names, pattern = '^%', replacement = "percent_")
  .names <- stringr::str_replace_all(string = .names, pattern = '%', replacement = "_percent_")

  # remove punctuation except underscore and period
  pat <- "(?![._])[[:punct:]]"
  .names <- stringr::str_remove_all(string = .names, pattern = pat)

  # remove ^~`$
  .names <- stringr::str_remove_all(string = .names, pattern = "[\\^]")
  .names <- stringr::str_remove_all(string = .names, pattern = "[\\~]")
  .names <- stringr::str_remove_all(string = .names, pattern = "[\\`]")
  .names <- stringr::str_remove_all(string = .names, pattern = "[$]")

  # remove leading special characters
  .names <- stringr::str_remove_all(string = .names, pattern = "^[:punct:]")

  # add 'n' before numbers (names can't start with numbers)
  .names <- as.character(sapply(.names, function(string){
    is_first_num <- base::substr(string, 1, 1) %in% 0:9
    if (isTRUE(is_first_num)) string <- paste0('n', string)
    return(string)
  }))

  # replace blanks with 'BLANK'
  .names[.names == ''] <- "BLANK"

  # add trailing numbers to duplicate names
  for (name in .names){
    these_names <- .names[.names == name]
    if (length(these_names) > 1){
      new_names <- paste0(these_names, paste0("_", seq_along(these_names)))
      .names[.names == name] <- new_names
    }
  }

  return(.names)
}

#' @title Convert all psuedo-logical columns in a dataframe to booleans
#'
#' @description Converts columns of a dataframe containing binary c(0, 1), c("T", "F"), c("True", "False") to boolean c(TRUE, FALSE). Is agnostic to case.
#'
#' @param input_data dataframe
#'
#' @author Joe Marlo
#'
#' @return dataframe
#' @noRd
#'
#' @importFrom readr parse_logical
#'
#' @examples
#' x <- data.frame(
#'   zero_one = sample(0:1, 10, replace = TRUE),
#'   TF = sample(c("T", "F"), 10, replace = TRUE),
#'   truefalse = sample(c('true', 'false'), 10, replace = TRUE),
#'   char = sample(LETTERS, 10),
#'   yn = sample(c("yes", "no"), 10, replace = TRUE)
#'   )
#' clean_auto_convert_logicals(x)
clean_auto_convert_logicals <- function(input_data){
  # function converts columns of 0:1, T:F, True:False to logicals

  for (col in colnames(input_data)){
    is_in_list <- clean_detect_logical(input_data[[col]])

    # convert column to logical
    if (isTRUE(is_in_list)){
      col_as_char <- as.character(input_data[[col]])
      input_data[,col] <- readr::parse_logical(col_as_char)
    }
  }

  return(input_data)
}

clean_detect_logical <- function(x){

  if(inherits(x, 'data.frame')) stop('x cannot be a dataframe')

  # is x exclusively in list of pre-determined
  inclusion_list <- c(0, 1, 't', 'f', 'true', 'false')
  x_as_char <- as.character(x)
  x_cleaned <- base::tolower(unique(x_as_char))
  is_in_list <- length(setdiff(x_cleaned, inclusion_list)) == 0
  return(is_in_list)
}

#' @title Convert integer-like columns with few levels to a factor
#'
#' @description Useful for plotting
#'
#' @param .data a dataframe
#'
#' @return a vector of class factor
#' @noRd
#'
#' @examples
#' x <- data.frame(
#'   zero_one = sample(0:1, 10, replace = TRUE),
#'   integers = sample(0:10, 10, replace = TRUE),
#'   runif = runif(10),
#'   TF = sample(c("T", "F"), 10, replace = TRUE),
#'   char = sample(LETTERS, 10)
#'  )
#'  str(clean_auto_convert_integers(x))
clean_auto_convert_integers <- function(.data){

  for (col in colnames(.data)){
    is_few_integers <- clean_detect_integers(.data[[col]])

    # convert column to categorical
    if (isTRUE(is_few_integers)){
      .data[[col]] <- factor(.data[[col]],
                             levels = sort(unique(.data[[col]])))
    }
  }

  return(.data)
}

#' @describeIn clean_auto_convert_integers detect if a vector is integers
clean_detect_integers <- function(x, n_levels_threshold = 15){

  if(inherits(x, 'data.frame')) stop('x cannot be a dataframe')

  # does x match its self coerced to an integer
  is_integer <- tryCatch(all.equal(x, as.integer(x)),
                         error = function(e) FALSE)

  if (isTRUE(is_integer)){
    n_levels <- dplyr::n_distinct(x)
    if (n_levels <= n_levels_threshold) return(TRUE)
  }

  return(FALSE)
}

#' Attempt to detect which columns of a dataframe are an ID column
#'
#' ID detection is based on column name and the values. If none are detected, then columns are categorized as 'X'
#'
#' @param .data a dataframe
#'
#' @author Joe Marlo & George Perrett
#'
#' @return a list denoting which column names is an column ID
#' @noRd
#'
#' @examples
#' .data <- data.frame(
#'  treatment = c(TRUE, TRUE, FALSE, TRUE, FALSE),
#'  rsp = c(0.808, 0.296,-1.579,-1.272, 0.627),
#'  ID = c(0.808, 0.296,-1.579,-1.272, 0.627),
#'  dummyID = 1:5,
#'  dummy1 = c(34, 35, 10, 5, 38)
#' )
#' clean_detect_ID_column(.data)

clean_detect_ID_column <- function(.data) {

  # set list of potential column names to match
  ID_potentials <- c("^id")
  all_col_names <- colnames(.data)

  # find ID columns
  ID_matches <- sapply(X = all_col_names, FUN = function(col){
    any(
      stringr::str_detect(
        string = col,
        pattern = stringr::regex(ID_potentials, ignore_case = TRUE)
      )
    )
  })
  ID <- all_col_names[ID_matches][1]
  # test if any columns are integers with spacing 1
  if (is.na(ID)) {
    is_ID <- unlist(sapply(X = .data[, all_col_names], FUN = function(col){
      is_ID <- FALSE
      if (is.numeric(col)){
        is_ID <- all(table(diff(sort(col))) == 1)
      }
      return(is_ID)
    }))
    ID <- all_col_names[is_ID][1]
  }

  # defaults if none are found
  if (isTRUE(is.na(ID))) ID <- NULL
  X <- setdiff(all_col_names, ID)

  matched_cols <- list(X = X, ID = ID)
  return(matched_cols)
}

#' Attempt to detect which columns of a dataframe are Z, Y, and X
#'
#' Attempts to auto detect which columns are treatment, response, or ID columns. Treatment and response are detected based on column name alone; ID is based on column name and the values. If none are detected, then columns are categorized as 'X'. Will only return one item (the first) for Z, Y, and ID.
#'
#' @param .data a dataframe
#'
#' @author Joe Marlo
#'
#' @return a list denoting which column names are Z, Y, X, and ID
#' @noRd
#'
#' @examples
#' .data <- data.frame(
#'  treatment = c(TRUE, TRUE, FALSE, TRUE, FALSE),
#'  rsp = c(0.808, 0.296,-1.579,-1.272, 0.627),
#'  ID = c(0.808, 0.296,-1.579,-1.272, 0.627),
#'  dummyID = 1:5,
#'  dummy1 = c(34, 35, 10, 5, 38)
#' )
#' clean_detect_ZYX_columns(.data)
clean_detect_ZYX_columns <- function(.data) {

  # set list of potential column names to match
  Z_potentials <- c("^Z_| ", '^Z$', "trt", "treat", "treatment")
  Y_potentials <- c("^Y_| ", '^Y$', "response", "rsp")
  ID_potentials <- c("^id")
  all_col_names <- colnames(.data)

  # find Z column
  Z_matches <- sapply(X = all_col_names, FUN = function(col){
    any(
      stringr::str_detect(
        string = col,
        pattern = stringr::regex(Z_potentials, ignore_case = TRUE)
      )
    )
  })
  Z <- all_col_names[Z_matches][1]

  # find Y columns
  all_col_names_ex_Z <- setdiff(all_col_names, Z)
  Y_matches <- sapply(X = all_col_names_ex_Z, FUN = function(col){
    any(
      stringr::str_detect(
        string = col,
        pattern = stringr::regex(Y_potentials, ignore_case = TRUE)
      )
    )
  })
  Y <- all_col_names_ex_Z[Y_matches][1]

  # find ID columns
  all_col_names_ex_ZY <- setdiff(all_col_names, c(Z, Y))
  ID_matches <- sapply(X = all_col_names_ex_ZY, FUN = function(col){
    any(
      stringr::str_detect(
        string = col,
        pattern = stringr::regex(ID_potentials, ignore_case = TRUE)
      )
    )
  })
  ID <- all_col_names_ex_ZY[ID_matches][1]
  # test if any columns are integers with spacing 1
  if (is.na(ID)) {
    is_ID <- unlist(sapply(X = .data[, all_col_names_ex_ZY], FUN = function(col){
      is_ID <- FALSE
      if (is.numeric(col)){
        is_ID <- all(table(diff(sort(col))) == 1)
      }
      return(is_ID)
    }))
    ID <- all_col_names_ex_ZY[is_ID][1]
  }

  # defaults if none are found
  if (isTRUE(is.na(Z))) Z <- NULL
  if (isTRUE(is.na(Y))) Y <- NULL
  if (isTRUE(is.na(ID))) ID <- NULL
  X <- setdiff(all_col_names, c(Z, Y, ID))

  matched_cols <- list(Z = Z, Y = Y, X = X, ID = ID)
  return(matched_cols)
}

#' Determine which default columns to use in plot_exploration()
#'
#' @param .column_types a list containing names of categorical and continuous columns. Ideally from output of clean_detect_column_types().
#' @param .treatment_column name of column denoting treatment
#' @param .response_column name of the outcome column
#'
#' @return a list denoting which columns to use in plotting
#' @noRd
#'
#' @seealso \code{\link{plot_exploration} \link{clean_detect_column_types}}
#'
#' @examples
#' X <- data.frame(
#'  treatment = sample(as.logical(0:1), 5, TRUE),
#'  response = rnorm(5),
#'  X1 = 1:5,
#'  X2 = rnorm(5),
#'  X3 = LETTERS[1:5],
#'  X4 = as.factor(LETTERS[1:5])
#')
#' column_types <- clean_detect_column_types(X)
#' clean_detect_plot_vars(column_types, 'treatment', 'response')
clean_detect_plot_vars <- function(.column_types, .treatment_column, .response_column){

  trt_in_types <- .treatment_column %in% unlist(.column_types)
  outcome_in_types <- .response_column %in% unlist(.column_types)
  if (!isTRUE(trt_in_types & outcome_in_types)) stop(".treatment_column and .response_column must be in .column_types")

  vars_categorical <- setdiff(.column_types$categorical, c(.treatment_column, .response_column))
  vars_continuous <- setdiff(.column_types$continuous, c(.treatment_column, .response_column))

  n_categorical <- length(vars_categorical)
  n_continuous <- length(vars_continuous)

  Y <- NULL
  fill <- .treatment_column
  shape <- "None"
  size <- "None"
  grouping <- "None"
  facet <- "None"

  # TODO see brainstorm matrix in slack
  if (.response_column %in% .column_types$continuous){
    Y <- .response_column
    if (n_continuous > 0){
      plot_type <- 'Scatter'
      X <- vars_continuous[1]
    } else {
      plot_type <- 'Boxplot'
      X <- vars_categorical[1]
    }
  } else if (.response_column %in% .column_types$categorical){
    if (n_continuous > 0){
      plot_type <- 'Boxplot'
      X <- vars_continuous[1]
      grouping <- .treatment_column
    } else {
      plot_type <- 'Barplot'
      X <- vars_continuous[1]
      Y <- .response_column
      facet <- .treatment_column
    }
  } else stop("Outcome is somehow not classifed as either continuous or categorical")

  plot_variables <- list(
    plot_type = plot_type,
    X = X,
    Y = Y,
    fill = fill,
    shape = shape,
    size = size,
    grouping = grouping,
    facet = facet
  )

  return(plot_variables)
}


#' Convert categorical columns to indicator variables
#'
#' @param df a dataframe with varibales
#'
#' @return a dataframe with no categorical variables
#' @author George Perrett & Joe Marlo
clean_to_indicator <- function(df){
  character_vars <- names(which(sapply(df, is.character)))
  factor_vars <- names(which(sapply(df, is.factor)))


  if(length(factor_vars) >0 | length(character_vars) > 0 ){
    x <- dplyr::select(df, c(all_of(factor_vars), all_of(character_vars)))

    get_levels <- function(vec){
      out <- length(unique(vec))
    }
    levels <- sapply(x, get_levels)
    col_names <- names(x)
    names_list <- list()
    for (i in 1:length(col_names)) {
      names_list[[i]] <- rep(col_names[i], levels[i])
      names_list[[i]] <- paste(names_list[[i]], unique(x[[i]]), sep = "_")
    }

    y <- dplyr::select(df, -c(all_of(factor_vars), all_of(character_vars)))
    dummy_vec <- function(vec){
      # get unique vals
      uni_vals <- unique(vec)
      dummys <- list()

      for (i in 1:length(uni_vals)) {
        dummys[[i]] <- ifelse(vec == uni_vals[i], 1, 0)
      }

      names(dummys) <- uni_vals
      dummys <- bind_cols(dummys)
      return(dummys)
    }

    x <- bind_cols(purrr::map(x, dummy_vec))
    names(x) <- unlist(names_list)
    confounders_mat <- as.data.frame(cbind(y, x))
  }

  else{
    confounders_mat <- as.data.frame(df)
  }

  return(confounders_mat)
}
