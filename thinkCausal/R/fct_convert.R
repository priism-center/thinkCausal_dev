#' @title Convert the column types of a dataframe
#'
#' @description Designed to fail in an appropriate way
#'
#' @param .data data frame
#' @param new_data_types character vector of new data types
#'
#' @return data frame
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
#'  str(convert_data_types(x, c('Binary', 'Categorical', 'Continuous', 'Binary', 'Continuous')))
convert_data_types <- function(.data, new_data_types){
  # get current data types
  # old_data_types <- convert_data_type_to_simple(.data)

  # convert the data
  for (i in seq_along(.data)){
    .data[[i]] <- convert_data_types_(.data[[i]], new_data_types[i])
  }

  return(.data)
}

#' @describeIn convert_data_types converts x to the new_data_type
convert_data_types_ <- function(x, new_data_type){
  if (new_data_type %notin% c('Categorical', 'Binary', 'Continuous')) stop("new_data_type must be one of c('Categorical', 'Binary', 'Continuous')")

  convert_fn <- switch(
    new_data_type,
    "Categorical" = as.character,
    'Binary' = coerce_to_logical,
    "Continuous" = as.numeric
  )

  x <- convert_fn(x)

  return(x)
}

#' @title Simplify data types for humans
#'
#' @description Categorizes R data types into 'Continuous', 'Categorical', or 'Binary'
#'
#' @param .data dataframe
#'
#' @author Joe Marlo
#'
#' @return character vector of length ncol(.data)
#' @noRd
#'
#' @seealso \code{\link{convert_data_type_to_complex}}
#'
#' @examples
#' n_row <- 10
#' my_character = sample(c('one', 'two', 'three'), size = n_row, replace = TRUE)
#' my_logical = sample(c(TRUE, FALSE), size = n_row, replace = TRUE)
#' my_numeric = rnorm(n_row)
#' X <- data.frame(my_character = my_character, my_logical = my_logical, my_numeric = my_numeric)
#' convert_data_type_to_simple(X)
convert_data_type_to_simple <- function(.data){

  # get raw data types
  raw_data_types <- base::sapply(.data, class)

  # create mapping between complex and simple data types
  data_type_mapping <- data.frame(
    complex = c("character", "factor", "logical", "numeric", "integer"),
    simple = c("Categorical", "Categorical", 'Binary', "Continuous", "Continuous"),
    stringsAsFactors = FALSE
  )

  # get simple data
  simple_data_types <- dplyr::left_join(
    x = data.frame(complex = as.vector(raw_data_types)),
    y = data_type_mapping,
    by = 'complex')$simple

  return(simple_data_types)
}


#' @title Identify levels of an indicator variable when data is has alredy been one hot encoded
#'
#' @description For thinkCausal app development use
#'
#' @param x a string that corresponds to the name of a variable that may be part of a categorical variable
#' @param cat_df dataframe of all non continuous confounders

#' @author George Perrett
#'
#' @return a list that containing a vecotr best and a list possible. best is the most likely combination of levels while possible is all possible combinations of levels
#' @noRd
#'
#' @examples
#' cat_df <- data.frame(
#'  momwhite = c(1,0,0,0,0,0,1),
#'  momblack = c(0,1,1,1,1,0,0),
#'  momhisp = c(0,0,0,0,0,0,0),
#'  arc =c(1,0,0,0,1,0,1)
#'  )
#'identify_indicators(x = 'momwhite', cats = cat_df)
identify_indicators <- function(x, cats){

  transform_indicator_to_categorical <- function(.x, cat_df){
    compare <- cat_df %>%
      select(-all_of(.x))

    run_test <- lapply(1:length(compare), function(i){
      tmp <- tibble(cat_df[[all_of(.x)]], compare[[i]])
      rows <- tmp %>%
        mutate(rSum = rowSums(across())) %>%
        filter(rSum > 1) %>%
        nrow()
      if(rows == 0) names(compare)[i]
    })

    out <- unlist(run_test)
    out <- c(.x, out)
    out <- sort(out)
    return(out)

    return(out)
  }

  run <- transform_indicator_to_categorical(.x = x, cat_df = cats)
  eval <- lapply(run, function(i){transform_indicator_to_categorical(.x = i, cat_df = cats)})
  probs <- table(match(eval, unique(eval)))/length(eval)
  out <- list(best = eval[[which(probs == max(probs))]], possible = unique(eval))
  return(out)
}

#' Convert dummies in a dataframe to a categorical variable
#' For downloaded reproducible script use
#'
#' @param df dataframe
#' @param group_names a vector of strings containing dummies' names of a categorical variable.
#' The name of the vector will used as the categorical variable's name
#'
#' @author Junhui Yang
#'
#' @return a data frame
#' @noRd
#'
#' @examples
#' df <- data.frame(
#' momwhite = c(1,0,0,0,0,0,1),
#' momblack = c(0,1,1,1,1,0,0),
#' momhisp = c(0,0,0,0,0,0,0),
#' y.obs = rnorm(7, 10, 3)
#' )
#' race <- c("momwhite", "momblack", "momhisp")
#' clean_dummies_to_categorical(df = df, group_names = race)
clean_dummies_to_categorical <- function(df, group_names, new_name = 'categorical_var'){

  # find the column indexes of dummy variables in the same group
  idx <- which(colnames(df) %in% group_names)
  tmp <- df[,idx]
  categorical <- apply(tmp, 1, function(x) ifelse(sum(x, na.rm = T) == 0, 'REFERENCE', colnames(tmp)[which(x == TRUE)]))
  # remove the multiple dummies from the dataset
  df <- df[,-idx, drop=FALSE]
  # add the new categorical variable into the dataset
  df <- cbind(df, categorical)
  names(df)[length(df)] <- new_name
  return(df)

}
