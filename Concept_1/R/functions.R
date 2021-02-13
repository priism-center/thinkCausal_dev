
pretty_datatable <- function(...){
  # wrapper around DT::datatable so commonly used arguments
    # can be set as global defaults
  
  DT::datatable(..., 
                rownames = FALSE, 
                options = list(
                  pageLength = 20, # sets n observations shown
                  lengthChange = FALSE, #  removes option to change n observations shown
                  sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
                  scrollX = TRUE # enable side scroll so table doesn't overflow
                )
  )
}

progress_bar <- function(progress) {
  # returns the html to create a bootsrap progress bar filled to the progress amount
  # TODO: change fill color
  #https://getbootstrap.com/docs/4.4/components/progress/
  tags$div(
    class = 'progress',
    tags$div(
      class = "progress-bar progress-bar-striped progress-bar-animated bg-info",
      role = "progressbar",
      style = paste0("width: ", progress, "%"),
      'aria-valuenow' = as.character(progress),
      'aria-valuemin' = "0",
      'aria-valuemax' = "100"
    )
  )
}

# example data set for demo 
make_dat_demo <- function(n = 400){
  inv.log <- function(x){exp(x*.8)/(1 + exp(x*.8))}
  X <- runif(n, min = 0,max = 2)
  X2 <- rnorm(n, -2, 5)
  prob <- inv.log((-2 + (X^2)))
  Z <- rbinom(n, 1, prob)
  A <- matrix(runif(4), 2, 2)
  A <- A %*% t(A)
  diag(A) <- 1
  Sigma <- A
  X3 <- MASS::mvrnorm(n, rep(2, 2), Sigma)[, 1]
  X4 <- MASS::mvrnorm(n, rep(2, 2), Sigma)[, 2]
  Y0 <- 2*X + .8*X2 + rnorm(n, 0, 1)
  Y1 <- 2*X + 3*(X^2) + .8*X2 +  rnorm(n, 0, 1)
  df <- data.frame(X, prob, Z, Y0, Y1, X2, X3, X4)
  df <- df %>% mutate(Y = if_else(Z ==1, Y1, Y0))
  df <- df %>% dplyr::select(-c(Y0, Y1, prob))
  return(df)
}


detect_ZYX_columns <- function(input_colnames) {
  # attempts to auto detect which columns are treatment and response based on the 
  # column name alone
  # if none are detected, then returns the first two respectively
  # TODO: instead of regex, do string distance?
  
  Z_potentials <- c("Z", "trt", "treat", "treatment")
  Y_potentials <- c("Y", "response", "rsp")
  all_col_names <- input_colnames
  
  # find Z column
  Z_matches <- sapply(X = all_col_names, FUN = function(col){
    any(stringr::str_detect(string = col, pattern = Z_potentials))
  })
  Z <- all_col_names[Z_matches][1]

  # find Y columns
  all_col_names_ex_Z <- setdiff(all_col_names, Z)
  Y_matches <- sapply(X = all_col_names_ex_Z, FUN = function(col){
    any(stringr::str_detect(string = col, pattern = Y_potentials))
  })
  Y <- all_col_names_ex_Z[Y_matches][1]
  
  # defaults if none are found
  all_col_names_ex_Y <- setdiff(all_col_names, Y)
  if (isTRUE(is.na(Z))) Z <- all_col_names_ex_Y[1]
  all_col_names_ex_Z <- setdiff(all_col_names, Z)
  if (isTRUE(is.na(Y))) Y <- all_col_names_ex_Z[1]
  
  matched_cols <- list(Z = Z, Y = Y, X = setdiff(all_col_names, c(Z, Y)))
  return(matched_cols)
}
