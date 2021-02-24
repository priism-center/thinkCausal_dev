
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

simple_data_types <- function(input_data){
  # simplifies the data types to either categorical, logical, or numeric
  # returns NA if it can't simplify
  
  raw_data_types <- apply(input_data, 2, class)
  
  # create mapping between complex and simple data types
  data_type_mapping <- data.frame(
    complex = c("character", "factor", "logical", "numeric", "integer"),
    simple = c("Categorical", "Categorical", 'Logical', "Continuous", "Continuous"),
    stringsAsFactors = FALSE
  )
  
  # get simple data
  simple_data_types <- left_join(
    x = data.frame(complex = as.vector(raw_data_types)),
    y = data_type_mapping,
    by = 'complex')
  
  return(simple_data_types$simple)
}

auto_convert_logicals <- function(input_data){
  # function converts columns of 0:1, T:F, True:False to logicals
  
  for (col in colnames(input_data)){
    
    # is the column exclusively in list of pre-determined
    inclusion_list <- c(0, 1, 't', 'f', 'true', 'false')
    col_as_char <- as.character(input_data[[col]])
    col_cleaned <- base::tolower(unique(col_as_char))
    is_in_list <- length(setdiff(col_cleaned, inclusion_list)) == 0
    
    # convert column to logical
    if (isTRUE(is_in_list)){
      input_data[,col] <- readr::parse_logical(col_as_char)
    }
  }
  
  return(input_data)
}


cate_test <- function(.fit = fit, confounders = X){
  # extract individual conditional effects 
  icate <- bartCause::extract(.fit , 'icate')
  icate.m <- apply(icate, 2, mean)
  icate.sd <- apply(icate, 2, sd)

  # fit regression tree 
  cart <- rpart::rpart(icate.m ~ confounders)
  # svae variable importance
  importance <- cart$variable.importance/sum(cart$variable.importance)*100
  names(importance) <- sub(".", "", names(importance))
  
  importance_table <- importance %>% 
    as_tibble() %>% 
    mutate(Variable = names(importance)) %>% 
    dplyr::select(Variable, value) %>% 
    rename(Importance = value) 
  
  # plot variable importance
  p1 <- ggplot(importance_table, aes(Importance, reorder(Variable, Importance))) + 
    geom_segment(aes(xend = 0, yend = Variable))  +
    geom_point(size = 4) + 
    labs(x = 'Importance', y = 'Variable', title = 'Potential Moderators')
  
  # Reorder confounder matrix by var importance
  X <- X[, names(importance)]
  
  # plot conditional effects
  posterior <- bartCause::extract(.fit, 'icate') %>% 
    as_tibble() %>% 
    pivot_longer(cols = 1:445)
  
  ploter <- function(x) {
    if(length(unique(x)) > 2) {
      p <-  posterior %>% 
        mutate(confounder = rep(x, 5000)) %>% 
        group_by(record = name) %>% 
        mutate(ci.1 = quantile(value, probs = .1), 
               ci.9 = quantile(value, probs = .9), 
               ci.025 = quantile(value, probs = .025), 
               ci.975 = quantile(value, probs = .975), 
               point = mean(value)) %>% 
        dplyr::select(-c(value, name)) %>% 
        distinct() %>% 
        ungroup() %>% 
        ggplot(aes(confounder, point)) + 
        geom_ribbon(aes(ymin = ci.025, ymax = ci.975,fill = '95% ci'), alpha = .7) + 
        geom_ribbon(aes(ymin = ci.1,ymax = ci.9, fill = '80% ci'), alpha = .7) + 
        scale_fill_manual(values = c('steelblue', 'steelblue3')) + 
        geom_point(size = .8) + 
        geom_smooth(method = 'gam' ,aes(y = point, x = confounder), col = 'black', se = F) + 
        labs(y = 'CATE')
    }
    else{
      p <- posterior %>%
        mutate(confounder = rep(x, 5000),
               confounder = factor(confounder)) %>%
        ggplot(aes(value, group = as.factor(name), col = confounder)) +
        geom_density(alpha = .7)
    }
    
    return(p)
  }
  
  cate_plts <- list()
  for (i in 1:ncol(X)) {
    cate_plts[[i]] <- ploter(X[,i])
    
  }
  
  results <- list(p1, cate_plts)
  return(results)
}
