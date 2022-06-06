
# library(tidyverse)
# library(bartCause)

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

# detect the type of a varable
is_categorical <- function(x){
  is_int <- isTRUE(clean_detect_integers(x))
  is_binary <- isTRUE(clean_detect_logical(x))
  is_cont <- isTRUE(is.double(x))
  is_cat <- !any(is_int, is_binary, is_cont)
  return(is_cat)
}

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

clean_detect_logical <- function(x){
  
  if(inherits(x, 'data.frame')) stop('x cannot be a dataframe')
  
  # is x exclusively in list of pre-determined
  inclusion_list <- c(0, 1, 't', 'f', 'true', 'false')
  x_as_char <- as.character(x)
  x_cleaned <- base::tolower(unique(x_as_char))
  is_in_list <- length(setdiff(x_cleaned, inclusion_list)) == 0
  return(is_in_list)
}

# read data for testing functions
# df <- read.csv("./data/IHDP_observational.csv") %>%
#   select(-c(yc0hat, yc1hat))
# fit_ate <- bartc(y.obs, treat, ., data = df, estimand = "ate", commonSup.rule = 'sd')
# fit_att <- bartc(y.obs, treat, ., data = df, estimand = "att", commonSup.rule = 'sd')
# fit_atc <- bartc(y.obs, treat, ., data = df, estimand = "atc")


### plot functions for residuals from bart fit ---------------------------------
# plot observed y vs predicted y
plot_residual_observed_predicted <- function(.model, covariate = NULL){
  
  # ensure model is a of class bartcFit
  validate_model_(.model)
  
  # extract the covariates
  dat <-  as.data.frame(.model$data.rsp@x)
  # add observed y
  dat$y.obs <- .model$data.rsp@y
  # add predicted y
  dat$y.hat.mean <- .model$fit.rsp$yhat.train.mean
  
  # if estimand is ATT, only keep the units in the treatment group
  # if estimand is ATC, only keep the units in the control group
  if(.model$estimand == "att"){
    dat <- dat[.model$trt == 1, ]
  }else if(.model$estimand == "atc"){
    dat <- dat[.model$trt == 0, ]
  }
  
  if(is.null(covariate)){ # if no grouping variable is provided
    p <- ggplot(data = dat, aes(x = y.hat.mean, y = y.obs)) + 
      geom_point()
  }else{ # if a grouping variable is provided
    # ensure the input variable is within the dataset
    index <- which(colnames(dat) == covariate)
    if (!isTRUE(index > 0)) stop('Cannot find variable in original data. Is variable within the original dataframe used to fit the .model?')
    
    categorical <- isTRUE(is_categorical(dat[[covariate]]))
    binary <- isTRUE(clean_detect_logical(dat[[covariate]]))
    
    if(categorical | binary){ # color by a categorical or logical variable
      p <- ggplot(data = dat, aes(x = y.hat.mean, y = y.obs)) + 
        geom_point(aes(colour = factor(!!rlang::sym(covariate))))
    }else{ # color by a continuous variable in gradient
      p <- ggplot(data = dat, aes(x = y.hat.mean, y = y.obs)) + 
        geom_point(aes(colour = !!rlang::sym(covariate)))
    }
  }
  p <- p + labs(x = "Predicted Y", y = "Observed Y") + 
    theme_minimal() 
  return(p)
}

# plot_residual_observed_predicted(fit_ate)
# plot_residual_observed_predicted(fit_ate, "sex")
# plot_residual_observed_predicted(fit_ate, "momage")
# 
# plot_residual_observed_predicted(fit_att)
# plot_residual_observed_predicted(fit_att, "sex")
# plot_residual_observed_predicted(fit_att, "momage")
# 
# plot_residual_observed_predicted(fit_atc)
# plot_residual_observed_predicted(fit_atc, "sex")
# plot_residual_observed_predicted(fit_atc, "momage")


# plot density of residual (predicted y - observed y)
plot_residual_density <- function(.model, covariate = NULL){
  
  # ensure model is a of class bartcFit
  validate_model_(.model)
  
  # extract the covariates
  dat <-  as.data.frame(.model$data.rsp@x)
  # add observed y
  dat$y.obs <- .model$data.rsp@y
  # add predicted y
  dat$y.hat.mean <- .model$fit.rsp$yhat.train.mean
  # add residual
  dat$residual <- dat$y.hat.mean - dat$y.obs
  
  if(.model$estimand == "att"){
    dat <- dat[.model$trt == 1, ]
  }else if(.model$estimand == "atc"){
    dat <- dat[.model$trt == 0, ]
  }
  
  if(is.null(covariate)){
    p <- ggplot(data = dat, aes(x = residual)) + 
      geom_density()
  }else{
    # ensure the input variable is within the dataset
    index <- which(colnames(dat) == covariate)
    if (!isTRUE(index > 0)) stop('Cannot find variable in original data. Is variable within the original dataframe used to fit the .model?')
    
    categorical <- isTRUE(is_categorical(dat[[covariate]]))
    binary <- isTRUE(clean_detect_logical(dat[[covariate]]))
    
    if(categorical | binary){ # color by a categorical or logical variable
      p <- ggplot(data = dat, aes(x = residual, colour = factor(!!rlang::sym(covariate)))) + 
        geom_density()
    }else{ 
      p <- ggplot(data = dat, aes(x = residual)) + 
        geom_density()
    }
  }
  p <- p + labs(x = "Residual", y = "Density") + 
    theme_minimal()
  return(p)
}

# plot_residual_density(fit_ate)
# plot_residual_density(fit_ate, "sex")
# 
# plot_residual_density(fit_att)
# plot_residual_density(fit_att, "sex")
# 
# plot_residual_density(fit_atc)
# plot_residual_density(fit_atc, "sex")



# plot residual vs predicted y 
plot_residual_predicted_residual <- function(.model, covariate = NULL){
  
  # ensure model is a of class bartcFit
  validate_model_(.model)
  
  # extract the covariates
  dat <-  as.data.frame(.model$data.rsp@x)
  
  # add observed y
  dat$y.obs <- .model$data.rsp@y
  
  # filter to estimand
    if(.model$estimand == "att"){
    dat <- dat[.model$trt == 1, ]
  }else if(.model$estimand == "atc"){
    dat <- dat[.model$trt == 0, ]
  }
  

  # add predicted y
  dat$y.hat.mean <- apply(bartCause::extract(.model, 'mu.obs'), 2, mean)
  
  # add residual
  dat$residual <- dat$y.hat.mean - dat$y.obs

    if(is.null(covariate)){
    p <- ggplot(data = dat, aes(x = y.hat.mean, y = residual)) + 
      geom_point()
  }else{
    # ensure the input variable is within the dataset
    index <- which(colnames(dat) == covariate)
    if (!isTRUE(index > 0)) stop('Cannot find variable in original data. Is variable within the original dataframe used to fit the .model?')
    
    p <- ggplot(data = dat, aes(x = !!rlang::sym(covariate), y = residual)) + 
        geom_point()
      
    # categorical <- isTRUE(is_categorical(dat[[covariate]]))
    # binary <- isTRUE(clean_detect_logical(dat[[covariate]]))
    # 
    # if(categorical | binary){ # color by a categorical or logical variable
    #   p <- ggplot(data = dat, aes(x = y.hat.mean, y = residual)) + 
    #     geom_point(aes(colour = factor(!!rlang::sym(covariate))))
    # }else{ # color by a continuous variable in gradient
    #   p <- ggplot(data = dat, aes(x = y.hat.mean, y = residual)) + 
    #     geom_point(aes(colour = !!rlang::sym(covariate)))
    # }
  }
  if(is_null(covariate)){
  p <- p + geom_hline(yintercept = 0) + 
    labs(x = "Predicted Y", y = "Residual") + 
    theme_minimal() 
  }else{
    p <- p + geom_hline(yintercept = 0) + 
      labs(x = covariate, y = "Residual") + 
      theme_minimal() 
  }
  return(p)
}

# plot_residual_observed_residual(fit_ate)
# plot_residual_observed_residual(fit_ate, "sex")
# plot_residual_observed_residual(fit_ate, "momage")
# 
#plot_residual_observed_residual(fit_att)
# plot_residual_observed_residual(fit_att, "sex")
# plot_residual_observed_residual(fit_att, "momage")
# 
# plot_residual_observed_residual(fit_atc)
# plot_residual_observed_residual(fit_atc, "sex")
# plot_residual_observed_residual(fit_atc, "momage")


