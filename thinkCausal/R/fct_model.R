#' Fit a BART model using a standardized interface
#'
#' @param .data the data. First column should be treatment, second column response.
#' @param ran.eff if not "None", then the name of the column within .data that represents the random effects
#' @param .estimand the causal estimand. See bartCause::bartc
#' @param .weights name of the variable corresponding to a vector of survey weights
#' @param rct a logical TRUE is completely randomized design FALSE otherwise
#' @return an object of class "bartcFit"
#' @noRd
fit_bart <- function(.data, .weights, ran_eff, .estimand, rct){
  ind <- max(3, 3 + length(.weights) + length(ran_eff))
  if(rlang::is_null(.weights)){
    tryCatch({
      if(rlang::is_null(ran_eff)){
        bartCause::bartc(
          response = .data[, 2],
          treatment = .data[, 1],
          confounders = clean_confounders_for_bart(.data[, ind:length(.data)]),
          estimand = .estimand,
          method.trt = ifelse(rct, 'none', 'bart'),
          seed = 2
        )
      }

      else{
        bartCause::bartc(
          response = .data[[2]],
          treatment = .data[[1]],
          confounders = clean_confounders_for_bart(.data[, ind:length(.data)]),
          estimand = .estimand,
          group.by = ran_eff,
          use.ranef = TRUE,
          method.trt = ifelse(rct, 'none', 'bart'),
          seed = 2
        )
      }
    },
    error = function(e) NULL
    )
  }else{
    tryCatch({
      if(rlang::is_null(ran_eff)){
        bartCause::bartc(
          response = .data[, 2],
          treatment = .data[, 1],
          confounders = clean_confounders_for_bart(.data[, ind:length(.data)]),
          estimand = .estimand,
          weights = .data[[.weights]],
          method.trt = ifelse(rct, 'none', 'bart'),
          seed = 2
        )
      }

      else{
        bartCause::bartc(
          response = .data[[2]],
          treatment = .data[[1]],
          confounders = clean_confounders_for_bart(.data[, ind:length(.data)]),
          estimand = .estimand,
          weights = .data[[.weights]],
          group.by = ran_eff,
          use.ranef = TRUE,
          method.trt = ifelse(rct, 'none', 'bart'),
          seed = 2
        )
      }
    },
    error = function(e) NULL
    )
  }
}

clean_confounders_for_bart <- function(df){
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
    confounders_mat <- as.matrix(cbind(y, x))
  }

  else{
    confounders_mat <- as.matrix(df)
  }

  return(confounders_mat)
}


#' Identify predictors of the outcome variable. This is typically used to check overlap of most relevant predictors.
#'
#' @description A fct function
#'
#' @param X a matrix of predictor variables. Typically store$verified_df
#' @param y a vector of the outcome variable. Typically store$verified_df[[2]]
#' @param .weights a vector of survey weights
#' @param  ran_eff a vector of random intercepts
#'
#' @author George Perrett
#'
#' @return vector
#'
#' @noRd

find_predictors <- function(X, .y, .weights, ran_eff){
  ind <- max(3, 3 + length(.weights) + length(ran_eff))
  X <- clean_confounders_for_bart(df = clean_confounders_for_bart(X[, ind:length(X)]))
  fit <- glmnet::cv.glmnet(x = X, y = .y)
  rownames(coef(fit))[as.vector(coef(fit)) != 0 & rownames(coef(fit)) != "(Intercept)"]

}


#' Check overlap rules of a BART model
#'
#' @description A fct function
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$analysis$model$model
#'
#' @author George Perrett
#'
#' @return list
#'
#' @noRd
check_overlap_rules <- function(.model){

  sd.cut <- c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
  sd.stat <- .model$sd.cf

  total.sd <- switch (.model$estimand,
                      ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
                      att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
                      atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
  )

  sd.removed <- switch (.model$estimand,
                        ate = ifelse(.model$trt == 1, sd.stat > sd.cut[1], sd.stat >sd.cut[2]),
                        att = .model$sd.cf[.model$trt==1] > sd.cut[1],
                        atc = .model$sd.cf[.model$trt==0] > sd.cut[2]
  )

  ## chi sqr rule
  chi.cut <- 3.841
  chi.stat <- (.model$sd.cf / .model$sd.obs) ** 2
  total.chi <- switch (
    .model$estimand,
    ate = sum((.model$sd.cf / .model$sd.obs) ** 2 > 3.841),
    att = sum((.model$sd.cf[.model$trt == 1] / .model$sd.obs[.model$trt == 1]) ** 2 > 3.841),
    atc = sum((.model$sd.cf[.model$trt == 0] / .model$sd.obs[.model$trt == 0]) ** 2 > 3.841)
  )
  chi.removed <- switch(
    .model$estimand,
    ate = ifelse(chi.stat > chi.cut, 1, 0),
    att = ifelse(chi.stat[.model$trt == 1] > chi.cut, 1, 0),
    atc = ifelse(chi.stat[.model$trt == 0] > chi.cut, 1, 0)
  )

  list(
    ind_chisq_removed = chi.removed,
    sum_chisq_removed = total.chi,
    stat_chi = chi.stat,
    ind_sd_removed = sd.removed,
    sum_sd_removed = total.sd,
    stat_sd = sd.stat
  )

}


