#' Fit a BART model using a standardized interface
#'
#' @param .data the data. First column should be treatment, second column response.
#' @param support one of c('ate', 'atc', 'att'). See bartCause::bartc
#' @param ran.eff if not "None", then the name of the column within .data that represents the random effects
#' @param .estimand the causal estimand. See bartCause::bartc
#'
#' @return an object of class "bartcFit"
#' @export
#' @noRd
fit_bart <- function(.data, support, block, .weights, ran_eff, .estimand){
  ind <- max(3, 3 + length(.weights) + length(ran_eff))
  if(rlang::is_null(.weights)){
    tryCatch({
      if(rlang::is_null(ran_eff)){
        bartCause::bartc(
          response = .data[, 2],
          treatment = .data[, 1],
          confounders = clean_confounders_for_bart(.data[, ind:length(.data)]),
          estimand = .estimand,
          commonSup.rule = support,
          keepTrees = TRUE,
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
          commonSup.rule = support,
          keepTrees = TRUE,
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
          commonSup.rule = support,
          keepTrees = TRUE,
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
          commonSup.rule = support,
          keepTrees = TRUE,
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

#' Check for common support via the standard deviation and chi-square rules
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$analysis$model$model
#'
#' @author Vince Dorie, George Perrett, Joe Marlo
#'
#' @return a list
#' @export
#' @noRd
check_common_support <- function(.model){

  validate_model_fit_(.model)

  # calculate summary stats
  sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
  total_sd <- switch (.model$estimand,
                      ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
                      att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
                      atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
  )

  inference_group <- switch (.model$estimand,
                             ate = length(.model$sd.obs[!.model$missingRows]),
                             att = length(.model$sd.obs[!.model$missingRows] & .model$trt == 1),
                             atc = length(.model$sd.obs[!.model$missingRows] & .model$trt == 0)
  )


  # create dataframe of the sd and chi values
  dat <- as_tibble(.model$data.rsp@x) %>%
    rename(`Propensity Score` = ps)

  dat.sd <- dat %>%
    mutate(sd.cut = if_else(.model$trt == 1, sd.cut[1], sd.cut[2]),
           removed = if_else(.model$sd.cf > sd.cut, 'Removed', 'Included'),
           support_rule = 'sd',
           stat = .model$sd.cf,
           sd.cf = .model$sd.cf) %>%
    select(-sd.cut)


  dat.chi <- dat %>%
    mutate(removed = if_else((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 'Removed', 'Included'),
           support_rule = 'chi',
           stat = (.model$sd.cf / .model$sd.obs) ** 2,
           sd.cf = .model$sd.cf)

  dat <- rbind(dat.sd, dat.chi)

  if(.model$estimand == 'att') dat <- dat[rep(.model$trt, 2) == 1,]
  if(.model$estimand == 'atc') dat <- dat[rep(.model$trt, 2) == 0,]

  prop <- dat %>%
    group_by(support_rule) %>%
    count(removed) %>%
    mutate(prop = n/sum(n)*100) %>%
    filter(removed == 'Removed') %>%
    arrange(support_rule) %>%
    ungroup()

  percent_out <- purrr::as_vector(prop$prop)
  names(percent_out) <- prop$support_rule
  # orderd alphabetically
  if(is.na(percent_out['sd'])) percent_out['sd'] <- 0
  if(is.na(percent_out['chi'])) percent_out['chi'] <- 0

  sd_output <- paste0(round(percent_out['sd'], 2), "% of cases would have been removed under the standard deviation overlap rule")

  chi_output <- paste0(round(percent_out['chi'], 2), "% of cases would have been removed under the chi squared overlap rule")


  # create message to user
  common_support_message <- paste(chi_output, sd_output, sep = '<br><br><br>')

  out <- list(
    message = common_support_message,
    proportion_removed_sd = sd_output,
    proportion_removed_chi = chi_output
  )

  return(out)
}


