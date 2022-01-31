#' Check for common support via the standard deviation and chi-square rules
#'
#' @param .model a model produced by bartCause::bartc(). Typically store$model_results
#'
#' @author Vince Dorie, George Perrett, Joe Marlo
#'
#' @return
#' @export
check_common_support <- function(.model){

  validate_model_fit_(.model)

  # create helper functions

  `%not_in%` <- Negate(`%in%`)
  getCommonSupportSubset <- function(sd.obs, sd.cf, commonSup.rule, commonSup.cut, trt, missingRows)
  {
    if (!is.character(commonSup.rule) || commonSup.rule[1L] %not_in% c("none", "sd", "chisq"))
      stop("commonSup.rule must be one of 'none', 'sd', or 'chisq'")
    commonSup.rule <- commonSup.rule[1L]

    if (commonSup.rule == "none") return(rep_len(TRUE, length(sd.obs)))

    if (commonSup.rule == "sd") {
      if (!is.numeric(commonSup.cut) || !is.finite(commonSup.cut))
        stop("for common support rule 'sd', commonSup.cut must be a real number")
    } else {
      if (!is.numeric(commonSup.cut) || commonSup.cut[1L] <= 0 || commonSup.cut[1L] >= 1)
        stop("for common support rule 'chisq', commonSup.cut must be in (0, 1)")
    }
    commonSup.cut <- commonSup.cut[1L]

    stat <- getCommonSupportStatistic(sd.obs, sd.cf, commonSup.rule, commonSup.cut)
    cut  <- getCommonSupportCutoff(sd.obs, sd.cf, commonSup.rule, commonSup.cut, trt, missingRows)

    if (commonSup.rule == "sd")
      stat <= ifelse(trt, cut["trt"], cut["ctl"])
    else
      stat <= cut
  }

  getCommonSupportStatistic <- function(sd.obs, sd.cf, commonSup.rule, commonSup.cut)
    switch(commonSup.rule,
           sd = sd.cf,
           chisq = (sd.cf / sd.obs)^2,
           none = rep_len(NA_real_, length(sd.obs)))

  getCommonSupportCutoff <- function(sd.obs, sd.cf, commonSup.rule, commonSup.cut, trt, missingRows)
    switch(commonSup.rule,
           sd = c(trt = max(sd.obs[!missingRows & trt > 0]), ctl = max(sd.obs[!missingRows & trt <= 0])) + commonSup.cut * sd(sd.obs[!missingRows]),
           chisq = qchisq(1 - commonSup.cut, 1),
           none = rep_len(NA_real_, length(sd.obs)))

  # check common support

  # check using standard deviation rule
  prop_sd <- sum(getCommonSupportSubset(sd.obs = .model$sd.obs, sd.cf = .model$sd.cf, commonSup.rule = 'sd', commonSup.cut = 1, missingRows = .model$missingRows, trt = .model$trt) == F)/nrow(.model$data.rsp@x)*100
  prop_sd <- round(prop_sd, 2)
  sd_output <- paste0(prop_sd, "% of cases would have been removed under the standard deviation common support rule")

  # check use chi-square rule
  prop_chi <- sum(getCommonSupportSubset(sd.obs = .model$sd.obs, sd.cf = .model$sd.cf, commonSup.rule = 'chisq', commonSup.cut = .05, missingRows = .model$missingRows, trt = .model$trt) == F)/nrow(.model$data.rsp@x)*100
  prop_chi <- round(prop_chi, 2)
  chi_output <- paste0(prop_chi, "% of cases would have been removed under the chi squared common support rule")

  # create message to user
  common_support_message <- paste(sd_output, chi_output, sep = '<br>')

  out <- list(
    message = common_support_message,
    proportion_removed_sd = prop_sd,
    proportion_removed_chi = prop_chi
  )

  return(out)
}

