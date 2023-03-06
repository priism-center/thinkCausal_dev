# default theme is set on load by input$settings_options_ggplot_theme

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = function() ggplot2::scale_colour_viridis_d(),
  ggplot2.discrete.fill = function() ggplot2::scale_fill_viridis_d()
)

#' ggplot2 minimal theme without transparent background
#'
#' This mimics ggplot2::theme_minimal but excludes the transparent background so downloaded plots are legible.
#'
#' @param base_size See ggplot2::theme_bw
#' @param base_family See ggplot2::theme_bw
#' @param base_line_size See ggplot2::theme_bw
#' @param base_rect_size See ggplot2::theme_bw
#'
#' @author Joe Marlo
#'
#' @return ggplot2 theme
#'
#' @import ggplot2
#'
#' @noRd
theme_minimal_no_transparency <- function(base_size = 11, base_family =  "", base_line_size = base_size/22, base_rect_size = base_size/22){
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(axis.ticks = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          # plot.background = element_blank(),
          complete = TRUE)
}

#' General purpose plotting for EDA
#'
#' To be used programmatically. Not recommended for script/console use.
#'
#' @param .data typically store$verified_df
#' @param .plot_type  one of c('Scatter', 'Histogram', 'Density', 'Boxplot')
#' @param .x x variable as a string
#' @param .y y variable as a string
#' @param .levels factor levels
#' @param .fill fill color variable as a string
#' @param .fill_static fill color as a string
#' @param .size passed to ggplot2::geom_point(size = .size)
#' @param .shape variable to specify shape type as a string. Passed to ggplot2::geom_point(aes(shape = .shape))
#' @param .alpha transparency value [0, 1]
#' @param .vars_pairs deprecated
#' @param .n_bins number of bins to use in historgram
#' @param .jitter boolean. Jitter the scatter points
#' @param .groups grouping variable as a string. Passed to ggplot2::aes(group = .groups)
#' @param .facet variable (as a string) to use for panel plots
#' @param .facet_second variable (as a string) to use for panel plots
#' @param .include_regression one of c("Include", "None")
#'
#' @author Joe Marlo and George Perrett
#'
#' @return ggplot2 object
#' @noRd
#'
#' @seealso \code{\link{clean_detect_plot_vars}}
#'
#' @import ggplot2
#'
#' @examples
#' library(arm)
#' data('lalonde', package = 'arm')
#' X <- lalonde
#' X <- dplyr::select(X, 'treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' X <- clean_auto_convert_logicals(X)
#' plot_exploration(
#'  .data = X,
#'  .plot_type = 'Boxplot', #c('Scatter', 'Histogram', 'Density', 'Boxplot'),
#'  .x = 're78',
#'  .y = 'age',
#'  .fill = NULL,
#'  .fill_static = "#5c5980",
#'  .size = 'age',
#'  .shape = 'None',
#'  .alpha = 0.5,
#'  .vars_pairs,
#'  .n_bins = 30,
#'  .jitter = FALSE,
#'  .groups = 'None',
#'  .facet = 'None',
#'  .facet_second = 'None',
#'  .include_regression = 'None'
#' )
plot_exploration <- function(.data,
                             .plot_type = c('Scatter', 'Histogram', "Barplot", 'Density', 'Boxplot'),
                             .x,
                             .y,
                             .levels,
                             .fill,
                             .fill_static = "grey20",
                             .size,
                             .shape,
                             .alpha = 0.9,
                             .vars_pairs,
                             .n_bins,
                             .jitter,
                             .groups,
                             .facet,
                             .facet_second,
                             .include_regression = c("Include", "None")) {

  # convert "None"s to NULL
  if (isTRUE(.fill == "None")) .fill <- NULL
  .color <- .fill
  if (isTRUE(.size == "None")) .size <- NULL
  if (isTRUE(.shape == "None")) .shape <- NULL
  if (length(.levels) > 1) .data <- clean_dummies_to_categorical(df = .data, group_names = c(.x, .levels), new_name = .x)
  if (length(.levels) > 1 & .groups != 'None') .data <- clean_dummies_to_categorical(df = .data, group_names = c(.groups, .levels), new_name = .groups)


  # create base ggplot object
  p <- ggplot(.data, aes_string(x = sym(.x)))

  # pairs plot
  # if (.plot_type == 'Pairs'){
  #   p <- GGally::ggpairs(.data[, .vars_pairs])
  # }

  # scatter
  if (.plot_type == 'Scatter'){

    if (.jitter){
      p <- p +
        geom_jitter(aes_string(y = sym(as.character(.y)),
                               fill = if(is.null(.fill)) NULL else sym(.fill),
                               color = if(is.null(.color)) NULL else sym(.color),
                               size = if(is.null(.size)) NULL else sym(.size),
                               shape = if(is.null(.shape)) NULL else sym(.shape)
        ), alpha = .alpha)
    } else {
      p <- p +
        geom_point(aes_string(y = sym(as.character(.y)),
                              fill = if(is.null(.fill)) NULL else sym(.fill),
                              color = if(is.null(.color)) NULL else sym(.color),
                              size = if(is.null(.size)) NULL else sym(.size),
                              shape = if(is.null(.shape)) NULL else sym(.shape)
        ), alpha = .alpha)
    }

    # regression line
    if(.include_regression == 'Include'){
      p <- p + geom_smooth(
        aes_string(y = sym(.y)),
        method = "lm",
        formula = 'y ~ x',
        color = "grey20"
      )
    }
  }

  # histogram
  if (.plot_type == 'Histogram'){
    p <- p + geom_histogram(color = 'white', bins = .n_bins,
                            fill = .fill_static, alpha = 0.9) +
      labs(y = NULL)
  }

  # barplot
  if (.plot_type == "Barplot"){
    p <- p + geom_bar(color = 'white', fill = .fill_static, alpha = 0.9) +
      labs(y = "Count of observations")
  }

  # density
  if (.plot_type == 'Density'){
    p <- p + geom_density(fill = .fill_static, alpha = 0.5) +
      labs(y = NULL)
  }

  # boxplot
  if (.plot_type == 'Boxplot'){
    p <- p +
      geom_boxplot(fill = .fill_static, alpha = 0.5,
                   if(.groups != 'None') aes_string(y = sym(.groups))
      ) +
      coord_flip() +
      scale_y_discrete()
  }

  # add faceting
  if (.facet != "None"){

    if (.facet_second == "None"){
      p <- p + facet_grid(sym(.facet), labeller = label_both)
    } else {
      p <- p + facet_grid(list(sym(.facet), sym(.facet_second)),
                          labeller = label_both)
    }
  }

  return(p)
}

# plot density of residual (predicted y - observed y)
plot_residual_density <- function(.model, covariate = NULL){

  # ensure model is a of class bartcFit
  validate_model_fit_(.model)

  # extract the covariates
  dat <- as.data.frame(.model$data.rsp@x)
  # add observed y
  dat$y.obs <- .model$data.rsp@y

  if(.model$estimand == "att"){
    dat <- dat[.model$trt == 1, ]
  }else if(.model$estimand == "atc"){
    dat <- dat[.model$trt == 0, ]
  }


  # add predicted y
  dat$y.hat.mean <- apply(bartCause::extract(.model, "mu.obs"), 2, mean)

  # add residual
  dat$residual <- dat$y.hat.mean - dat$y.obs

  dat$reference <- rnorm(n = nrow(dat), 0, sd(dat$residual))

  dat <- dat %>%
    tidyr::pivot_longer(cols = c('residual', 'reference'))

  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value, color = name)) +
    ggplot2::geom_density() +
    ggplot2::scale_color_manual(values = c(2, 1)) +
    ggplot2::labs(x = "Residual", y = "Density")

  return(p)
}

# plot residual vs predicted y
plot_residual_predicted_residual <- function(.model, covariate = NULL){

  # ensure model is a of class bartcFit
  validate_model_fit_(.model)

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
  if(rlang::is_null(covariate)){
    p <- p + geom_smooth(color = 2, se = F) +
      labs(x = "Predicted Y", y = "Residual")
  }else{
    p <- p + geom_smooth(color = 2, se = F) +
      labs(x = covariate, y = "Residual")
  }
  return(p)
}


# plot_common_support_temp <- function(.model, .x = 'Propensity Score',  rule = c('both', 'sd', 'chi')){
#
#   # ensure model is a of class bartcFit
#   validate_model_fit_(.model)
#
#   rule <- rule[1]
#   if (rule %notin% c('both', 'sd', 'chi')) stop('rule must be one of c("both", "sd", "chi")')
#   if (rule == 'both') rule <- c('sd', 'chi')
#
#   # calculate summary stats
#   sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
#   total_sd <- switch (.model$estimand,
#                       ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
#                       att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
#                       atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
#   )
#
#   inference_group <- switch (.model$estimand,
#                              ate = length(.model$sd.obs[!.model$missingRows]),
#                              att = length(.model$sd.obs[!.model$missingRows] & .model$trt == 1),
#                              atc = length(.model$sd.obs[!.model$missingRows] & .model$trt == 0)
#   )
#
#
#   # create dataframe of the sd and chi values
#   dat <- as_tibble(.model$data.rsp@x) %>%
#     rename(`Propensity Score` = ps)
#
#   dat.sd <- dat %>%
#     mutate(sd.cut = if_else(.model$trt == 1, sd.cut[1], sd.cut[2]),
#            removed = if_else(.model$sd.cf > sd.cut, 'Removed', 'Included'),
#            support_rule = 'sd',
#            stat = .model$sd.cf,
#            sd.cf = .model$sd.cf) %>%
#     select(-sd.cut)
#
#
#   dat.chi <- dat %>%
#     mutate(removed = if_else((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 'Removed', 'Included'),
#            support_rule = 'chi',
#            stat = (.model$sd.cf / .model$sd.obs) ** 2,
#            sd.cf = .model$sd.cf)
#
#   dat <- rbind(dat.sd, dat.chi)
#
#   if(.model$estimand == 'att') dat <- dat[rep(.model$trt, 2) == 1,]
#   if(.model$estimand == 'atc') dat <- dat[rep(.model$trt, 2) == 0,]
#
#   prop <- dat %>%
#     group_by(support_rule) %>%
#     count(removed) %>%
#     mutate(prop = n/sum(n)*100) %>%
#     filter(removed == 'Removed') %>%
#     arrange(support_rule) %>%
#     ungroup()
#
#   percent_out <- purrr::as_vector(prop$prop)
#   names(percent_out) <- prop$support_rule
#   # orderd alphabetically
#   if(is.na(percent_out['sd'])) percent_out['sd'] <- 0
#   if(is.na(percent_out['chi'])) percent_out['chi'] <- 0
#
#   dat <- dat %>%
#     mutate(support_rule_text = if_else(support_rule == 'chi',
#                                        paste0('Chi-squared rule: ', round(percent_out['chi'], 2), "% of cases would have been removed"),
#                                        paste0('Standard deviation rule: ', round(percent_out['sd'], 2), "% of cases would have been removed")))
#
#   # plot it
#   p <- dat %>%
#     filter(support_rule %in% rule) %>%
#     ggplot(aes(x = !!rlang::sym(.x), y = sd.cf, color = removed)) +
#     geom_point(alpha = 0.7) +
#     scale_color_manual(values = c(1, 2)) +
#     facet_wrap(~support_rule_text, ncol = 1, scales = 'free_y') +
#     labs(title ="Overlap checks",
#          x = .x,
#          y = 'Predicted counterfactual standard deviation',
#          color = NULL) +
#     theme(legend.position = 'bottom',
#           strip.text = element_text(hjust = 0))
#
#   return(p)
# }
