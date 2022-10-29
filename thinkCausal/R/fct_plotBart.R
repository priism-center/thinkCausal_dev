#' @title Plot the histogram or density of the Conditional Average Treatment Effect
#' @description Plot the conditional average treatment effect (CATE) of a 'bartCause' model.
#' The conditional average treatment effect is derived from taking the difference between
#' predictions for each individual under the control condition and under the treatment condition averaged over the population.
#' Means of the CATE distribution will resemble SATE and PATE but the CATE distribution accounts for more uncertainty than SATE and less uncertainty than PATE.
#'
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_CATE(model_results)
#' }
plot_CATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model, 'cate')
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, 0.9)
  lb <- quantile(pate$pate, 0.1)
  ub.95 <- quantile(pate$pate, 0.975)
  lb.95 <- quantile(pate$pate, 0.025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60', ccolor = 'black') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density',
           linetype = NULL)

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}


#' @title Plot Individual Conditional Average Treatment effects
#' @description Plots a histogram of Individual Conditional Average Treatment effects (ICATE).
#' ICATEs are the difference in each individual's predicted outcome under the treatment and predicted outcome under the control averaged over the individual.
#' Plots of ICATEs are useful to identify potential heterogeneous treatment effects between different individuals. ICATE plots can be grouped by discrete variables.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param .group_by a grouping variable as a vector
#' @param n_bins number of bins
#' @param .alpha transparency of histograms
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr bartCause
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_ICATE(model_results, lalonde$married)
#' }
plot_ICATE <- function(.model, .group_by = NULL, n_bins = 30, .alpha = .7){

  validate_model_(.model)
  if (!is.null(.group_by)) is_discrete_(.group_by)

  posterior <- bartCause::extract(.model, 'icate')
  icates <- as_tibble(apply(posterior, 2, mean))

  # adjust value based on estimand
  .group_by <- adjust_for_estimand_(.model, .group_by)

  # create base plot
  p <- ggplot(icates, aes(x = value)) +
    geom_histogram(bins = n_bins, color = 'black')

  # add grouping
  if(!is.null(.group_by)){
    p <- ggplot(data = icates,
                aes(x = value, fill = as.factor(.group_by))) +
      geom_histogram(position = 'identity', bins = n_bins, alpha = .alpha, col = 'black')
  }

  # add labels
  p <- p +
    labs(title = NULL,
         x = NULL,
         y = 'Count',
         fill = NULL)

  return(p)
}

#' @title Plot histogram or density of Population Average Treatment Effect
#' @description Plot shows the Population Average Treatment Effect which is derived from the posterior predictive distribution of the difference between \eqn{y | z=1, X} and \eqn{y | z=0, X}.
#' Mean of PATE will resemble CATE and SATE but PATE will account for more uncertainty and is recommended for informing inferences on the average treatment effect.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_PATE(model_results)
#' }
plot_PATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model)
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60', color = 'black') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}

#' @title Plot histogram or density of Sample Average Treatment Effects
#' @description Plot a histogram or density of the Sample Average Treatment Effect (SATE). The Sample Average Treatment Effect is derived from taking the difference of each individual's observed outcome and a predicted counterfactual outcome from a BART model averaged over the population.
#' The mean of SATE will resemble means of CATE and PATE but will account for the least uncertainty.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this x-axis value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#' @import ggplot2 bartCause
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_SATE(model_results)
#' }
plot_SATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model, 'sate')
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60', color = 'black') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}

#' @title Plot the balance
#' @description Visualize balance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @param compare character of either means or variance denotes what to compare balance on
#' @param estimand character of either ATE, ATT or ATC the causal estimand you are making inferences about

#' @author George Perrett & Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr patchwork
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn
#' @importFrom stats reorder
#' @examples
#' data(lalonde)

#' plot_balance(lalonde, 'treat', c('re78', 'age', 'educ'), compare = 'means', estimand = 'ATE') + labs(title = 'My new title')
plot_balance <- function(.data, treatment, confounders, compare = c('means', 'variance', 'covariance'), estimand = c('ATE', 'ATT', 'ATC')){
  if(missing(treatment)) stop('enter a string indicating the name of the treatment variable')
  if('factor' %in% sapply(.data[, confounders], class)) stop('factor variables must be converted to numeric or logical indicator variables')
  if('character' %in% sapply(.data[, confounders], class)) stop('factor variables must be converted to numeric or logical indicator variables')

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  # make sure arguments are set
  compare <- match.arg(compare)
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")
  if(compare == 'variance')x_var <- 'variance' else x_var <- 'means'

  # prep the df
  if(compare == 'covariance'){
    # gets interactions of all columns
    cov_dat <- combn(.data[, confounders], 2, FUN = Reduce, f = `*`)

    # get column names so we know what is what
    colnames(cov_dat) <- paste(
      combn(names(.data[, confounders]), 2)[1, ],
      combn(names(.data[, confounders]), 2)[2, ],
      sep = '*')
    # add back treatment
    .data <- cbind.data.frame(cov_dat, treatment = .data[[treatment]])

  }else{
    .data <- .data %>%
      dplyr::select(all_of(c(confounders, treatment))) %>%
      rename(`treatment` = treatment)

  }

  data_types <- apply(.data, 2, function(i) identical(unique(i)[order(unique(i))], c(0, 1))) %>%
    as.data.frame()
  data_types$name<- rownames(data_types)
  names(data_types)[1] <- 'type'

  data_types <- data_types %>%
    mutate(type = if_else(type, 'binary/categorical', 'continuous')) %>%
    arrange(name) %>%
    filter(name != 'treatment')

  # calculate and plot it
  .data <- .data %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(
      variance = var(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    arrange(name)

  .data <- cbind.data.frame(.data, type = data_types$type)

  .data <- .data %>%
    mutate(means =
             case_when(
               estimand == 'ATE' ~ (mean_TRUE - mean_FALSE) / sqrt((variance_TRUE + variance_FALSE) /2),
               estimand == 'ATT' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_TRUE),
               estimand == 'ATC' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_FALSE)
             ),
           variance = sqrt(variance_TRUE/variance_FALSE),
           means = if_else(type != 'continuous', mean_TRUE - mean_FALSE, means)
    )

  # calculate the order before rounding
  if(compare == 'variance'){
    .data <- .data %>%
      mutate(order = if_else(variance < 1, 1/variance, variance))
  }else{
    .data <- .data %>%
      mutate(order = abs(means))
  }

  .data <- .data %>%
    mutate(flag_means = if_else(means > 2 | means < -2, 1, 0),
           flag_variance = if_else(variance > 4 | variance < .25, 1, 0),
           flag = if(compare == 'variance') flag_variance else  flag_means,
           means = if_else(means > 2, 2, means),
           means = if_else(means < -2, -2, means),
           variance = if_else(variance > 4, 4, variance),
           variance = if_else(variance < .25, .25, variance)) %>%
    na.omit()

  p1 <- .data %>%
    filter(type != 'continuous') %>%
    ggplot(aes(
      x = get(x_var),
      y = reorder(name, order))) +
    geom_vline(
      xintercept = ifelse(x_var == 'means', 0, 1),
      linetype = 'dashed',
      color = 'gray60'
    ) +
    geom_point(size = 4) +
    labs(
      x = case_when(
        compare == 'means' ~ 'Mean difference (Proportion)',
        compare == 'variance' ~ 'Ratio of variance (log scale)',
        compare == 'covariance' ~ 'Scaled mean difference of interactions (balance of covaraince)'
      ),
      y = NULL,
      color = NULL
    ) +
    coord_cartesian(xlim = c(-1, 1)) +
    theme(legend.position = 'none')

  p2 <- .data %>%
    filter(type == 'continuous') %>%
    ggplot(aes(
      x = get(x_var),
      y = reorder(name, order),
      col = as.factor(flag)
    )) +
    geom_vline(
      xintercept = ifelse(x_var == 'means', 0, 1),
      linetype = 'dashed',
      color = 'gray60'
    ) +
    geom_point(size = 4) +
    scale_color_manual(values = c('black', 'red')) +
    labs(
      x = case_when(
        compare == 'means' ~ 'Standardized mean difference',
        compare == 'variance' ~ 'Ratio of variance (log scale)',
        compare == 'covariance' ~ 'Standardized mean difference of interactions (balance of covaraince)'
      ),
      y = NULL,
      color = NULL
    ) +
    theme(legend.position = 'none')

  if (compare == 'variance'){
    p <- p2 +
      coord_cartesian(xlim = c(.25, 4)) +
      scale_x_continuous(trans='log10') +
      labs(
        title = 'Balance',
        subtitle = if_else(
          max(.data$flag) != 0,
          'points represent the treatment group\nred points are beyond 4 times different',
          'points represent the treatment group'
        )
      )
  }else{
    p2 <-p2 + coord_cartesian(xlim = c(-2, 2))
    if(length(unique(.data$type)) > 1){
      p1 <- p1 + facet_wrap(~type)
      p2 <- p2 + facet_wrap(~type)

      p1 <- p1 + labs(
        title = 'Balance',
        subtitle = if_else(
          max(.data$flag) != 0,
          'points represent the treatment group\nred points are beyond 2 standard deviations.',
          'points represent the treatment group'
        )
      )

      p <- p1 + p2

    }else if(unique(.data$type) == 'continuous'){
      p <- p2 + labs(
        title = 'Balance',
        subtitle = if_else(
          max(.data$flag) != 0,
          'points represent the treatment group\nred points are beyond 2 standard deviations.',
          'points represent the treatment group'
        )
      )
    }else{
      p <- p1 + labs(
        title = 'Balance',
        subtitle =  'points represent the treatment group')
    }

  }

  return(p)

}

#' @title Print balance statistics
#' @description See balance statisitics of variables between treatment and control groups.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @param estimand character of either ATE, ATT or ATC the causal estimand you are making inferences about
#' @author George Perrett
#'
#' @return tibble
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stats var
#'
#' @examples
#' data(lalonde)
#' print_balance(lalonde, 'treat', confounders = c('re78', 'age', 'educ'), estimand = 'ATE')

print_balance <- function(.data, treatment, confounders, estimand = c('ATE', 'ATT', 'ATC')){

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])
  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")

  data_types <- apply(.data[, confounders], 2, function(i) identical(unique(i)[order(unique(i))], c(0, 1))) %>%
    as.data.frame()
  data_types$name<- rownames(data_types)
  names(data_types)[1] <- 'type'

  data_types <- data_types %>%
    mutate(type = if_else(type, 'binary/categorical', 'continuous')) %>%
    arrange(name)

  table <- .data %>%
    dplyr::select(all_of(c(confounders, treatment))) %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    mutate(
      raw_means = mean_TRUE - mean_FALSE,
      means =
        case_when(
          estimand == 'ATE' ~ (mean_TRUE - mean_FALSE) / sqrt((variance_TRUE + variance_FALSE) /2),
          estimand == 'ATT' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_TRUE),
          estimand == 'ATC' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_FALSE)
        ),
      variance = sqrt(variance_TRUE/variance_FALSE)
    ) %>%
    na.omit() %>%
    dplyr::select(name, raw_means, means, variance) %>%
    rename(variable = name,
           `difference in means` = raw_means,
           `standardized difference in means` = means,
           `ratio of the variance` = variance) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    arrange(variable)

  table$`standardized difference in means` <- as.character(table$`standardized difference in means`)
  table$`ratio of the variance` <- as.character(table$`ratio of the variance`)
  table[data_types$type != 'continuous', 3] <- '--'
  table[data_types$type != 'continuous', 4] <- '--'

  return(table)

}


#' @title Print covariance statistics
#' @description See balance statistics of covariance for specified variables between treatment and control groups.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @param estimand character of either ATE, ATT or ATC the causal estimand you are making inferences about
#' @author George Perrett
#'
#' @return tibble
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stats var
#'
#' @examples
#' data(lalonde)
#' print_covariance(lalonde, 'treat', confounders = c('re78', 'age', 'educ'), estimand = 'ATE')

print_covariance <- function(.data, treatment, confounders, estimand = c('ATE', 'ATT', 'ATC')){

  if(missing(treatment)) stop('enter a string indicating the name of the treatment variable')
  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  estimand <- match.arg(estimand)
  estimand <- toupper(estimand)
  classes <- sapply(.data[, confounders], class)

  if (estimand %notin% c('ATE', 'ATT', 'ATC')) stop("estimand must be either: ATE, ATT or ATC")
  # gets interactions of all columns
  cov_dat <- combn(.data[, confounders], 2, FUN = Reduce, f = `*`)

  # get column names so we know what is what
  colnames(cov_dat) <- paste(
    combn(names(.data[, confounders]), 2)[1, ],
    combn(names(.data[, confounders]), 2)[2, ],
    sep = '*')
  # add back treatment
  .data <- cbind.data.frame(cov_dat, treatment = .data[[treatment]])

  table <- .data %>%
    pivot_longer(cols = -treatment) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              variance = var(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = treatment, values_from = c(variance, mean)) %>%
    mutate(
      raw_means = mean_TRUE - mean_FALSE,
      means =
        case_when(
          estimand == 'ATE' ~ (mean_TRUE - mean_FALSE) / sqrt((variance_TRUE + variance_FALSE) /2),
          estimand == 'ATT' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_TRUE),
          estimand == 'ATC' ~ (mean_TRUE - mean_FALSE) / sqrt(variance_FALSE)
        ),
      variance = sqrt(variance_TRUE/variance_FALSE)
    ) %>%
    na.omit() %>%
    dplyr::select(name, means) %>%
    arrange(desc(abs(means))) %>%
    rename(variable = name,`standardized difference in means` = means) %>%
    mutate(across(where(is.numeric), round, 2))

  return(table)

}


#' @title Plot the covariance
#' @description Visualize balance of the covariance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest

#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @examples
#' data(lalonde)

#' plot_covariance(lalonde, 'treat', c('re75','re74' , 'age', 'educ')) + labs(title = 'My new title')

plot_covariance <- function(.data, treatment, confounders){
  .data$treatment <- .data[[treatment]]
  #.data[, confounders] <- apply(.data[, confounders], 2, function(i) (i - mean(i))/sd(i))
  .data %>%
    GGally::ggpairs(
      upper = list(continuous = "density", combo = "box_no_facet"),
      lower = list(continuous = "points",  combo = "box_no_facet"),
      columns = confounders,
      aes(colour= as.factor(treatment), alpha = .7)) +
    scale_color_manual(values = c('blue', 'red')) +
    scale_fill_manual(values = c('blue', 'red'))
}

#' @title Plot common support based on the standard deviation rule, chi squared rule, or both
#' @description Plot common support based on the standard deviation rule, chi squared rule, or both.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param .x a character string denoting which covariate to use a the x axis default is propensity score
#' @param rule one of c('both', 'sd', 'chi') denoting which rule to use to identify lack of support
#'
#' @details Sufficient overlap/common support is an assumption of causal inference.
#' BART models use the uncertainty of counter factual uncertainty.
#' When the posterior distribution of an individual's counterfactual prediction extends beyond a specified cut-point, that point likely has insufficient common support.
#' 'bartCause' model offer the option to automatically remove points without common support from analyses, however, this must be specified during model fitting.
#' Cut-points are determined through one of two rules: the standard deviation (sd) or chi-squared (chi).
#' Under the standard deviation rule, a point has weak common support if its posterior distribution of the counterfactual deviation is greater than the maximum posterior of the observed predictions with 1 standard deviation of the distribution of standard deviations for each individual's predicted outcome under the observed assignment.
#' Under the chi-squared rule, a point is discarded if the variance between its counterfactual prediction over observed prediction are statistically different under a chi-squared distribution with 1 degree of freedom. For more details on discard rules see Hill and Su 2013.
#'
#' When called this plot will show how many points would have been removed under the standard deviation and chi-squared rules. This plot should be used as a diagnostic for 'bartCause' models fit without a common-support rule.
#'
#' @references
#' Hill, J., & Su, Y. S. (2013).
#' Assessing lack of common support in causal inference using Bayesian nonparametrics: Implications for evaluating the effect of breastfeeding on children's cognitive outcomes.
#' The Annals of Applied Statistics,
#' 1386-1420.
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom rlang sym
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_common_support(model_results)
#' plot_common_support(model_results, .x = 'age')
#' }
plot_common_support <- function(.model, .x = 'Propensity Score',  rule = c('both', 'sd', 'chi')){

  # ensure model is a of class bartcFit
  validate_model_(.model)

  rule <- rule[1]
  if (rule %notin% c('both', 'sd', 'chi')) stop('rule must be one of c("both", "sd", "chi")')
  if (rule == 'both') rule <- c('sd', 'chi')

  # calculate summary stats
  sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])
  total_sd <- switch (.model$estimand,
                      ate = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]) + sum(.model$sd.cf[.model$trt==0] > sd.cut[2]),
                      att = sum(.model$sd.cf[.model$trt==1] > sd.cut[1]),
                      atc = sum(.model$sd.cf[.model$trt==0] > sd.cut[2])
  )

  inference_group <- switch (.model$estimand,
                             ate = length(.model$sd.obs[!.model$missingRows]),
                             att = sum(!.model$missingRows & .model$trt == 1),
                             atc = sum(!.model$missingRows & .model$trt == 0)
  )

  prop_sd <- round((total_sd / inference_group)*100 , 2)
  text_sd <- paste0('Standard deviation rule: ', prop_sd, "% of cases would have been removed")

  # calculate summary stats
  total_chi <- switch (.model$estimand,
                       ate = sum((.model$sd.cf / .model$sd.obs) ** 2 > 3.841),
                       att = sum((.model$sd.cf[.model$trt == 1] / .model$sd.obs[.model$trt == 1]) ** 2 > 3.841),
                       atc = sum((.model$sd.cf[.model$trt == 0] / .model$sd.obs[.model$trt == 0]) ** 2 > 3.841)
  )

  prop_chi <- round(total_chi / inference_group, 2)*100
  text_chi <- paste0('Chi-squared rule: ', prop_chi, "% of cases would have been removed")

  # create dataframe of the sd and chi values
  dat <- as_tibble(.model$data.rsp@x) %>%
    rename(`Propensity Score` = ps)

  dat.sd <- dat %>%
    mutate(sd.cut = if_else(.model$trt == 1, sd.cut[1], sd.cut[2]),
           removed = if_else(.model$sd.cf > sd.cut, 'Removed', 'Included'),
           support_rule = 'sd',
           stat = .model$sd.cf,
           sd.cf = .model$sd.cf,
           support_rule_text = text_sd) %>%
    select(-sd.cut)


  dat.chi <- dat %>%
    mutate(removed = if_else((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 'Removed', 'Included'),
           support_rule = 'chi',
           stat = (.model$sd.cf / .model$sd.obs) ** 2,
           sd.cf = .model$sd.cf,
           support_rule_text = text_chi)

  dat <- rbind(dat.sd, dat.chi)

  if(.model$estimand == 'att') dat <- dat[rep(.model$trt, 2) == 1,]
  if(.model$estimand == 'atc') dat <- dat[rep(.model$trt, 2) == 0,]

  # plot it
  p <- dat %>%
    filter(support_rule %in% rule) %>%
    ggplot(aes(x = !!rlang::sym(.x), y = sd.cf, color = removed)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c(1, 2)) +
    facet_wrap(~support_rule_text, ncol = 1, scales = 'free_y') +
    labs(title ="Common support checks",
         x = .x,
         y = 'Predicted counterfactual standard deviation',
         color = NULL) +
    theme(legend.position = 'bottom',
          strip.text = element_text(hjust = 0))

  return(p)
}


plot_predicted_common_support <- function(.model, max_depth = 3, rule = c('both', 'sd', 'chi')){

  # ensure model is a of class bartcFit
  validate_model_(.model)

  rule <- rule[1]
  if (rule %notin% c('both', 'sd', 'chi')) stop('rule must be one of c("both", "sd", "chi")')


  # calculate summary stats
  sd.cut = c(trt = max(.model$sd.obs[!.model$missingRows & .model$trt > 0]), ctl = max(.model$sd.obs[!.model$missingRows & .model$trt <= 0])) + sd(.model$sd.obs[!.model$missingRows])

  # create dataframe of the sd and chi values
  dat <- as_tibble(.model$data.rsp@x)

  dat.sd <- dat %>%
    mutate(sd.cut = if_else(.model$trt == 1, sd.cut[1], sd.cut[2]),
           removed = if_else(.model$sd.cf > sd.cut, 1, 0)) %>%
    select(-sd.cut)

  dat.chi <- dat %>%
    mutate(removed = if_else((.model$sd.cf / .model$sd.obs) ** 2 > 3.841, 1, 0))


  dat.chi <- switch (.model$estimand,
                     ate = dat.chi[, names(dat.chi) %notin% c('ps', .model$name.trt)],
                     att = dat.chi[.model$trt == 1, names(dat.chi) %notin% c('ps', .model$name.trt)],
                     atc = dat.chi[.model$trt == 0, names(dat.chi) %notin% c('ps', .model$name.trt)])

  dat.sd <- switch (.model$estimand,
                    ate = dat.sd[, names(dat.sd) %notin% c('ps', .model$name.trt)],
                    att = dat.sd[.model$trt == 1,names(dat.sd) %notin% c('ps', .model$name.trt)],
                    atc = dat.sd[.model$trt == 0,names(dat.sd) %notin% c('ps', .model$name.trt)])


  chi.tree <- rpart::rpart(removed ~ ., dat.chi, maxdepth = max_depth)
  chi.p <- tryCatch(
    rpart_ggplot_(chi.tree) +
      labs(
        title = 'Predictors of Non-Overlap',
        subtitle = 'y = probability of removal\nn = cases per group',
        x = paste('Chi-Squared Rule\nCases removed due to lack of overlap: ',
                  sum(dat.chi$removed))
      ),
    error = function(e)
      FALSE
  )

  sd.tree <- rpart::rpart(removed ~ ., dat.sd, maxdepth = max_depth)
  sd.p <- tryCatch(
    rpart_ggplot_(sd.tree) + labs(
      title = 'Predictors of Non-Overlap',
      subtitle = 'y = probability of removal\nn = cases per group',
      x = paste('Standard Deviation Rule\nCases removed due to lack of overlap: ',
                sum(dat.sd$removed))
    ),
    error = function(e)
      FALSE
  )
  p <- switch (rule,
               both =
                 if (isFALSE(sd.p) &isFALSE(chi.p)) {
                   p <- message('no cases were removed under the standard deviation or chi-squared rules')
                 } else if (!isFALSE(sd.p) & !isFALSE(chi.p)) {
                   chi.p <- chi.p + labs(title = NULL, subtitle = NULL)
                   p <- sd.p + chi.p
                 } else if (isFALSE(sd.p)) {
                   p <- chi.p
                 } else{
                   p <- sd.p
                 },
               sd =
                 if (isFALSE(sd.p)) {
                   p <- message('no cases were removed under the standard deviation rule')
                 } else{
                   p <- sd.p
                 },
               chi =
                 if (isFALSE(chi.p)) {
                   p <- message('no cases were removed under the chi squared rule')
                 } else{
                   p <- chi.p
                 }
  )

  return(p)
}

#' @title Partial dependency plot of a continuous moderating variable
#' @description Plot a partial dependency plot with a continuous covariate from a 'bartCause' model. Identify treatment effect variation predicted across levels of a continuous variable.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param n_bins number of bins to cut the moderator with. Defaults to the lesser of 15 and number of distinct levels of the moderator
#' @details Partial dependency plots are one way to evaluate heterogeneous treatment effects that vary by values of a continuous covariate. For more information on partial dependency plots from BART causal inference models see Green and Kern 2012.
#' @author George Perrett, Joseph Marlo
#'
#' @references
#' Green, D. P., & Kern, H. L. (2012).
#' Modeling heterogeneous treatment effects in survey experiments with Bayesian additive regression trees.
#' Public opinion quarterly, 76(3), 491-511.
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom stats density median na.omit predict quantile sd
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none',
#'  keepTrees = TRUE
#' )
#' plot_moderator_c_pd(model_results, lalonde$age)
#' }
plot_moderator_c_pd <- function(.model, moderator, n_bins = NULL){

  validate_model_(.model)

  # validate n_bins argument
  n_mod_levels <- n_distinct(moderator)
  if (n_mod_levels <= 1) stop('dplyr::n_distinct(moderator) must be at least 2')
  if (is.null(n_bins)) n_bins <- pclamp_(15, 2, n_mod_levels)
  if (!(n_bins > 1 & n_bins <= n_mod_levels)) stop("n_bins must be greater than 1 and less than or equal to dplyr::n_distinct(moderator)")

  # extract data from model
  new_data <- as_tibble(.model$data.rsp@x)

  # create dataframe where all observations are treated
  new_data_z1 <- new_data
  name_trt <- .model$name.trt
  new_data_z1[, name_trt] <- 1
  new_data_z1 <- cbind.data.frame(y_response = .model$fit.rsp$y, new_data_z1)

  # create dataframe where all observations are control
  new_data_z0 <- new_data
  new_data_z0[, name_trt] <- 0
  new_data_z0 <- cbind.data.frame(y_response = .model$fit.rsp$y, new_data_z0)

  # locate the moderator in bartc data
  search_moderator <- function(x) sum(moderator - x)
  index <- which(lapply(new_data_z0, search_moderator) == 0)
  if (!isTRUE(index > 0)) stop('Cannot find moderator in original data. Is moderator within the original dataframe used to fit the .model?')

  # get range for predictions
  cut <- n_bins-1
  p <- seq(min(moderator), max(moderator), (max(moderator) - min(moderator))/cut)
  if (length(p) < n_distinct(moderator)) {
    .range <- p
  } else{
    .range <- unique(moderator)[order(unique(moderator))]
  }

  # predict new data with overridden treatment columns
  cates <- lapply(.range, fit_pd_, z1 = new_data_z1, z0 = new_data_z0, index = index, .model = .model)
  names(cates) <- seq_along(cates)
  cates <- bind_cols(cates)
  cates.m <- apply(cates, MARGIN = 2, FUN = mean)
  cates.m <- bind_cols(cates.m = cates.m, .range = .range)

  # get credible intervals
  ci_range <- c(0.025, 0.1, 0.9, 0.975)
  cates.ci <- as_tibble(t(apply(cates, MARGIN = 2, FUN = quantile, probs = ci_range)))
  cates_plot <- bind_cols(cates.m, cates.ci)
  indices <- 2 + seq_along(ci_range)
  colnames(cates_plot)[indices] <- paste0('ci_', as.character(ci_range * 100))

  # plot it
  p <- ggplot(cates_plot) +
    geom_ribbon(aes(x = .range, y = cates.m, ymin = ci_2.5, ymax = ci_97.5, fill = '95% ci')) +
    geom_ribbon(aes(x = .range, y = cates.m, ymin = ci_10, ymax = ci_90, fill = '80% ci')) +
    scale_fill_manual(values = c('grey40', 'grey60')) +
    geom_point(aes(x = .range, y = cates.m), size = 2) +
    geom_line(aes(x = .range, y = cates.m)) +
    labs(title = NULL,
         x = NULL,
         y = 'CATE') +
    theme(legend.position = "bottom")

  return(p)
}


#' @title LOESS plot of a continuous moderating variable
#' @description Plot the LOESS prediction of ICATEs by a continuous covariate. This is an alternative to partial dependency plots to assess treatment effect heterogeneity by a continuous covariate. See Carnegie, Dorie and Hill 2019.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param line_color the color of the loess line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @references
#' Carnegie, N., Dorie, V., & Hill, J. L. (2019).
#' Examining treatment effect heterogeneity using BART.
#' Observational Studies, 5(2), 52-70.
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_c_loess(model_results, lalonde$age)
#' }
plot_moderator_c_loess <- function(.model, moderator, line_color = 'blue'){

  validate_model_(.model)
  is_numeric_vector_(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, rowMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- moderator[order(moderator)]
  rownames(dat) <- seq_len(nrow(dat))

  # plot it
  p <- ggplot(dat, aes(moderator, value)) +
    geom_point() +
    geom_smooth(method = 'loess',
                formula = y ~ x,
                se = TRUE,
                size = 1.5,
                color = line_color) +
    labs(title = NULL,
         x = NULL,
         y = 'icate')

  return(p)
}

#' @title Plot the Conditional Average Treatment Effect conditional on a discrete moderator
#' @description Plot the Conditional Average Treatment Effect split by a discrete moderating variable. This plot will provide a visual test of moderation by discrete variables.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param .alpha transparency value [0, 1]
#' @param facet TRUE/FALSE. Create panel plots of each moderator level?
#' @param .ncol number of columns to use when faceting
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_d(model_results, lalonde$educ)
#' }
plot_moderator_d <- function(.model, moderator, type = c('density', 'histogram', 'errorbar'), .alpha = 0.7, facet = FALSE, .ncol = 1){
  type <- type[1]
  validate_model_(.model)
  is_discrete_(moderator)
  if(type %notin%c('density', 'histogram', 'errorbar')) stop('type must be either density, histogram errorbar')
  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  estimand <- switch (.model$estimand,
                      ate = 'CATE',
                      att = 'CATT',
                      atc = 'CATC'
  )
  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')

  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- seq_len(nrow(dat))

  # plot it
  p <- ggplot(dat, aes(value, fill = moderator))

  if(type == 'density'){
    p <- p + geom_density(alpha = .alpha) +
      labs(title = NULL,
           x = estimand,
           y = NULL) +
      theme(legend.position = 'bottom')
  }else if(type == 'histogram'){
    p <- p +
      geom_histogram(
        alpha = .alpha,
        col = 'black',
        position = 'identity') +
      labs(title = NULL,
           x = estimand,
           y = NULL) +
      theme(legend.position = 'bottom')
  } else{
    # tidy up the data
    dat <- dat %>%
      group_by(moderator) %>%
      mutate(.min = quantile(value, .025),
             .max = quantile(value, .975),
             point = mean(value)) %>%
      dplyr::select(-value) %>%
      arrange(desc(point)) %>%
      ungroup() %>%
      distinct()

    # plot it
    p <- ggplot(dat, aes(x = moderator, y = point, color = moderator)) +
      geom_point(size = 2) +
      geom_linerange(aes(ymin = .min, ymax = .max), alpha = .alpha) +
      labs(title = NULL,
           x = element_blank(),
           y = estimand) +
      theme(legend.position = 'bottom')
  }


  # add faceting
  if(isTRUE(facet)){
    p <- p + facet_wrap(~moderator, ncol = .ncol)
  }

  return(p)
}



#' @title Plot the posterior interval of the Conditional Average Treatment Effect grouped by a discrete variable
#' @description Plots the range of the Conditional Average Treatment Effect grouped by a discrete variable. This is analogous to plot_moderator_d_density but is preferable for moderators with many categories. Rather than plotting the full density, the posterior range is shown.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param .alpha transparency value [0, 1]
#' @param horizontal flip the plot horizontal?
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_d_linerange(model_results, lalonde$educ)
#' }
plot_moderator_d_linerange <- function(.model, moderator, .alpha = 0.7, horizontal = FALSE){

  validate_model_(.model)
  is_discrete_(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior  %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- seq_len(nrow(dat))

  # tidy up the data
  dat <- dat %>%
    group_by(moderator) %>%
    mutate(.min = quantile(value, .025),
           .max = quantile(value, .975),
           point = mean(value)) %>%
    dplyr::select(-value) %>%
    arrange(desc(point)) %>%
    ungroup() %>%
    distinct()

  # plot it
  p <- ggplot(dat, aes(x = moderator, y = point, color = moderator)) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = .min, ymax = .max), alpha = .alpha) +
    labs(title = NULL,
         x = element_blank(),
         y = 'CATE') +
    theme(legend.position = 'bottom')

  if (horizontal) p <- p + coord_flip()

  return(p)
}

#' @title Plot a single regression tree of covariates on ICATEs
#' @description Plot a single regression tree for exploratory heterogeneous effects. Fit single regression tree on bartc() ICATEs to produce variable importance plot. This plot is useful for identifying potential moderating variables.
#' Tree depth may be set to depths 1, 2 or 3. Terminal nodes signal the Conditional Average Treatment effect within levels of moderation variables. Trees with different values across terminal nodes suggest strong treatment effect moderation.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param max_depth one of c(1, 2, 3). Maximum number of node levels within the tree. 2 is recommended
#'
#' @author George Perrett, Joseph Marlo
#'
#' @import ggplot2 dplyr
#' @importFrom ggdendro dendro_data
#' @importFrom rpart rpart
#' @importFrom bartCause extract
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_moderator_search(model_results)
#' }
plot_moderator_search <- function(.model, max_depth = c(2, 1, 3)){

  validate_model_(.model)

  max_depth <- max_depth[[1]]
  if (max_depth %notin% c(2, 1, 3)) stop('max_depth must be one of c(1, 2, 3)')

  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)

  # pull data from model and create a matrix of confounders
  .data <- as.data.frame(.model$data.rsp@x)

  # adjust data for estimand
  if (.model$estimand == 'ate') {
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else if (.model$estimand == 'att') {
    .data <- .data[.model$trt == 1,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else{
    .data <- .data[.model$trt == 0,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  }

  # fit regression tree
  cart <- rpart::rpart(icate.m ~ ., data = as.data.frame(confounders), maxdepth = max_depth)
  # p <- rpart.plot::rpart.plot(cart, type = 2, branch = 1, box.palette = 0)

  # create dendrogram
  p_gg <- rpart_ggplot_(cart)

  return(p_gg)
}


rpart_ggplot_ <- function(.model){

  # remove depth information from model so resulting plot is easy to read
  .model$frame$dev <- 1

  # extract data to construct dendrogram
  fitr <- ggdendro::dendro_data(.model)
  n_leaf <- .model$frame$n[.model$frame$var == '<leaf>']
  n_split <- .model$frame$n[.model$frame$var != '<leaf>']
  pred_split <- round(.model$frame$yval[.model$frame$var != '<leaf>'], 1)
  terminal_leaf_y <- 0.1
  leaf_labels <- tibble(
    x = fitr$leaf_labels$x,
    y = terminal_leaf_y,
    label = paste0(
      'y = ', fitr$leaf_labels$label,
      '\nn = ', n_leaf)
  )
  yes_no_offset <- c(0.75, 1.25)
  yes_no <- tibble(
    x = c(fitr$labels$x[[1]] * yes_no_offset[1],
          fitr$labels$x[[1]] * yes_no_offset[2]),
    y = rep(fitr$labels$y[[1]], 2),
    label = c("yes", "no")
  )
  split_labels <- tibble(
    x = fitr$labels$x,
    y = fitr$labels$y + 0.07,
    label = paste0(
      'y = ', pred_split,
      '\nn = ', n_split
    )
  )

  # set terminal segments to y = terminal_leaf_y
  initial_node_y <- fitr$labels$y[[1]]
  fitr$segments <- fitr$segments %>%
    mutate(y_new = ifelse(y > yend, y, yend),
           yend_new = ifelse(yend < y, yend, y)) %>%
    select(n, x, y = y_new, xend, yend = yend_new) %>%
    mutate(y = ifelse(y > initial_node_y, terminal_leaf_y, y),
           yend = ifelse(x == xend & x == round(x) & y > yend, terminal_leaf_y, yend))

  # set plot constants
  label_text_size <- 3
  x_limits <- c(0.5, nrow(fitr$leaf_labels) + 0.5)
  y_limits <- c(min(fitr$segments$y) - 0.05,
                max(fitr$segments$y) + 0.15)

  # plot it
  p <- ggplot() +
    geom_segment(data = fitr$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_label(data = yes_no,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = leaf_labels,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = split_labels,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = fitr$labels,
               aes(x = x, y = y, label = label),
               label.size = NA, fontface = 'bold') +
    expand_limits(x = x_limits,
                  y = y_limits) +
    scale_x_continuous(labels = NULL, breaks = NULL) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
    labs(title = 'Exploratory heterogeneous effects',
         x = NULL,
         y = NULL) +
    theme(panel.background = element_blank())

  return(p)
}
#' @title Plot the overlap via propensity score method
#' @description Plot histograms showing the overlap between propensity scores by treatment status.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param min_x numeric value specifying the minimum propensity score value to be shown on the x axis
#' @param max_x numeric value specifying the maximum propensity score value to be shown on the x axis
#' @param plot_type the plot type, one of c('Histogram', 'Density')
#' @param pscores propensity scores. If not provided, then propensity scores will be calculated using BART
#' @param \dots additional arguments passed to `dbarts::bart2` propensity score calculation
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @seealso \code{\link{plot_overlap_vars}}
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' plot_overlap_pScores(
#'  .data = lalonde,
#'  treatment = 'treat',
#'  confounders = c('age', 'educ'),
#'  plot_type = 'histogram',
#'  pscores = NULL,
#'  seed = 44
#')
#'}
plot_overlap_pScores <- function(.data, treatment, confounders, plot_type = c("histogram", "density"),min_x = NULL, max_x = NULL, pscores = NULL, ...) {

  plot_type <- tolower(plot_type[[1]])
  if (plot_type %notin% c('histogram', 'density')) stop('plot_type must be one of c("histogram", "density"')
  if (!is.null(pscores) & !inherits(pscores, 'numeric')) stop('propensity_scores must be a numeric vector')

  # calculate propensity scores from bart model
  if (is.null(pscores)){
    pscores <- propensity_scores(
      .data = .data,
      treatment = treatment,
      confounders = confounders,
      ...
    )
  }

  dat <- data.frame(Z = coerce_to_logical_(.data[[treatment]]),
                    pscores = pscores)

  if(!is.null(min_x)) {dat <- dat %>% filter(pscores >= min_x)}
  if(!is.null(max_x)) {dat <- dat %>% filter(pscores <= max_x)}


  if (plot_type == 'histogram'){

    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_histogram(data = filter(dat, Z == 1),
                     aes(x = pscores, y = ..count.., fill = Z),
                     alpha = 0.8) +
      geom_histogram(data = filter(dat, Z == 0),
                     aes(x = pscores, y = -..count.., fill = Z),
                     alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c(4,2)) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Count',
           fill = "Treatment")

  }

  if (plot_type == 'density') {

    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_density(data = filter(dat, Z == 1),
                   aes(x = pscores, y = ..density.., fill = Z),
                   alpha = 0.8) +
      geom_density(data = filter(dat, Z == 0),
                   aes(x = pscores, y = -..density.., fill = Z),
                   alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c(4,2)) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Count',
           fill = "Treatment")

  }

  return(p)
}

#' @title Calculate propensity scores using BART
#' @description Calculates propensity scores using Bayesian Additive Regression Trees via `bartCause::bartc()`.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param \dots additional arguments passed to `dbarts::barts()`
#'
#' @return a numeric vector of propensity scores
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dbarts bart2
#'
#' @seealso \code{\link{plot_overlap_pScores}}
propensity_scores <- function(.data, treatment, confounders, ...){

  if (treatment %notin% colnames(.data)) stop('treatment not found in .data')
  if (any(confounders %notin% colnames(.data))) stop('Not all confounders are found in .data')
  if('factor' %in% sapply(.data[, confounders], class)) stop('factor and character variables must be converted to numeric or logical indicator variables')
  if('character' %in% sapply(.data[, confounders], class)) stop('factor and character variables must be converted to numeric or logical indicator variables')

  # coerce treatment column to logical
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  # run the Bart model
  confounders_mat <- as.matrix(.data[, confounders])
  dim.red_results <- dbarts::bart2(.data[[treatment]] ~ confounders_mat)
  p.score <- apply(dbarts::extract(dim.red_results, 'ev'), 2, mean)

  return(p.score)
}

#' @title Plot the overlap of variables
#' @description Plot histograms showing the overlap between variables by treatment status.
#'
#' @param .data dataframe
#' @param treatment character. Name of the treatment column within .data
#' @param confounders character list of column names denoting confounders within .data
#' @param plot_type the plot type, one of c('histogram', 'density'). Defaults to 'histogram'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @seealso \code{\link{plot_overlap_pScores}}
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' data(lalonde)
#' plot_overlap_vars(
#'  .data = lalonde,
#'  treatment = 'treat',
#'  confounders = c('age', 'educ'),
#'  plot_type = 'Histogram'
#')
plot_overlap_vars <- function(.data, treatment, confounders, plot_type = c("histogram", "density")){

  plot_type <- tolower(plot_type[1])
  if (plot_type %notin% c('histogram', 'density')) stop('plot_type must be one of c("histogram", "density"')
  if (treatment %notin% colnames(.data)) stop('treatment not found in .data')
  if (any(confounders %notin% colnames(.data))) stop('Not all confounders are found in .data')

  # coerce treatment column to logical
  .data[[treatment]] <- coerce_to_logical_(.data[[treatment]])

  # extract the relevant columns and rename treatment column
  .data <- .data[, c(treatment, confounders)]
  colnames(.data) <- c("Z_treat", confounders)

  # pivot the data
  dat_pivoted <- pivot_longer(.data, cols = -Z_treat)

  if (plot_type == 'histogram'){
    # histograms showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60')

    if(is.numeric(dat_pivoted$value)){
      p <- p + geom_histogram(data = filter(dat_pivoted, Z_treat == 1),
                              aes(x = value, y = ..count.., fill = Z_treat),
                              alpha = 0.8) +
        geom_histogram(data = filter(dat_pivoted, Z_treat == 0),
                       aes(x = value, y = -..count.., fill = Z_treat),
                       alpha = 0.8)
    }else{
      p <- p + geom_bar(data = filter(dat_pivoted, Z_treat == 1),
                        aes(x = value, y = ..count.., fill = Z_treat),
                        alpha = 0.8) +
        geom_bar(data = filter(dat_pivoted, Z_treat == 0),
                 aes(x = value, y = -..count.., fill = Z_treat),
                 alpha = 0.8)
    }
    p <- p +  scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c(4, 2)) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Count',
           fill = 'Treatment')

  }

  if (plot_type == 'density') {
    if (is.character(dat_pivoted$value)) stop('Density plots are unavalable for character variables')

    # density plots showing overlaps
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_density(data = filter(dat_pivoted, Z_treat == 1),
                   aes(x = value, y = ..density.., fill = Z_treat),
                   alpha = 0.8)+
      geom_density(data = filter(dat_pivoted, Z_treat == 0),
                   aes(x = value, y = -..density.., fill = Z_treat),
                   alpha = 0.8) +
      scale_y_continuous(labels = function(lbl) abs(lbl)) +
      scale_fill_manual(values = c(4, 2)) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           x = NULL,
           y = 'Density',
           fill = "Treatment")

  }

  return(p)
}

# plot density of residual (predicted y - observed y)
plot_residual_density <- function(.model){

  # ensure model is a of class bartcFit
  validate_model_(.model)

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
plot_residual_predicted <- function(.model, covariate = NULL){
  # check if model is bartCause
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
  if(rlang::is_null(covariate)){
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
#' @title Trace plot the estimands of a `bartCause::bartc()` model
#' @description Returns a ggplot of the estimated effect over each iteration of the model fit. This is used to visually assess the convergence of Markov chain Monte Carlo (MCMC) sampling. Chains should be well mixed such that no single color is notably separate from others.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @author Joseph Marlo, George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_trace(.model = model_results)
#' }
plot_trace <- function(.model){

  # ensure model is a of class bartcFit
  validate_model_(.model)

  n_chains <- seq_len(.model$n.chains)

  p <- .model %>%
    bartCause::extract('cate', combineChains = FALSE) %>%
    t() %>%
    as.data.frame() %>%
    tibble() %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(n_chains) %>%
    mutate(Chain = factor(sub('V', '', name), levels = as.character(n_chains))) %>%
    ggplot(aes(x = iteration, y = value, color = Chain)) +
    geom_line(alpha = 0.8) +
    labs(title = 'Diagnostics: Trace plot',
         x = 'Iteration',
         y = toupper(.model$estimand),
         color = 'Chain')

  return(p)
}
#' @title Plot a waterfall of the ICATEs
#' @description Plots the point and posterior intervals of each individual's ICATE ordered by the ICATE or a continuous variable. Points can be colored by a discrete variable.
#' Waterfall plots are a useful visual diagnostic of possible treatment effect heterogeneity.
#' A flat line implies little treatment effect heterogeneity while a steeper curve implies that the treatment effect varies across individuals in the sample. Ordering points by a continuous variable or coloring points by a discrete variable can be helpful to identify potential moderators of the treatment effect.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param descending order the ICATEs by value?
#' @param .order a vector representing a custom order
#' @param .color a vector representing colors
#' @param .alpha transparency value [0, 1]
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSuprule = 'none'
#' )
#' plot_waterfall(model_results)
#' }
plot_waterfall <- function(.model, descending = TRUE, .order = NULL, .color = NULL, .alpha = 0.5){

  validate_model_(.model)
  if(!is.null(.color)){
    if (!is.vector(.color)) stop(".color must be a vector")
    if (nrow(.model$data.rsp@x) != length(.color)) stop(paste(".color must be a vector of length", nrow(.model$data.rsp@x)))
  }

  # calculate stats
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  icate.m <- apply(posterior, 1, mean)
  icate.sd <- apply(posterior, 1, sd)
  icate.uci <- icate.m + icate.sd * 1.96
  icate.lci <- icate.m - icate.sd * 1.96

  dat <- tibble(icate.m, icate.lci, icate.uci)

  if(!is.null(.color)){
    .color <- adjust_for_estimand_(.model, .color)
    dat$.color <- .color
  }
  # specify order of icates on x axis
  if(isTRUE(descending)){
    dat <- dat %>% arrange(desc(icate.m))
  } else if(!is.null(.order)){
    if(isTRUE(descending)){
      dat <- arrange(dat, desc(.order))
    } else{
      dat <- arrange(dat, .order)
    }
  } else{
    dat <- arrange(dat, icate.m)
  }

  dat <- mutate(dat, icate.o = row_number())

  # create base plot
  p <- ggplot(dat, aes(x = icate.o, y = icate.m)) +
    geom_linerange(aes(ymin = icate.lci, ymax = icate.uci),
                   alpha = .alpha) +
    geom_point() +
    labs(title = NULL,
         x = 'Ordered icates',
         y = 'icate') +
    theme(legend.position = 'top')

  # add color
  if(!is.null(.color)){
    p <- p +
      aes(color = as.character(.color)) +
      labs(title = NULL,
           x = 'Ordered icates',
           y = 'icate')
  }

  # apply custom order
  if(!is.null(.order)){
    .order <- adjust_for_estimand_(.model, .order)
    p <- p +
      aes(x = .order, y = icate.m) +
      labs(title = NULL,
           x = 'ordered icates',
           y = 'icate')
  }

  return(p)
}


`%notin%` <- Negate(`%in%`)

# coerce_to_logical_(c(1, 0, 'T', FALSE, '0', '1'))
coerce_to_logical_ <- function(x){
  x[x %in% c(1, '1')] <- TRUE
  x[x %in% c(0, '0')] <- FALSE
  x <- as.logical(x)
  if (!is.logical(x) | sum(is.na(x)) > 0) stop("treatment_col must be logical with no NAs")
  return(x)
}

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

is_numeric_vector_ <- function(x){
  if (!inherits(x, 'numeric')) stop('moderator must be numeric vector')
}

is_discrete_ <- function(x){
  # must be more than one level and all levels can't be unique
  is_discrete <- length(unique(x)) > 1 && length(unique(x)) < length(x)
  if (!isTRUE(is_discrete)) stop('moderator must be discrete')
}

# adjust [moderator] to match estimand
adjust_for_estimand_ <- function(.model, x){
  validate_model_(.model)

  out <- switch(
    .model$estimand,
    ate = x,
    att = x[.model$trt == 1],
    atc = x[.model$trt != 1]
  )

  return(out)
}

# used within plot_moderator_c_pd()
fit_pd_ <- function(x, z1, z0, index, .model){
  z1[, index] <- x
  z0[, index] <- x
  preds.1 <- predict(.model, newdata = z1)
  preds.0 <- predict(.model, newdata = z0)
  preds <- preds.1 - preds.0

  cate <- apply(preds, 1, mean)
  return(cate)
}


#
# rpart_ggplot_overlap <- function(.model){
#
#   # remove depth information from model so resulting plot is easy to read
#   .model$frame$dev <- 1
#
#   # extract data to construct dendrogram
#   fitr <- ggdendro::dendro_data(.model)
#   n_leaf <- .model$frame$n[.model$frame$var == '<leaf>']
#   n_split <- .model$frame$n[.model$frame$var != '<leaf>']
#   pred_split <- round(.model$frame$yval[.model$frame$var != '<leaf>'], 1)
#   terminal_leaf_y <- 0.1
#   leaf_labels <- tibble(
#     x = fitr$leaf_labels$x,
#     y = terminal_leaf_y,
#     label = paste0(
#       'y = ', fitr$leaf_labels$label,
#       '\nn = ', n_leaf)
#   )
#   yes_no_offset <- c(0.7, 1.3)
#   yes_no <- tibble(
#     x = c(fitr$labels$x[[1]] * yes_no_offset[1],
#           fitr$labels$x[[1]] * yes_no_offset[2]),
#     y = rep(fitr$labels$y[[1]], 2),
#     label = c("yes", "no")
#   )
#   split_labels <- tibble(
#     x = fitr$labels$x,
#     y = fitr$labels$y + 0.085,
#     label = paste0(
#       'y = ', pred_split,
#       '\nn = ', n_split
#     )
#   )
#
#   # set terminal segments to y = terminal_leaf_y
#   initial_node_y <- fitr$labels$y[[1]]
#   fitr$segments <- fitr$segments %>%
#     mutate(y_new = ifelse(y > yend, y, yend),
#            yend_new = ifelse(yend < y, yend, y)) %>%
#     select(n, x, y = y_new, xend, yend = yend_new) %>%
#     mutate(y = ifelse(y > initial_node_y, terminal_leaf_y, y),
#            yend = ifelse(x == xend & x == round(x) & y > yend, terminal_leaf_y, yend))
#
#   # set plot constants
#   label_text_size <- 3
#   x_limits <- c(0.5, nrow(fitr$leaf_labels) + 0.5)
#   y_limits <- c(min(fitr$segments$y) - 0.05,
#                 max(fitr$segments$y) + 0.15)
#
#   # plot it
#   p <- ggplot() +
#     geom_segment(data = fitr$segments,
#                  aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_label(data = yes_no,
#                aes(x = x, y = y, label = label),
#                size = label_text_size) +
#     geom_label(data = leaf_labels,
#                aes(x = x, y = y, label = label),
#                size = label_text_size) +
#     geom_label(data = split_labels,
#                aes(x = x, y = y, label = label),
#                size = label_text_size) +
#     geom_label(data = fitr$labels,
#                aes(x = x, y = y, label = label),
#                label.size = NA, fontface = 'bold') +
#     expand_limits(x = x_limits,
#                   y = y_limits) +
#     scale_x_continuous(labels = NULL, breaks = NULL) +
#     scale_y_continuous(labels = NULL, breaks = NULL) +
#     labs(title = 'Exploratory non-overlap covariates',
#          x = NULL,
#          y = NULL) +
#     theme(panel.background = element_blank())
#
#   return(p)
# }


pclamp_ <- function(x, x_min, x_max) pmin(x_max, pmax(x, x_min))

# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'x',
      'xend',
      'y',
      'yend',
      'y_new',
      'yend_new',
      'name',
      'value',
      'support_rule',
      'index',
      'threshold',
      'point',
      '.min',
      '.max',
      'label',
      'Z',
      'Z_treat',
      '..count..',
      '..density..',
      'iteration',
      'Chain',
      'icate.o',
      'ci_2.5',
      'ci_97.5',
      'ci_10',
      'ci_90'
    )
  )
}

