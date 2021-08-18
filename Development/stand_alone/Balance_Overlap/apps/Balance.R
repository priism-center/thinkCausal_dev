library(ggpubr)
library(tidyverse)
library(shiny)
library(bartCause)
library(ggforce)
library(gridExtra)
library(cowplot)
library(ggthemes) 
library(latex2exp)
library(htmlwidgets)

summary_regression_digits <-function(model, type = 'lm', digits = digits){
  if(type == 'lm'){
    tmp <- summary(model)
    tmp$residuals <- round(tmp$residuals, digits)
    tmp$coefficients <- round(tmp$coefficients, digits)
    tmp$adj.r.squared <- round(tmp$adj.r.squared, digits)
    tmp$r.squared <- round(tmp$r.squared, digits)
    tmp$sigma <- round(tmp$sigma, digits)
  }else if(type == 'bart'){
    tmp <- summary(model)
    tmp$estimates <- round(tmp$estimates, digits)
  }
  
  return(tmp)
}

plot_scatterplot <- function(df){
  df$Z <- as.factor(df$Z)
  colors <- c('0' = 'blue', '1' = 'red')
  linetypes <- c('Mean Difference' = 'solid', 'True SATE' = 'dashed')
  ggplot(df) + 
    geom_point( aes(x = X, y = Y, color = Z ))+ 
    xlab("X") + ylab("Observed Outcome") + 
    labs(title = "Distribution of Observed Outcomes") + 
    geom_hline(aes(yintercept = mean(Y[Z == 1]), color = '1', linetype = 'Mean Difference')) +
    geom_hline(aes(yintercept = mean(Y[Z == 0]), color = '0', linetype = 'Mean Difference')) +
    geom_hline(aes(yintercept = mean(Y1), color = '1', linetype = 'True SATE')) +
    geom_hline(aes(yintercept = mean(Y0), color = '0', linetype = 'True SATE')) +
    scale_color_manual("Treated?", values = colors) + 
    scale_linetype_manual("Linetype", values = linetypes) +
    theme_minimal(base_line_size = 0.4) + 
    theme(plot.title = element_text(size = 8), 
          legend.position="bottom",
          text=element_text(family="serif", size=8)) + 
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))
  
}

plot_marginal_distribution <- function(df) {
  df$Z <- as.factor(df$Z)
  colors <- c('0' = 'blue', '1' = 'red')
  bin_breaks <- seq(range(df$X)[1]-(1e-4), range(df$X)[2]+(1e-4), length.out = 61)
  ggplot(df, aes(x = X, fill = Z)) + 
    theme_minimal(base_line_size = 0.4) +
    geom_histogram(breaks = bin_breaks, alpha = 0.4, position = "identity") +
    geom_vline(data = df[which(df$Z == 0),], aes(xintercept= mean(X), color="0"), size=1) + 
    geom_vline(data = df[which(df$Z == 1),], aes(xintercept= mean(X), color="1"), size=1) +
    scale_color_manual(name = "Treated?", values = colors) + 
    scale_fill_manual(name = "Treated?", values = colors) +
    labs(title = "Distribution of Covariate X", y = "Frequency") +
    theme(legend.position="bottom",
          plot.title = element_text(size = 8), 
          text=element_text(family="serif", size=8)) 
  
}

frame_generation <- function(df_balanced) {
  
  set.seed(1)
  
  df_balanced$id <- seq(1, nrow(df_balanced))
  
  bin_breaks <- seq(range(df_balanced$X)[1]-(1e-4), range(df_balanced$X)[2]+(1e-4), length.out = 61)
  
  # define number of frames
  n_frame <- 16
  
  df_output <- df_balanced
  
  df_output$stage <- 1
  
  hist_0 <- data.frame(lower = bin_breaks[1:(length(bin_breaks) - 1)], upper = bin_breaks[2:length(bin_breaks)], 
                       count = 0, stage = 1, x = (bin_breaks[1:(length(bin_breaks) - 1)] + bin_breaks[2:length(bin_breaks)]) / 2)
  
  hist_1 <- data.frame(lower = bin_breaks[1:(length(bin_breaks) - 1)], upper = bin_breaks[2:length(bin_breaks)], 
                       count = 0, stage = 1, x = (bin_breaks[1:(length(bin_breaks) - 1)] + bin_breaks[2:length(bin_breaks)]) / 2)
  
  n_group <- n_frame / 2 - 2
  
  tmp_df <- df_output
  
  tmp_hist_0 <- hist_0
  
  tmp_hist_1 <- hist_1
  
  # generate frames for Z = 0
  group_ids <- data.frame(id = sample(df_balanced$id[df_balanced$Z == 0], sum(df_balanced$Z == 0)), 
                          group = c(rep(seq(1, n_group - 1), each = round(sum(df_balanced$Z == 0) / n_group)),
                                    rep(n_group, sum(df_balanced$Z == 0) - round(sum(df_balanced$Z == 0) / n_group) * (n_group - 1))))
  
  for (i in 2:(n_frame/2)) {
    
    if (i < (n_frame / 2))
      tmp_df$Y[group_ids$id[group_ids$group == i - 1]] <- tmp_df$Y[group_ids$id[group_ids$group == i - 1]] - 
        unlist(sapply(tmp_df$Y[group_ids$id[group_ids$group == i - 1]], function(x) runif(1, min = 0, max = x)))
    
    if (i == (n_frame / 2))
      tmp_df$Y[group_ids$id[group_ids$group == i - 2]] <- 0
    
    if (i > 2 & i < (n_frame / 2)) 
      tmp_df$Y[group_ids$id[group_ids$group == i - 2]] <- 0
    
    tmp_df$stage <- i 
    
    tmp_hist_0$count <- unlist(sapply(1:nrow(tmp_hist_0), function(x) {
      idx <- which(tmp_df$Z == 0 & tmp_df$Y == 0 & tmp_df$X > tmp_hist_0$lower[x] & tmp_df$X <= tmp_hist_0$upper[x])
      length(idx)
    }))
    
    tmp_hist_0$stage <- i
    
    tmp_hist_1$stage <- i
    
    df_output <- rbind(df_output, tmp_df)
    
    hist_0 <- rbind(hist_0, tmp_hist_0)
    
    hist_1 <- rbind(hist_1, tmp_hist_1)
  }
  
  # generate frames for Z = 1
  group_ids <- data.frame(id = sample(df_balanced$id[df_balanced$Z == 1], sum(df_balanced$Z == 1)), 
                          group = c(rep(seq(1, n_group - 1), each = round(sum(df_balanced$Z ==1) / n_group)),
                                    rep(n_group, sum(df_balanced$Z == 1) - round(sum(df_balanced$Z == 1) / n_group) * (n_group - 1))))
  
  tmp_df$stage <- i + 1
  
  tmp_hist_0$stage <- i + 1
  
  tmp_hist_1$stage <- i + 1
  
  df_output <- rbind(df_output, tmp_df)
  
  hist_0 <- rbind(hist_0, tmp_hist_0)
  
  hist_1 <- rbind(hist_1, tmp_hist_1)
  
  for (i in 2:(n_frame/2)) {
    
    if (i < (n_frame / 2))
      tmp_df$Y[group_ids$id[group_ids$group == i - 1]] <- tmp_df$Y[group_ids$id[group_ids$group == i - 1]] - 
        unlist(sapply(tmp_df$Y[group_ids$id[group_ids$group == i - 1]], function(x) runif(1, min = 0, max = x)))
    
    if (i == (n_frame / 2))
      tmp_df$Y[group_ids$id[group_ids$group == i - 2]] <- 0
    
    if (i > 2 & i < (n_frame / 2)) 
      tmp_df$Y[group_ids$id[group_ids$group == i - 2]] <- 0
    
    tmp_df$stage <- i + (n_frame / 2) 
    
    tmp_hist_1$count <- unlist(sapply(1:nrow(tmp_hist_1), function(x) {
      idx <- which(tmp_df$Z == 1 & tmp_df$Y == 0 & tmp_df$X > tmp_hist_1$lower[x] & tmp_df$X <= tmp_hist_1$upper[x])
      length(idx)
    }))
    
    tmp_hist_0$stage <- i + (n_frame / 2) 
    
    tmp_hist_1$stage <- i + (n_frame / 2) 
    
    df_output <- rbind(df_output, tmp_df)
    
    hist_0 <- rbind(hist_0, tmp_hist_0)
    
    hist_1 <- rbind(hist_1, tmp_hist_1)
  }
  
  return(list(df_output, hist_0, hist_1))
  
}

dgp_radomized_balanced <- function (n, beta_1, seed) {
  set.seed(seed) 
  pre_quiz <- rnorm(n = n, mean = 65, sd = 3)
  beta_0 <- 10
  tau <- 5
  Z <- sample(x = c(0,1), size = n, replace = TRUE)
  Y_0 <- beta_0 + beta_1 * pre_quiz + 0 + rnorm(n) 
  Y_1 <- beta_0 + beta_1 * pre_quiz + tau + rnorm(n) 
  Y <- ifelse(Z == 1, Y_1, Y_0)
  output <- data.frame(Z, pre_quiz, Y_0, Y_1, Y)
  colnames(output) <- c( "Z", "X", "Y0", "Y1", "Y")
  return (output) 
}

dgp_overlap_unbalance <- function(n, seed){
  set.seed(seed)
  x <- runif(n,min = 10, max = 11)
  p_z <- pmin(pmax(x-10, 0.1), 0.8) # z is dependent on x
  z <- rbinom(n = n, size = 1, prob = p_z)
  y0 <- 2 * x
  y1 <- y0 - 0.5
  y <- ifelse(z==0, y0, y1) + 0.3*rnorm(n)
  return(data.frame(X=x,Z=z,Y0 = y0, Y1 = y1, Y=y))
}

dgp_nonoverlap_balance <- function(n, seed){
  set.seed(seed)
  num_1 <- floor(n/2)
  num_0 <- n - num_1
  half <- floor(num_1/2)
  x1 <- c(rnorm(half, mean = -2, sd = 1), rnorm(num_1-half, mean = 2, sd = 1))
  x0 <- rnorm(num_0, mean = 0, sd = 1)
  x <- c(x1, x0)
  # use the boundaries of bars in the histogram to ensure no overlap between the two groups in the histogram
  p_z <- ifelse(x<1.14896350&x>-1.14896350, 1, 0) 
  z <- rbinom(n = n, size = 1, prob = p_z)
  y0 =  ifelse(x<1&x>-1, 30 + x - 0.2*x^2 - 10 + rnorm(n), 30 + x - 0.2*x^2 + rnorm(n))
  y1 =  30 + x + 0.4*x^2 + 5 + rnorm(n)
  y = ifelse(z == 0, y0, y1)
  return(data.frame(X=x,Z=z,Y0 = y0, Y1 = y1, Y=y))
}



ui <- fluidPage(  
  withMathJax(),
  tags$style(
    HTML(
      ".MathJax {
            color: black !important;
          }"
    )),
  fluidRow(
    column(10, offset = 1,
           h1("Balance"),
           p('In a', strong("completely randomized design"), 'the treatment assignment is a random variable that is independent of all variables', tags$var('x'), 'that occur before treatment assignment. 
      Under repeated randomizations, there will be no differences', strong('on average'), 'in the pre-treatment covariates, comparing treatment and control groups. 
      The plot shows balance in the covariate', tags$var('X'), 'between treatment and control groups. 
      The difference in means is an unbiased estimate of the treatment effect under a completely randomized design, 
      and for this particular realization the estimate, 86.59 - 81.53 =  5.05, is close to the true sample average treatment effect 5.00.'),
           tags$div(
             id = 'div_balance', style="display:none;",
             sliderInput(
               inputId = 'slider_balance',
               label = 'slider for balance',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('animation'),
           br(), 
           tags$div( actionButton(inputId = "play_balance",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_balance",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_balance').click(function(){
                             {$('#div_balance .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput('balance'),
           br()
    )
    
  ),
  fluidRow(
    column(10, offset = 1,
           p('In an', strong('observational study'), 'there can be systematic differences between groups of units that receive different treatments with respect to key covariates', tags$var('x'), 'that can affect the outcome', tags$var('y'),'. 
    Such covariates that are associated with the treatment and the potential outcomes are typically called confounders or confounding covariates because if we observe differences in average outcomes across these groups, 
    we can’t separately attribute these differences to the treatment or the confounders—the effect of the treatment is thus “confounded” by these variables.'), 
           p( 'Imbalance with measured confounders occurs when the distributions of confounders differ for the treatment and control groups. 
    This could manifest, for instance, as differences in means or standard deviations of a covariate between treatment and control groups. 
    More generally, any differences in covariate distributions across groups can be referred to as lack of balance across groups. 
    When treatment and control groups suffer from imbalance, the simple comparison of group averages', helpText('$$\\bar{Y}_1− \\bar{Y}_0$$'), 'is not, in general, a good estimate of the average treatment effect. 
    In the example below, the mean difference, 0.14, is far from the underlying and unobservable sample average treatment effect (SATE) of -0.5.'),
           tags$div(
             id = 'div_imbalance', style="display:none;",
             sliderInput(
               inputId = 'slider_imbalance',
               label = 'slider for imbalance',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('imbalance_animation'),
           tags$div( actionButton(inputId = "play_imbalance",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_imbalance",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_imbalance').click(function(){
                             {$('#div_imbalance .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput('imbalance')
    )),
  fluidRow(
    column(10, offset = 1,
           p('As the pre-treatment variable is unbalanced between the two groups, the mean difference is biased. 
           Another approach to estimate the average treatment effect is to fit a linear regression of the outcome',tags$var('Y'), 'on the treatment variable', tags$var('Z'), 'and
           the confounder',tags$var('X'), 'which can help us adjust for systematically unbalanced characteristics across groups.'),
           br()
           
    )),
  fluidRow(
    column(3, offset = 1,
           selectInput("select_lm_result", h4("Select the result from the regression to show: "), 
                       choices = c("Only show the coefficient estimated for the treatment effect" = "coefficient", 
                                   "Show the full regression output" = "full_output"), 
                       selected = "coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.select_lm_result === 'full_output'",
                            verbatimTextOutput('lm_result')),
           conditionalPanel("input.select_lm_result === 'coefficient'",
                            verbatimTextOutput('coef'))
           
    )),
  fluidRow(
    column(10, offset = 1,
           p("In this case linear regression has done well in identifying the correct ATE - which is good, but the data generating process was specifically designed to meet the assumptions. 
             Let's look at a case where it might fail."), br())),
  fluidRow(
    column(10, offset = 1,
           tags$div(
             id = 'div_balance_lm_fail', style="display:none;",
             sliderInput(
               inputId = 'slider_balance_lm_fail',
               label = 'slider for lm fail',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('balance_lm_fail_animation'),
           tags$div( actionButton(inputId = "play_balance_lm_fail",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_balance_lm_fail",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_balance_lm_fail').click(function(){
                             {$('#div_balance_lm_fail .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput('balance_lm_fail'),
           br(),
           p('The pre-treatment variable',tags$var('X'), 'is balanced in terms of group mean between the two groups, but the mean difference and linear regression both fail to identify the correct ATE. 
             This is due to the problem of lack of overlap, which is introduced in the next section.'),
           br()
    )),
  fluidRow(
    column(3, offset = 1,
           selectInput("select_lm_fail_result", h4("Select the result from the regression to show: "), 
                       choices = c("Only show the coefficient estimated for the treatment effect" = "lm_fail_coefficient", 
                                   "Show the full regression output" = "lm_fail_full_output"), 
                       selected = "lm_fail_coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.select_lm_fail_result === 'lm_fail_full_output'",
                            verbatimTextOutput('balance_lm_fail_result')),
           conditionalPanel("input.select_lm_fail_result === 'lm_fail_coefficient'",
                            verbatimTextOutput('balance_lm_fail_coef'))
           
    ))
)    

server <- function(input, output) {
  
  # balance
  df_balanced <- dgp_radomized_balanced(n = 1000, beta_1 = 1.1, seed = 123)
  SATE_meandiff <- paste('Mean Difference:', round(mean(df_balanced[df_balanced$Z == 1, ]$Y) - mean(df_balanced[df_balanced$Z == 0, ]$Y), 2))
  SATE <- paste('True SATE:', round(mean(df_balanced$Y1 - df_balanced$Y0), 2))
  
  dat_all <- frame_generation(df_balanced)
  
  output$animation <- renderPlot({
    
    tmp <- dat_all[[1]] %>% filter(stage == input$slider_balance)
    hist0 <- dat_all[[2]] %>% filter(stage == input$slider_balance)
    hist1 <- dat_all[[3]] %>% filter(stage == input$slider_balance)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all[[1]]$X)-0.5, max(dat_all[[1]]$X))) + ylim(c(0,max(c(dat_all[[1]]$Y0, dat_all[[1]]$Y1, dat_all[[1]]$count, dat_all[[2]]$count)) + 1)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all[[1]]$X)-0.5, linetype=1, color = "black") 
    
    return(p)
  })
  
  output$balance <- renderPlot({
    p1 <- plot_scatterplot(df_balanced) +
      annotate(
        "text", label = SATE_meandiff,
        x = 60, y = 95, size = 5, colour = "red"
      ) + 
      annotate(
        "text", label = SATE,
        x = 60, y = 93, size = 5, colour = "red"
      )
    p1_x_hist <- plot_marginal_distribution(df_balanced)
    plot_grid(p1,  p1_x_hist, ncol = 2, nrow = 1)
  })
  
  # imbalance
  df_overlap_unbalance <- dgp_overlap_unbalance(1000, seed = 1234)
  df_overlap_unbalance$Z <- as.factor(df_overlap_unbalance$Z)
  SATE_meandiff_imbalance <- paste('Mean Difference:', round(mean(df_overlap_unbalance$Y[df_overlap_unbalance$Z == 1]) -
                                                               mean(df_overlap_unbalance$Y[df_overlap_unbalance$Z == 0]), 2))
  SATE_imbalance <- paste('True SATE:', round(mean(df_overlap_unbalance$Y1 - df_overlap_unbalance$Y0), 2))
  
  dat_all_imbalance <- frame_generation(df_overlap_unbalance)
  
  output$imbalance_animation <- renderPlot({
    
    tmp <- dat_all_imbalance[[1]] %>% filter(stage == input$slider_imbalance)
    hist0 <- dat_all_imbalance[[2]] %>% filter(stage == input$slider_imbalance)
    hist1 <- dat_all_imbalance[[3]] %>% filter(stage == input$slider_imbalance)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all_imbalance[[1]]$X)-0.1, max(dat_all_imbalance[[1]]$X))) + 
      ylim(c(0,max(c(dat_all_imbalance[[1]]$Y0, dat_all_imbalance[[1]]$Y1, dat_all_imbalance[[1]]$count, dat_all_imbalance[[2]]$count)) + 2)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all_imbalance[[1]]$X)-0.1, linetype=1, color = "black") 
    
    return(p)
  })
  
  output$imbalance <- renderPlot({
    p2 <- plot_scatterplot(df_overlap_unbalance) +
      annotate(
        "text", label = SATE_meandiff_imbalance,
        x = 10.2, y = 22, size = 5, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_imbalance,
        x = 10.2, y = 21.7, size = 5, colour = "red"
      )
    p2_x_hist <- plot_marginal_distribution(df_overlap_unbalance)
    plot_grid(p2,  p2_x_hist, ncol = 2, nrow = 1)
  })
  
  model <- lm(Y~Z+X, data = df_overlap_unbalance)
  
  output$lm_result <- renderPrint({
    print(summary_regression_digits(model, type = 'lm', digits = 2), digits = 2)
  })
  output$coef <- renderPrint({summary(model)$coefficients[,1]["Z1"]})
  
  # balance but linear regression fail
  df_nonoverlap_balance <- dgp_nonoverlap_balance(1000, 1234)
  df_nonoverlap_balance$Z <- as.factor(df_nonoverlap_balance$Z)
  SATE_meandifff_nonoverlap_balance <- paste('Mean Difference:', round(mean(df_nonoverlap_balance$Y[df_nonoverlap_balance$Z == 1]) -
                                                                         mean(df_nonoverlap_balance$Y[df_nonoverlap_balance$Z == 0]), 2))
  SATE_nonoverlap_balance <- paste('True SATE:', round(mean(df_nonoverlap_balance$Y1 - df_nonoverlap_balance$Y0), 2))
  
  model_fail <- lm(Y~X+Z, data = df_nonoverlap_balance)
  
  dat_all_lm_fail <- frame_generation(df_nonoverlap_balance)
  
  output$balance_lm_fail_animation <- renderPlot({

      tmp <- dat_all_lm_fail[[1]] %>% filter(stage == input$slider_balance_lm_fail)
      hist0 <- dat_all_lm_fail[[2]] %>% filter(stage == input$slider_balance_lm_fail)
      hist1 <- dat_all_lm_fail[[3]] %>% filter(stage == input$slider_balance_lm_fail)
      
      p <- ggplot() + theme_minimal() + 
        geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                   shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
        geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                   shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
        geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
        geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
        xlim(c(min(dat_all_lm_fail[[1]]$X)-0.76, max(dat_all_lm_fail[[1]]$X))) + 
        ylim(c(0,max(c(dat_all_lm_fail[[1]]$Y0, dat_all_lm_fail[[1]]$Y1, dat_all_lm_fail[[1]]$count, dat_all_lm_fail[[2]]$count)) + 1)) +
        geom_hline(yintercept=0, linetype=1, color = "black") +
        geom_vline(xintercept = min(dat_all_lm_fail[[1]]$X)-0.76, linetype=1, color = "black") 
      
      return(p)
    })
    
    
  output$balance_lm_fail <- renderPlot({
    p3 <- plot_scatterplot(df_nonoverlap_balance) +
      annotate(
        "text", label = SATE_meandifff_nonoverlap_balance,
        x = -3, y = 40, size = 5, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_nonoverlap_balance,
        x = -3, y = 38.5, size = 5, colour = "red"
      )
    p3_x_hist <- plot_marginal_distribution(df_nonoverlap_balance)
    plot_grid(p3,  p3_x_hist, ncol = 2, nrow = 1)
  })
  
  output$balance_lm_fail_result <- renderPrint({
    print(summary_regression_digits(model_fail, type = 'lm', digits = 2), digits = 2)
  })
  output$balance_lm_fail_coef <- renderPrint({round(summary(model_fail)$coefficients[,1]["Z1"],2)})
  output$balance_lm_fail_SATE <- renderText({paste('True SATE:', round(mean(df_nonoverlap_balance$Y1 - df_nonoverlap_balance$Y0),2))})
  
  observeEvent(input$reset_balance, {
    updateSliderInput(
      # session = session,
      inputId = 'slider_balance',
      value = 1)
  })
  
  observeEvent(input$reset_imbalance, {
    updateSliderInput(
      # session = session,
      inputId = 'slider_imbalance',
      value = 1)
  })
  
  observeEvent(input$reset_balance_lm_fail, {
    updateSliderInput(
      # session = session,
      inputId = 'slider_balance_lm_fail',
      value = 1)
  })
  
}

shinyApp(ui = ui, server = server)    

