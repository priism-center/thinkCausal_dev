library(ggpubr)
library(tidyverse)
library(shiny)
library(bartCause)
library(ggforce)
library(gridExtra)
library(cowplot)
# library(ggthemes) 
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

dgp_overlap <- function(n, delta_c = 5, a1 = 0.5, b1 = 5, a0 = -0.5, b0 = 5, 
                        lwr, upp, seed = 100, nooverlap = FALSE, subset_flag = FALSE){
  set.seed(seed)
  X <- rnorm(n = n, mean = 0, sd = 3)
  # Compare the log-odds scale of lower and upper bounds and return the lowest
  lwr_pre <- log(lwr / (1 - lwr))
  upp_pre <- log(upp / (1 - upp))
  lwr_alpha <- lwr_pre / min(X)
  upp_alpha <- upp_pre / max(X)
  alpha <- min(lwr_alpha, upp_alpha)
  p <- exp(alpha * X) / (1 + exp(alpha * X))
  if(nooverlap){
    Z <- ifelse(p>=0.5, 1, 0)
  }else if (subset_flag){
    Z <- c(rbinom(n = floor(n * 0.6), size = 1, prob = p[1:floor(n * 0.6)]), rep(0, n - floor(n * 0.6)))
  }else{
    Z <- rbinom(n = n, size = 1, prob = p)
  }
  Y0 <- 60 + a0 * X^2 + b0 * X + rnorm(n = n, mean = 0, sd = 2)
  Y1 <- 60 + a1 * X^2 + b1 * X + delta_c + rnorm(n = n, mean = 0, sd = 2)
  Y <- ifelse( Z == 1, Y1, Y0)
  return(data.frame( X, Y, Y0, Y1, Z))
}


ui <- fluidPage(  
  
  fluidRow(
    column(10, offset = 1,
           h1("Overlap"),
           p('Overlap or common support describes the extent to which the support of the covariate data is the same between the treatment and control groups. 
             There is complete overlap when there exist both treatment and control units in all neighborhoods of the covariate space.'),
           tags$div(
             id = 'div_overlap', style="display:none;",
             sliderInput(
               inputId = 'slider_overlap',
               label = 'slider for overlap',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('overlap'), 
           tags$div( actionButton(inputId = "play_overlap",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_overlap",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_overlap').click(function(){
                             {$('#div_overlap .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput('overlap_check'),
           br()
    )
    
  ),
  fluidRow(
    column(10, offset = 1,
           p('Lack of complete overlap in confounders creates problems, because in that setting there are treatment observations for which we have no empirical counterfactuals (that is, control observations with the same covariate distribution) or vice versa.'),
           p('(a) Two distributions with no overlap; (b) two distributions with partial overlap; (c) a scenario in which the range of one distribution is a subset of the range of the other')
           
    )),
  fluidRow(
    column(4, 
           tags$div(
             id = 'div_lackoverlap_left', style="display:none;",
             sliderInput(
               inputId = 'slider_lackoverlap_left',
               label = 'slider for lackoverlap_left',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('lackoverlap_left'), 
           tags$div( actionButton(inputId = "play_lackoverlap_left",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_lackoverlap_left",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_lackoverlap_left').click(function(){
                             {$('#div_lackoverlap_left .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput("lackoverlap_check1_left"), plotOutput("lackoverlap_check2_left"), br()),
    column(4, 
           tags$div(
             id = 'div_lackoverlap_middle', style="display:none;",
             sliderInput(
               inputId = 'slider_lackoverlap_middle',
               label = 'slider for lackoverlap_middle',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('lackoverlap_middle'), 
           tags$div( actionButton(inputId = "play_lackoverlap_middle",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_lackoverlap_middle",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_lackoverlap_middle').click(function(){
                             {$('#div_lackoverlap_middle .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput("lackoverlap_check1_middle"), plotOutput("lackoverlap_check2_middle"), br()),
    column(4, 
           tags$div(
             id = 'div_lackoverlap_right', style="display:none;",
             sliderInput(
               inputId = 'slider_lackoverlap_right',
               label = 'slider for lackoverlap_right',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('lackoverlap_right'), 
           tags$div( actionButton(inputId = "play_lackoverlap_right",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_lackoverlap_right",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_lackoverlap_right').click(function(){
                             {$('#div_lackoverlap_right .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput("lackoverlap_check1_right"), plotOutput("lackoverlap_check2_right"), br())
  ),
  fluidRow(
    column(10, offset = 1,
           br(),
           p('Suppose we are interested in estimating the treatment effect in the middle example above, and in the example',tags$var('X'), 'is only one confounding covariate — that is, only one predictor', tags$var('X'), 'is necessary to satisfy ignorability. The red dots correspond to the units who received the treatment;
             the blue dots correspond to the units who did not receive the treatment. In the interactive figure below, the dashed and dotted lines are regression lines fit to the observed data.'),
           p('Because there are no control units with high',tags$var('X'), 'and no treatment units with low',tags$var('X'), ', the linear model, to create counterfactual predictions,
             is forced to extrapolate over portions of the space where there are no data to support them. This causes an underestimate of the true average treatment effect. Allowing for an interaction does not solve the problem.
             You may see in the all-seeing plot where the red solid line represents the true relationship between the potential outcome for treatment receipt and the', tags$var('X'), ', ', tags$var('E(Y1|X)'), 
             ', and the blue solid line represents the true relationship between the potential outcome for the control condition and the ', tags$var('X'), ', ', tags$var('E(Y1|X)'), 
             '; the true causal effect at any level of the', tags$var('X'), 'is the vertical distance between the two solid lines. Each average causal effect is an average across the relevant subset of these individual-level causal effects.'),
           inputPanel(selectInput("select_all_seeing", label = "Choose to see researchers' or all-seeing perspectives:",
                                  choices = c("Researchers' view - fitting a linear regression" = 'nonoverlap_lm',
                                              "Researchers' view - fitting a linear regression with an interaction" = 'nonoverlap_lm_interaction',
                                              "All-seeing view" = 'nonoverlap_all_seeing'),
                                  selected = "Researchers' view - fitting a linear regression")),
           plotOutput("all_seeing")

    )),
  fluidRow(
    column(3, offset = 1,
           selectInput("nonoverlap_select_lm_result", h4("Select the result from the linear regression to show: "),
                       choices = c("Only show the coefficient estimated for the treatment effect" = "nonoverlap_lm_coefficient",
                                   "Show the full regression output" = "nonoverlap_lm_full_output"),
                       selected = "nonoverlap_lm_coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.nonoverlap_select_lm_result === 'nonoverlap_lm_full_output'",
                            verbatimTextOutput('nonoverlap_lm_result')),
           conditionalPanel("input.nonoverlap_select_lm_result === 'nonoverlap_lm_coefficient'",
                            verbatimTextOutput('nonoverlap_lm_coef'))

    )),
  fluidRow(
    column(3, offset = 1,
           selectInput("nonoverlap_select_lm_interaction_result", h4("Select the result from the linear regression with interaction to show: "),
                       choices = c("Only show the coefficient estimated for the treatment effect" = "nonoverlap_lm_interaction_coefficient",
                                   "Show the full regression output" = "nonoverlap_lm_interaction_full_output"),
                       selected = "nonoverlap_lm_interaction_coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.nonoverlap_select_lm_interaction_result === 'nonoverlap_lm_interaction_full_output'",
                            verbatimTextOutput('nonoverlap_lm_interaction_result')),
           conditionalPanel("input.nonoverlap_select_lm_interaction_result === 'nonoverlap_lm_interaction_coefficient'",
                            verbatimTextOutput('nonoverlap_lm_interaction_coef'))

    )),
  fluidRow(
    column(10, offset = 1,
           textOutput('nonoverlap_SATE'),
           br(), 
           p('Since we rely on empirical counterfactual units to inform counterfactual outcomes, when treatment and control groups do not completely overlap, the data are inherently limited in what they can tell us about treatment effects in the regions of nonoverlap.
             In regions where the groups do not have covariate overlap, causal estimation is purely based on extrapolation. Hence, any inferences in the areas with no overlap would be sensitive to model-specification.
             Failure to detect areas that lack common support can lead to biased inference due to imbalance or inappropriate model extrapolation.
             Bayesian Additive Regression Tree (BART) model with flexible functional form has many advantages over other causal inference strategies. Next section, you will learn how BART method deal with the problem of lack of overlap.'),
           br())
  )
)    

server <- function(input, output) {
  
  # overlap
  df_balanced <- dgp_radomized_balanced(n = 1000, beta_1 = 1.1, seed = 14)
  SATE_meandiff_balance <- paste('Mean Difference:', round(mean(df_balanced[df_balanced$Z == 1, ]$Y) - mean(df_balanced[df_balanced$Z == 0, ]$Y), 2))
  SATE_balance <- paste('True SATE:', round(mean(df_balanced$Y1 - df_balanced$Y0), 2))
  
  dat_all_balance <- frame_generation(df_balanced)
  
  output$overlap <- renderPlot({
    
    tmp <- dat_all_balance[[1]] %>% filter(stage == input$slider_overlap)
    hist0 <- dat_all_balance[[2]] %>% filter(stage == input$slider_overlap)
    hist1 <- dat_all_balance[[3]] %>% filter(stage == input$slider_overlap)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all_balance[[1]]$X)-0.5, max(dat_all_balance[[1]]$X))) + 
      ylim(c(0,max(c(dat_all_balance[[1]]$Y0, dat_all_balance[[1]]$Y1, dat_all_balance[[1]]$count, dat_all_balance[[2]]$count)) + 1)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all_balance[[1]]$X)-0.5, linetype=1, color = "black") 
    
    return(p)
  })
  
  output$overlap_check <- renderPlot({
    p1 <- plot_scatterplot(df_balanced) +
      annotate(
        "text", label = SATE_meandiff_balance,
        x = 60, y = 95, size = 5, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_balance,
        x = 60, y = 93, size = 5, colour = "red"
      )
    p1_overlap <- plot_marginal_distribution(df_balanced)
    plot_grid(p1,  p1_overlap, ncol = 2, nrow = 1)
  })
  
  observeEvent(input$reset_overlap,{
    updateSliderInput(
      # session = session,
      inputId = 'slider_overlap',
      value = 1)
  })
  
  # lack of overlap
  delta_c = 5
  a1 = 0.5
  b1 = 5
  a0 = -0.5
  b0 = 5
  
  df_overlap3 <- dgp_overlap(n = 100, delta_c = delta_c, a1 = a1, b1 = b1, a0 = a0, b0 = b0, 
                             lwr = 0.05, upp = 0.95, nooverlap = TRUE)
  df_overlap2 <- dgp_overlap(n = 100, delta_c = delta_c, a1 = a1, b1 = b1, a0 = a0, b0 = b0, 
                             lwr = 0.05, upp = 0.95)
  df_overlap1 <- dgp_overlap(n = 100, delta_c = delta_c, a1 = a1, b1 = b1, a0 = a0, b0 = b0, 
                             lwr = 0.05, upp = 0.95, subset_flag = TRUE)
  
  SATE_meandiff_left <- paste('Mean Difference:', round(mean(df_overlap3$Y[df_overlap3$Z == 1]) - mean(df_overlap3$Y[df_overlap3$Z == 0]), 2))
  SATE_left <- paste('True SATE:', round(mean(df_overlap3$Y1 - df_overlap3$Y0), 2))
  
  SATE_meandiff_middle <- paste('Mean Difference:', round(mean(df_overlap2$Y[df_overlap2$Z == 1]) - mean(df_overlap2$Y[df_overlap2$Z == 0]), 2))
  SATE_middle <- paste('True SATE:', round(mean(df_overlap2$Y1 - df_overlap2$Y0), 2))
  
  SATE_meandiff_right <- paste('Mean Difference:', round(mean(df_overlap1$Y[df_overlap1$Z == 1]) - mean(df_overlap1$Y[df_overlap1$Z == 0]), 2))
  SATE_right <- paste('True SATE:', round(mean(df_overlap1$Y1 - df_overlap1$Y0), 2))
  
  dat_all_left <- frame_generation(df_overlap3)
  dat_all_middle <- frame_generation(df_overlap2)
  dat_all_right <- frame_generation(df_overlap1)
  
  output$lackoverlap_left <- renderPlot({
    
    tmp <- dat_all_left[[1]] %>% filter(stage == input$slider_lackoverlap_left)
    hist0 <- dat_all_left[[2]] %>% filter(stage == input$slider_lackoverlap_left)
    hist1 <- dat_all_left[[3]] %>% filter(stage == input$slider_lackoverlap_left)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all_left[[1]]$X)-0.5, max(dat_all_left[[1]]$X))) + 
      ylim(c(0,max(c(dat_all_left[[1]]$Y0, dat_all_left[[1]]$Y1, dat_all_left[[1]]$count, dat_all_left[[2]]$count)) + 1)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all_left[[1]]$X)-0.5, linetype=1, color = "black") 
    
    return(p)
  })
  output$lackoverlap_middle <- renderPlot({
    
    tmp <- dat_all_middle[[1]] %>% filter(stage == input$slider_lackoverlap_middle)
    hist0 <- dat_all_middle[[2]] %>% filter(stage == input$slider_lackoverlap_middle)
    hist1 <- dat_all_middle[[3]] %>% filter(stage == input$slider_lackoverlap_middle)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all_middle[[1]]$X)-0.5, max(dat_all_middle[[1]]$X))) + 
      ylim(c(0,max(c(dat_all_middle[[1]]$Y0, dat_all_middle[[1]]$Y1, dat_all_middle[[1]]$count, dat_all_middle[[2]]$count)) + 1)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all_middle[[1]]$X)-0.5, linetype=1, color = "black") 
    
    return(p)
  })
  output$lackoverlap_right <- renderPlot({
    
    tmp <- dat_all_right[[1]] %>% filter(stage == input$slider_lackoverlap_right)
    hist0 <- dat_all_right[[2]] %>% filter(stage == input$slider_lackoverlap_right)
    hist1 <- dat_all_right[[3]] %>% filter(stage == input$slider_lackoverlap_right)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all_right[[1]]$X)-0.5, max(dat_all_right[[1]]$X))) + 
      ylim(c(0,max(c(dat_all_right[[1]]$Y0, dat_all_right[[1]]$Y1, dat_all_right[[1]]$count, dat_all_right[[2]]$count)) + 1)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all_right[[1]]$X)-0.5, linetype=1, color = "black") 
    
    return(p)
  })
  
  output$lackoverlap_check1_right <- renderPlot({
    plot_scatterplot(df_overlap1) + theme(plot.title = element_text(size=8)) + 
      annotate(
        "text", label = SATE_meandiff_right,
        x = -3, y = 120, size = 4, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_right,
        x = -3, y = 112, size = 4, colour = "red"
      )
  })
  
  output$lackoverlap_check1_middle <- renderPlot({
    plot_scatterplot(df_overlap2) + theme(plot.title = element_text(size=8)) + 
      annotate(
        "text", label = SATE_meandiff_middle,
        x = -3, y = 125, size = 4, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_middle,
        x = -3, y = 117, size = 4, colour = "red"
      )
  })
  
  output$lackoverlap_check1_left <- renderPlot({
    plot_scatterplot(df_overlap3) + theme(plot.title = element_text(size=8)) + 
      annotate(
        "text", label = SATE_meandiff_left,
        x = -3, y = 125, size = 4, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_left,
        x = -3, y = 117, size = 4, colour = "red"
      )
  })
 

  
  output$lackoverlap_check2_right <- renderPlot({
    plot_marginal_distribution(df_overlap1) + theme(plot.title = element_text(size=8))
  })
  output$lackoverlap_check2_middle <- renderPlot({
    plot_marginal_distribution(df_overlap2) + theme(plot.title = element_text(size=8))
  })
  output$lackoverlap_check2_left <- renderPlot({
    plot_marginal_distribution(df_overlap3) + theme(plot.title = element_text(size=8))
  })
  
  observeEvent(input$reset_lackoverlap_left,{
    updateSliderInput(
      # session = session,
      inputId = 'slider_lackoverlap_left',
      value = 1)
  })
  
  observeEvent(input$reset_lackoverlap_middle,{
    updateSliderInput(
      # session = session,
      inputId = 'slider_lackoverlap_middle',
      value = 1)
  })
  
  observeEvent(input$reset_lackoverlap_right,{
    updateSliderInput(
      # session = session,
      inputId = 'slider_lackoverlap_right',
      value = 1)
  })
  
  df_overlap2$Z <- as.factor(df_overlap2$Z)
  model <- lm(Y ~ X + Z, data = df_overlap2)
  model_interaction <- lm(Y ~ X*Z, data = df_overlap2)
  coef_model <- summary(model)$coefficients
  coef_model_interaction <- summary(model_interaction)$coefficients

  output$all_seeing <- renderPlot({

    colors <- c("0" = "blue", "1" = "red")
    linetype <- c("linear model without interaction" = "dashed", "linear model with an interaction" = "dotted")

    if(input$select_all_seeing == 'nonoverlap_lm'){ 
      ggplot() + geom_point(data = df_overlap2[df_overlap2$Z == 1, ], aes(x = X, y = Y, color = "1")) + 
        geom_point(data = df_overlap2[df_overlap2$Z == 0, ], aes(x = X, y = Y, color = "0")) + 
        scale_color_manual("Treated?", values=colors) + 
        geom_line(data = df_overlap2, aes(x = X, y = coef_model[1] + coef_model[2] * X), linetype = "dashed") + 
        geom_line(data = df_overlap2, aes(x = X, y = coef_model[1] + coef_model[2] * X + coef_model[3]), linetype = "dashed") +
        theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom") 
    }else if(input$select_all_seeing == 'nonoverlap_lm_interaction'){
      ggplot() + geom_point(data = df_overlap2[df_overlap2$Z == 1, ], aes(x = X, y = Y, color = "1")) + 
        geom_point(data = df_overlap2[df_overlap2$Z == 0, ], aes(x = X, y = Y, color = "0")) + 
        scale_color_manual("Treated?", values=colors) + 
        geom_line(data = df_overlap2, aes(x = X, y = coef_model_interaction[1] + coef_model_interaction[2] * X), 
                  linetype = "dotted") + 
        geom_line(data = df_overlap2, aes(x = X, y = coef_model_interaction[1] + 
                                            (coef_model_interaction[2] + coef_model_interaction[4]) * X + coef_model_interaction[3]), 
                  linetype = "dotted") + theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom")
    }else{ # all-seeing view
      shapes <- c('Observed' = 16, 'Counterfactual' = 1)
      ggplot() + geom_point(data = df_overlap2[df_overlap2$Z == 1, ], aes(x = X, y = Y, color = "1", shape = 'Observed')) + 
        geom_point(data = df_overlap2[df_overlap2$Z == 0, ], aes(x = X, y = Y, color = "0", shape = 'Observed')) + 
        geom_point(data = df_overlap2[df_overlap2$Z == 1, ], aes(x = X, y = Y0, color = "1", shape = 'Counterfactual')) +
        geom_point(data = df_overlap2[df_overlap2$Z == 0, ], aes(x = X, y = Y1, color = "0", shape = 'Counterfactual')) +
        geom_line(data = df_overlap2, aes(x = X, y = 60 + b1*X + a1*X^2 + delta_c), color = "red") +
        geom_line(data = df_overlap2, aes(x = X, y = 60 + b0*X + a0*X^2), color = "blue") +
        scale_color_manual("Treated?", values=colors) + 
        scale_shape_manual('Shape', values = shapes) +
        geom_line(data = df_overlap2, aes(x = X, y = coef_model[1] + coef_model[2] * X, linetype = "linear model without interaction")) + 
        geom_line(data = df_overlap2, aes(x = X, y = coef_model[1] + coef_model[2] * X + coef_model[3], 
                                          linetype = "linear model without interaction")) +
        geom_line(data = df_overlap2, aes(x = X, y = coef_model_interaction[1] + coef_model_interaction[2] * X, 
                                          linetype = "linear model with an interaction")) + 
        geom_line(data = df_overlap2, aes(x = X, y = coef_model_interaction[1] + (coef_model_interaction[2] + coef_model_interaction[4]) * X +
                                            coef_model_interaction[3], linetype = "linear model with an interaction")) +
        theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom") + scale_linetype_manual( "Linetype", values=linetype)
    }
  })

  output$nonoverlap_lm_result <- renderPrint({
    print(summary_regression_digits(model, type = 'lm', digits = 2), digits = 2)
  })
  output$nonoverlap_lm_coef <- renderPrint({
    round(coef_model[,1]["Z1"],2)
  })

  output$nonoverlap_lm_interaction_result <- renderPrint({
    print(summary_regression_digits(model_interaction, type = 'lm', digits = 2), digits = 2)
  })
  output$nonoverlap_lm_interaction_coef <- renderPrint({
    round(coef_model_interaction[,1]["Z1"],2)
  })

  output$nonoverlap_SATE <- renderText({
    paste('True SATE:', round(mean(df_overlap2$Y1 - df_overlap2$Y0), 2))
  })
}

shinyApp(ui = ui, server = server)    














