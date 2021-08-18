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

dgp_nonoverlap_unbalance <- function(n, seed){
  set.seed(seed)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  X <- ifelse(Z == 1, rnorm(n = n, mean = 40, sd = 10), rnorm(n = n, mean = 20, sd = 5))
  Y_0 <- 12 + 3*sqrt(X) + rnorm(n = n, 0,1)
  Y_1 <- 30 + exp(0.06*X) + rnorm(n = n, 0,1)
  Y <- ifelse(Z==1, Y_1, Y_0)
  output <- data.frame(Z, X, Y_0, Y_1, Y)
  colnames(output) <- c( "Z", "X", "Y0", "Y1", "Y")
  return(output)
}


ui <- fluidPage(  
  
  fluidRow(
    column(10, offset = 1,
           h1("BART and Overlap"),
           p('As the previous section demonstrated, problems can arise when we try to estimate causal effects for units that lack common support. 
           Extrapolations over areas of the covariate space where common support does not exist can lead to biased inferences because of the lack of information available to identify either ', tags$var('E[Y(0) | X]'), ' or ', tags$var('E[Y(1) | X]'), ' in these regions, 
           which is shared by all causal modeling strategies that do not first discard units in these areas. Simply discarding observations deemed problematic is unlikely to lead to an optimal solution.
           BART method’s flexible functional form and its ability to take advantage of information in the response surface allows it to better target areas of common causal support than traditional methods. 
           Several discarding rules have been developed based on the information that BART provides to determine and profile units lack sufficient counterfactual evidence.'),
           p("Let's first look at an example where the covariate", tags$var('X'), " is imbalanced and does not complete overlap between the treatment and control groups."),
           tags$div(
             id = 'div_imbalance_lackoverlap', style="display:none;",
             sliderInput(
               inputId = 'slider_imbalance_lackoverlap',
               label = 'slider for imbalance_lackoverlap',
               value = 1,
               min = 1,
               max = 16,
               step = 1,
               ticks = F,
               animate = animationOptions(interval = 1000, loop = FALSE)
             )
           ),
           plotOutput('imbalance_lackoverlap'), 
           tags$div( actionButton(inputId = "play_imbalance_lackoverlap",
                                  label = "Animate", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     br(), br(),
                     actionButton(inputId = "reset_imbalance_lackoverlap",
                                  label = "Reset animation", width = '80%', style="color: #fff; background-color: #778899; border-color: #778899"),
                     tags$script(
                       "$('#play_imbalance_lackoverlap').click(function(){
                             {$('#div_imbalance_lackoverlap .slider-animate-button').click()};
                             });"
                     ),
                     align = "center"),br(),br(),
           plotOutput('imbalance_lackoverlap_check'),
           br()
    )),
  fluidRow(
    column(10, offset = 1,
           br(),
           p("The difference in means is a biased estimate of the treatment effect. The OLS estimator also fails to capture the true effect because there just isn't enough information in the data to extrapolate fully into areas where there isn't overlap. 
             Using flexible models such as BART would be the way to go. The BART point estimate (posterior mean) of the average effect of the treatment is 8.07 with 95% posterior interval (6.14, 9.99), much closer to the true SATE, 10.01.")
    )),
  fluidRow(
    column(10, offset = 1,
           inputPanel(selectInput("all_seeing_BART", label = "Choose to see researchers' or all-seeing perspectives:",
                                  choices = c("Researchers' view" = 'BART_lm',
                                              "All-seeing view" = 'BART_all_seeing'),
                                  selected = 'BART_lm')),
           plotOutput("BART_all_seeing"))
  ),
  fluidRow(
    column(3, offset = 1,
           selectInput("bart_select_lm_result", h4("Select the result from the linear regression to show: "),
                       choices = c("Only show the coefficient estimated for the treatment effect" = "bart_lm_coefficient",
                                   "Show the full regression output" = "bart_lm_full_output"),
                       selected = "bart_lm_coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.bart_select_lm_result === 'bart_lm_full_output'",
                            verbatimTextOutput('bart_lm_result')),
           conditionalPanel("input.bart_select_lm_result === 'bart_lm_coefficient'",
                            verbatimTextOutput('bart_lm_coef'))
           
    )),
  fluidRow(
    column(3, offset = 1,
           selectInput("bart_result", h4("Select the result from the BART model to show: "),
                       choices = c("Only show the coefficient estimated for the treatment effect" = "bart_coefficient",
                                   "Show the full regression output" = "bart_full_output"),
                       selected = "bart_coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.bart_result === 'bart_full_output'",
                            verbatimTextOutput('bart_result')),
           conditionalPanel("input.bart_result === 'bart_coefficient'",
                            verbatimTextOutput('bart_coef'))
           
    )),
  fluidRow(
    column(10, offset = 1,
           p('The dashed line in the figure below displays the BART fit to the data which is quite close to the true conditional expectation for most of the support except at values of', tags$var('X'), 'far from the area of strong overlap.'),
           br(),
           plotOutput('bart_line'),
           p("After discarding observations in neighborhoods of the covariate space that lack sufficient common causal support using the BART rule 'sd', the interval cover the true treatment effect for the remaining sample."))
  ),
  fluidRow(
    column(3, offset = 1,
           selectInput("bart_discard_result", h4("Select the result from the BART model to show: "),
                       choices = c("Only show the coefficient estimated for the treatment effect" = "bart_discard_coefficient",
                                   "Show the full regression output" = "bart_discard_full_output"),
                       selected = "bart_discard_coefficient")),
    column(7, offset = 1,
           conditionalPanel("input.bart_discard_result === 'bart_discard_full_output'",
                            verbatimTextOutput('bart_discard_result')),
           conditionalPanel("input.bart_discard_result === 'bart_discard_coefficient'",
                            verbatimTextOutput('bart_discard_coef'))
           
    )),
  fluidRow(
    column(10, offset = 1,
           textOutput('bart_discard_SATT'),
           textOutput('bart_discard_CATT'),
           br(),
           p('SATT and CATT for the remaining units are 7.87 and 7.95, respectively. Our new BART estimate is 7.62 with 95% posterior interval (6.9, 8.3), which covers the SATT and CATT. 
             In the left panel of the figure below, points in circle are the observations discarded based on the BART rule. 
             The gray vertical segments are 95% intervals for the posterior predicted counterfactuals at each', tags$var('X'), 'value from an observation. 
             The right panel displays the BART inference for each treated unit (which can be averaged to estimate the effect of the treatment on the treated for this sample). 
             In the right panel, the true treatment effect as it varies with', tags$var('X'), ', ', tags$var('E[Y(1)−Y(0) | X]'), ', is plotted as the solid curve. 
             The vertical segments are marginal 95% posterior intervals for the treatment effect at each', tags$var('X'), 'value from a treated observation. 
             Notice that the uncertainty bounds grow much wider in the range where there is no overlap across treatment groups.'),
           br(),
           plotOutput('bart_discard_uncertainty'))
  )
)    

server <- function(input, output) {
  
  df_nonoverlap_unbalance <- dgp_nonoverlap_unbalance(200, 123)
  df_nonoverlap_unbalance$Z <- as.factor(df_nonoverlap_unbalance$Z)
  SATE_meandiff_nonoverlap_unbalance <- paste('Mean Difference:', round(mean(df_nonoverlap_unbalance$Y[df_nonoverlap_unbalance$Z == 1]) - mean(df_nonoverlap_unbalance$Y[df_nonoverlap_unbalance$Z == 0]), 2))
  
  SATE_nonoverlap_unbalance <- paste('True SATE:', round(mean(df_nonoverlap_unbalance$Y1 - df_nonoverlap_unbalance$Y0), 2))
  
  # linear regression
  model_nonoverlap_unbalance <- lm(Y ~ X + Z, data = df_nonoverlap_unbalance)
  coef_nonoverlap_unbalance <- summary(model_nonoverlap_unbalance)$coefficients
  
  dat_all_bart <- frame_generation(df_nonoverlap_unbalance)
  
  output$imbalance_lackoverlap <- renderPlot({
    
    
    
    tmp <- dat_all_bart[[1]] %>% filter(stage == input$slider_imbalance_lackoverlap)
    hist0 <- dat_all_bart[[2]] %>% filter(stage == input$slider_imbalance_lackoverlap)
    hist1 <- dat_all_bart[[3]] %>% filter(stage == input$slider_imbalance_lackoverlap)
    
    p <- ggplot() + theme_minimal() + 
      geom_point(data = tmp[which(tmp$Z==0),], aes(x = X, y = Y),
                 shape = 21, color = 'blue', size = 2, stroke = 1, fill = 'blue', alpha = 0.3) + 
      geom_point(data = tmp[which(tmp$Z==1),], aes(x = X, y = Y),
                 shape = 21, color = 'red', size = 2, stroke = 1, fill = 'red', alpha = 0.3) + 
      geom_col(data = hist0, aes(x = x, y = count), color = 'blue', fill = 'blue', alpha = 0.3) +
      geom_col(data = hist1, aes(x = x, y = count), color = 'red', fill = 'red', alpha = 0.3) +
      xlim(c(min(dat_all_bart[[1]]$X)-3, max(dat_all_bart[[1]]$X))) + 
      ylim(c(0,max(c(dat_all_bart[[1]]$Y0, dat_all_bart[[1]]$Y1, dat_all_bart[[1]]$count, dat_all_bart[[2]]$count)) + 2)) +
      geom_hline(yintercept=0, linetype=1, color = "black") +
      geom_vline(xintercept = min(dat_all_bart[[1]]$X)-3, linetype=1, color = "black") 
    
    return(p)
  }) 
  
  output$imbalance_lackoverlap_check <- renderPlot({
    
    p1 <- plot_scatterplot(df_nonoverlap_unbalance) +
      annotate(
        "text", label = SATE_meandiff_nonoverlap_unbalance,
        x = 40, y = 95, size = 5, colour = "red"
      ) + 
      annotate(
        "text", label = SATE_nonoverlap_unbalance,
        x = 40, y = 90, size = 5, colour = "red"
      )
    p1_x_hist <- plot_marginal_distribution(df_nonoverlap_unbalance)
    plot_grid(p1,  p1_x_hist, ncol = 2, nrow = 1)
  })
  
  colors <- c("0" = "blue", "1" = "red")
  output$BART_all_seeing <- renderPlot({
    
    linetypes <- c('Mean Difference' = 'solid', 'True SATE' = 'dashed', 'Linear Regression' = 'dotted')
    if(input$all_seeing_BART == "BART_lm"){ 
      ggplot(data = df_nonoverlap_unbalance) + 
        # geom_line(aes(x = X, y = 30 + exp(0.06*X)), color = "red") +
        # geom_line(aes(x = X, y = 12 + 3*sqrt(X)), color = "blue") +
        # geom_hline(aes(yintercept = mean(Y[Z == 1]), color = '1', linetype = 'Mean Difference')) +
        # geom_hline(aes(yintercept = mean(Y[Z == 0]), color = '0', linetype = 'Mean Difference')) +
        # geom_hline(aes(yintercept = mean(Y1), color = '1', linetype = 'True SATE')) +
        # geom_hline(aes(yintercept = mean(Y0), color = '0', linetype = 'True SATE')) +
        geom_line(aes(x = X, y = coef_nonoverlap_unbalance[1] + coef_nonoverlap_unbalance[2] * X, linetype = 'Linear Regression')) + 
        geom_line(aes(x = X, y = coef_nonoverlap_unbalance[1] + coef_nonoverlap_unbalance[2] * X + coef_nonoverlap_unbalance[3],
                      linetype = 'Linear Regression')) +
        geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 1, ], aes(x = X, y = Y, color = "1")) + 
        geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 0, ], aes(x = X, y = Y, color = "0")) + 
        scale_color_manual("Treated?", values = colors) + 
        scale_linetype_manual("Linetype", values = linetypes) +
        theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom") + ylab("Observed Outcome") +
        guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))
    }else{
      shapes <- c('Observed' = 16, 'Counterfactual' = 1)
      ggplot(data = df_nonoverlap_unbalance) + 
        geom_line(aes(x = X, y = 30 + exp(0.06*X)), color = "red") +
        geom_line(aes(x = X, y = 12 + 3*sqrt(X)), color = "blue") +
        geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 1, ], aes(x = X, y = Y, color = "1", shape = 'Observed')) + 
        geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 0, ], aes(x = X, y = Y, color = "0", shape = 'Observed')) + 
        geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 1, ], aes(x = X, y = Y0, color = "1", shape = 'Counterfactual')) +
        geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 0, ], aes(x = X, y = Y1, color = "0", shape = 'Counterfactual')) +
        scale_color_manual("Treated?", values = colors) + scale_shape_manual('Shape', values = shapes) +
        theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom") + ylab("Observed Outcome") +
        guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))
      
    }
  })
  

  output$bart_lm_result <- renderPrint({
    print(summary_regression_digits(model_nonoverlap_unbalance, type = 'lm', digits = 2), digits = 2)
  })
  output$bart_lm_coef <- renderPrint({
    round(coef_nonoverlap_unbalance[,1]["Z1"],2)
  })
  
  # BART
  df_nonoverlap_unbalance$Z_val <- as.numeric(as.character(df_nonoverlap_unbalance$Z))
  fit <- bartc(response = Y, treatment = Z_val, confounders = X, data = df_nonoverlap_unbalance)
  
  
  output$bart_result <- renderPrint({
    summary_regression_digits(fit, type = 'bart', digits = 2)
  })
  output$bart_coef <- renderPrint({
    round(summary(fit)$estimates,2)
  })
  
  
  output$bart_line <- renderPlot({
    y0_hat <- fitted(object = fit, type = 'y.0')
    y1_hat <- fitted(object = fit, type = 'y.1')
    
    df_BART <- data.frame(x=df_nonoverlap_unbalance$X, y0 = y0_hat, y1 = y1_hat)
    
    ggplot() + geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 1, ], aes(x = X, y = Y, color = "1")) + 
      geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 0, ], aes(x = X, y = Y, color = "0")) + 
      geom_line(data = df_nonoverlap_unbalance, aes(x = X, y = 30 + exp(0.06*X)), color = "red") +
      geom_line(data = df_nonoverlap_unbalance, aes(x = X, y = 12 + 3*sqrt(X)), color = "blue") +
      scale_color_manual("Treated?", values=colors) + 
      theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom") + 
      geom_line(data = df_BART, aes(x = x, y = y0), linetype = 'dashed') +
      geom_line(data = df_BART, aes(x = x, y = y1), linetype = 'dashed') })
  
  # BART discarding rule
  fit1 <- bartc(response = Y, treatment = Z_val, confounders = X, data = df_nonoverlap_unbalance, commonSup.rule = 'sd')
  
  output$bart_discard_result <- renderPrint({
    summary_regression_digits(fit1, type = 'bart', digits = 2)
  })
  output$bart_discard_coef <- renderPrint({
    round(summary(fit1)$estimates,2)
  })
 
  
  df_commonSup <- df_nonoverlap_unbalance[fit1$commonSup.sub,]
  SATT <- mean(df_commonSup[df_commonSup$Z==1,]$Y1 - df_commonSup[df_commonSup$Z==1,]$Y0)
  output$bart_discard_SATT <- renderText(paste('SATT:', round(SATT,2)))
  
  
  model_1 <- lm(Y1~X, data = df_commonSup)
  model_0 <- lm(Y0~X, data = df_commonSup)
  CATT <- mean(model_1$fitted.values[df_commonSup$Z==1] - model_0$fitted.values[df_commonSup$Z==1])
  output$bart_discard_CATT <- renderText(paste('CATT:', round(CATT,2)))
  
  output$bart_discard_uncertainty <- renderPlot({
    y0_hat <- fitted(object = fit1, type = 'y.0')
    y1_hat <- fitted(object = fit1, type = 'y.1')
    y.cf <- extract(fit1, 'y.cf')
    y.point <- apply(y.cf, 2, mean)
    y.sd <- apply(y.cf, 2, sd)
    ucl <- y.point + 2*y.sd 
    lcl<- y.point - 2*y.sd
    
    df_BART <- data.frame(x=df_nonoverlap_unbalance$X, y = df_nonoverlap_unbalance$Y, y0 = y0_hat, y1 = y1_hat, y.point = y.point, y.sd = y.sd, ucl = ucl, lcl = lcl)
    
    p1 <- ggplot() + geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 1, ], aes(x = X, y = Y, color = "1")) + 
      geom_point(data = df_nonoverlap_unbalance[df_nonoverlap_unbalance$Z == 0, ], aes(x = X, y = Y, color = "0")) + 
      geom_line(data = df_nonoverlap_unbalance, aes(x = X, y = 30 + exp(0.06*X)), color = "red") +
      geom_line(data = df_nonoverlap_unbalance, aes(x = X, y = 12 + 3*sqrt(X)), color = "blue") +
      scale_color_manual("Treated?", values=colors) + 
      theme_minimal(base_line_size = 0.4) + theme(legend.position="bottom") + 
      geom_line(data = df_BART, aes(x = x, y = y0), linetype = 'dashed') +
      geom_line(data = df_BART, aes(x = x, y = y1), linetype = 'dashed') +
      geom_circle(data = df_nonoverlap_unbalance[!fit1$commonSup.sub,], aes(x0 = X, y0 = Y, r = 0.6)) + 
      geom_errorbar(data = df_BART, aes(x = x, y = y, ymin = lcl, ymax = ucl), color = 'lightgrey')
    
    ites <- extract(fit1, type = "ite")
    ite.m <- apply(ites, 2, mean)
    ite.sd <- apply(ites, 2, sd)
    ite.lb <- ite.m - 2 * ite.sd
    ite.ub <- ite.m + 2 * ite.sd
    
    df_ite <- data.frame(x=df_nonoverlap_unbalance$X, ucl = ite.lb, lcl = ite.ub)
    idx <- which(df_nonoverlap_unbalance$Z == 1)
    p2 <- ggplot() + 
      geom_line(data = df_nonoverlap_unbalance[idx, ], 
                aes(x = X, y = 30 + exp(0.06*X) - 12 - 3*sqrt(X), color = "red")) +
      geom_errorbar(data = df_ite[idx,], aes(x = x, ymin = lcl, ymax = ucl), color = 'grey') +
      theme_minimal(base_line_size = 0.4) + ylab('treatment effect') + 
      theme(legend.position="bottom", legend.title = element_text(color = "transparent"), 
            legend.text = element_text(color = "transparent")) + 
      scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 0)))
    
    ggarrange(p1, p2, nrow = 1, ncol = 2)
  })
  
  observeEvent(input$reset_imbalance_lackoverlap,{
    updateSliderInput(
      # session = session,
      inputId = 'slider_imbalance_lackoverlap',
      value = 1)
  })

}

shinyApp(ui = ui, server = server)    














