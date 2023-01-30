
library(shiny)
library(bartCause)
library(tidyverse)
library(aciccomp2016)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(2), 
    column(6,
  'Intro text')
  ), 
  
  fluidRow(
    column(2), 
    column(6, 
           actionButton('draw', label = 'Draw a new sample'
           )
    )), 
  
  fluidRow(
    column(2), 
    column(6, 
           plotOutput('plot1')
           )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data("input_2016")
  dat <- input_2016
  n <- 200
  dat <- fastDummies::dummy_cols(dat, select_columns = c('x_2', 'x_21', 'x_24'), remove_selected_columns = TRUE)
  names(dat) <- paste0('x_', 1:length(dat))
  counter <- reactiveVal(0)

  draw_sample <- eventReactive(input$draw, {
     counter(counter() + 1)
     print(counter())
     set.seed(counter())
    index <- sample(1:nrow(dat), 200)
    X <- dat[index,]
    oracle <- dgp_2016(input_2016, 2, 1)
    y <- oracle$y[index]
    z <- oracle$z[index]
    ice <- oracle$y.1[index] - oracle$y.0[index]
    att <- round(mean(ice[z == 1]), 2)
    
    predictors <- n / 10
    adjust_prediction_y <- names(tail(lm(y ~ 0 + ., X)$coeff[order(lm(y ~ 0 + ., X)$coeff)], predictors))
    adjust_prediction_z <- names(tail(glm(z ~ 0 + ., X, family = binomial)$coeff[order(lm(y ~ 0 + ., X)$coeff)], predictors))
    
    p.z <- vector()
    for (i in 1:length(dat)) {
      coef <- glm(z ~ 0 + X[, i], X, family = binomial)$coef
      se <-
        sqrt(diag(vcov(glm(
          z ~ 0 + X[, i], X, family = binomial
        ))))
      p.z[i] <- 2 * (1 - pt(abs(coef / se), 99))
      
    }
    
    p.z[is.na(p.z)] <- F
    coefs <- lm(y ~ 0 + ., X)$coeff
    coefs <- coefs[p.z < .05][order(abs(coefs[p.z < .05]))] %>% na.omit()
    mix <- names(tail(coefs, predictors))
      
    allX <-
      summary(bartc(y, z, X, estimand = 'att'), target = 'sate')
    top10z <-
      summary(bartc(y, z, X[, adjust_prediction_z], estimand = 'att'), target = 'sate')
    top10y <-
      summary(bartc(y, z, X[, adjust_prediction_y], estimand = 'att'), target = 'sate')
    mix_model <-
      summary(bartc(y, z, X[, mix], estimand = 'att'), target = 'sate')
    random <-
      summary(bartc(y, z, X[, sample(1:length(dat), predictors)], estimand = 'att'), target = 'sate')
    
    
    results <-
      rbind(
        allX$estimates,
        top10y$estimates,
        top10z$estimates,
        mix_model$estimates,
        random$estimates
      )
    results$model <-
      c(
        'all covariates',
        'top 20 predictors of y',
        'top 20 predictors of z',
        'top 20 predictors of y that are significantly associated with z',
        '20 randomly selected covaraites'
      )
    
    print(results);print(att)
    
    p <- ggplot(results, aes(estimate, model)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height = .05) +
      geom_vline(xintercept = att, linetype = 2) +
      geom_label(x = att,
                 y = 5.3,
                 label = paste('True ATT =', att)) +
      labs(y = element_blank()) +
      theme_bw()
    
    return(p)
    
  })
  


  
  output$plot1 <- renderPlot({
    draw_sample()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
