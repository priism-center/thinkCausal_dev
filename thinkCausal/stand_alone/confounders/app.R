library(shiny)
library(shinyWidgets)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(1), 
        column(10,
    h2('All Confounders Measured'),
    h4('Choose an example: '),
    wellPanel(
        awesomeRadio(
            inputId = 'example',
            label = 'Options',
            choices = c('health', 'education', 'economics', 'exercise science'),
            selected = 'health'
        )
    ), 
    
    p("You want to understand if participating in a 6 month high intensity interval training program 
      casuses a decrease in blood sugar. 
      Besides blood sugarat the end of the 6 months program and treatment status (exercise program or no exercise program), 
      you are able to take measurements of baseline blood sugar, baseline exercise levels, 
      baseline water consumption and baseline salt consumption."),
    
    p("How these covaraites influence your causal 
      analysis depends on wether participation in the exercse program was done 
      through random assigment or if the study is an observational study and 
      particpants decided themsevels wehtehr to enrol in the program or not"), 
    br(), 
    
    p("Use the buttons below to switch between a randomized design and an observational design.
      Look for how the relationship between covaraites and participation in the exercise program 
      differs between the two designs"),
    
    wellPanel(
        awesomeRadio(inputId = 'world.1', 
                     label = 'Study Design:', 
                     choices = c('Randomized', 'Observational')),
        
        selectInput(inputId = 'confounder.plt.input', 
                    label = 'Select a Covariate:', 
                    choices = c('baseline_blood_sugar',
                                   'baseline_exercise', 
                                   'baseline_salt', 
                                   'baseline_water'), 
                    )
    ),
    
    plotOutput('rct.plt.1'), 
    
    p("Add Description of how RCT and obs are different"),
    
   h3('Covariates with Random Treatment Assignment'), 
   
   p('In the above section you saw that under random assignment, 
     the treatment variable (participation in the exercise program)
     is independent from all coviaraites.')
   
    
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    set.seed(1234)
    rct <- tibble(baseline_water = rnorm(200), 
                  baseline_salt = rnorm(200), 
                  baseline_exercise = rnorm(200), 
                  baseline_blood_sugar = rnorm(200), 
                  z = rbinom(200, 1, .5))
    
    rct <- rct %>% mutate(y0 = 110 + 2*baseline_blood_sugar + 
                            1*baseline_exercise + 
                            rnorm(200, 0, 1))
    
    
    rct <- rct %>% mutate(y1 = 110 + 2*baseline_blood_sugar + 
                          1*baseline_exercise + 
                            rnorm(200, 0, 1) - 10)
    
    rct <- rct %>% mutate(y = if_else(z ==1, y1, y0))
    
    rct$z <- ifelse(rct$z == 1, 'exercise program', 'no exercise program')
  

    
    
    # obs dgp 
    obs <- rct %>% select(1:4)
    beta.z <- c(-1,  -1, -1)
    p.score <- as.vector(pnorm(as.matrix(obs[,2:4]) %*% beta.z))
    obs$z <- rbinom(200, 1, p.score)

    
    obs <- obs %>% mutate(y1 = 110 + 2*baseline_blood_sugar + 
                            1*baseline_exercise + 
                            rnorm(200, 0, 1) - 10)
    
    obs <- obs %>% mutate(y0 = 110 + 2*baseline_blood_sugar + 
                            1*baseline_exercise + 
                            rnorm(200, 0, 1))
    
    obs <- obs %>% mutate(y = if_else(z ==1, y1, y0))
    obs$z <- ifelse(obs$z == 1, 'exercise program', 'no exercise program')
    
   
    output$rct.plt.1 <- renderPlot({
      if(input$world.1 == 'Randomized'){
        p <- ggplot(rct, aes_string(input$confounder.plt.input)) + 
          geom_density(aes(fill = z), alpha = .5) + 
          theme_bw() + 
          theme(legend.title = element_blank(), 
                legend.position = 'top')  
      }
      
      if(input$world.1 == 'Observational'){
        p <- ggplot(obs, aes_string(input$confounder.plt.input)) + 
          geom_density(aes(fill = z), alpha = .5) + 
          theme_bw() + 
          theme(legend.title = element_blank(), 
                legend.position = 'top') 
      }
        return(p)

    })
    
    
    
    
}


# Run the application
shinyApp(ui = ui, server = server)


summary(lm(y~Z, data = rct))

summary(lm(y~Z + baseline_blood_sugar, data = rct))


plot_model_fit <- function(dat, covariates = NULL){
  
  model_dat <- dat %>% dplyr::select('y', 'z')
  
  est <- summary(lm(y~., model_dat))

  no_cov_df <- tibble(
    estimate = est$coefficients[2,1],
    ucl = est$coefficients[2,1] + 1.96*est$coefficients[2,2],
    lcl = est$coefficients[2,1] - 1.96*est$coefficients[2,2],
    `Treatment Assignment` = 'No Covariates Included')
  
  model_dat <- dat %>% dplyr::select('y', 'z',covariates)
  
  est <- summary(lm(y~., model_dat))
  
  cov_df <- tibble(
    estimate = est$coefficients[2,1],
    ucl = est$coefficients[2,1] + 1.96*est$coefficients[2,2],
    lcl = est$coefficients[2,1] - 1.96*est$coefficients[2,2],
    `Treatment Assignment` = 'With Selected Covariates')
  
  df <- rbind.data.frame(no_cov_df, cov_df)
  
  true_df <- tibble(
    estimate = 10,
    lab = 'True Treatment Effect'
  )
  p <- ggplot() +
    geom_point(data = df, aes(`Treatment Assignment`, estimate, col = `Treatment Assignment`), size = 2) +
    geom_errorbar(data =df, aes(x = `Treatment Assignment`, ymin = lcl, ymax = ucl), width  = .05) +
    geom_hline(yintercept = 10) +
    geom_label(data = true_df, aes(label = lab,  x = 1.5, y = 10.1), size = 3) +
    coord_cartesian(ylim = c(9,11)) +
    labs(y = 'Estimated Treatment Effects') + 
    theme_bw() + 
    theme(legend.position = 'bottom' ,
          legend.title = element_blank()) 
  
  return(p)
}

plot_model_fit(dat = rct, covariates = c("baseline_blood_sugar"))

study <- function(){

  rct <- tibble(baseline_water = rnorm(200), 
                baseline_salt = rnorm(200), 
                baseline_exercise = rnorm(200), 
                baseline_blood_sugar = rnorm(200), 
                z = rbinom(200, 1, .5))
  
  rct <- rct %>% mutate(y0 = 110 + 2*baseline_blood_sugar + 
                          1*baseline_exercise + 
                          rnorm(200, 0, 1))
  
  
  rct <- rct %>% mutate(y1 = 110 + 2*baseline_blood_sugar + 
                          1*baseline_exercise + 
                          rnorm(200, 0, 1) - 10)
  
  rct <- rct %>% mutate(y = if_else(z ==1, y1, y0))
  
  rct$z <- ifelse(rct$z == 1, 'exercise program', 'no exercise program')
  
  
  diff.mean <- lm(y~z, data = rct)
  covariates<- c('baseline_blood_sugar', "baseline_exercise", "baseline_salt")
  rct_variables <- rct %>% select('y','z', covariates)
  reg <- lm(y ~ ., rct_variables)
  result <-  c(diff.mean$coefficients[2], reg$coefficients[2])
  names(result) <- NULL
  
  return(result)
}


results <- replicate(1000, study())

dim(results)
df <- tibble(estimates = c(results[1,], results[2,]), 
       model = c(rep("without covariates", 1000), rep("with covariates", 1000)), 
       sample = c(1:1000, 1:1000))
df <- df %>% 
  split(.$sample) %>% 
  accumulate(~ bind_rows(.x, .y)) %>% 
  bind_rows(.id = "frame") %>% 
  mutate(frame = as.integer(frame))

p_anim <- ggplot(data = df, aes(x = estimates, fill = model)) +
  geom_histogram(bins = 30, alpha = .5, position = 'identity')

anim <- p_anim + transition_manual(frame) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
animate(anim,nframes = 100, renderer = gifski_renderer(loop = F))