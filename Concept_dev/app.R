# ----- ----- ----- ----- title
#
# Title: thinkCausal Shiny App
# Subtitle: Page - regression for causal
# 
# Data Created: 03/15/2021
# Data Modified: 04/01/2021
#
# ----- ----- ----- ----- version
# 
# Ver: 1.1
# 
# Description: illustrate why ignorability is important for causal inference 
#              using regression analysis.

library(shiny)
library(shinythemes)
library(xtable)
library(MASS)
library(ggplot2)

# Function ---------------------------------------------------------------------
error <- function (n) {
  # generate random individual-level errors
  return (
    rnorm(n = n, mean = 0, sd = 1)
  )
}
alpha_generator <- function(pretest, lwr, upp) {
  # Calculates the log-odds scale for treatment 
  # Param: pretest: a vector of pretest scores 
  # Param: lwr: lower bound of the alpha
  # Param: upp: upper bound of the alpha
  
  # Compute the lower and upper bounds of pretest
  lwr_pre <- log(lwr / (1 - lwr))
  upp_pre <- log(upp / (1 - upp))
  # Compare the log-odds scale of lower and upper bounds and return the lowest lwr_alpha <- lwr_pre / min(pretest)
  lwr_alpha <- lwr_pre / min(pretest)
  upp_alpha <- upp_pre / max(pretest)
  return(min(lwr_alpha, upp_alpha))
}

# Color palette ----------------------------------------------------------------
blue <- "#335C81"
red <- "#F42C04"

# UI ---------------------------------------------------------------
ui <- navbarPage(

  title = "Test - Page - Regression for Causal",
  
  #      Fit a regression model with simulated data
  tabPanel(title="Ignorability", class = "reg-ignor",
           withMathJax(),
           div(    id = "reg-ignor-title",
                   h3("Why Ignorability is Important")
           ),
           hr(), 
           # Sidebar: a slider and selection inputs ----
           sidebarLayout(
             sidebarPanel(
               numericInput(inputId="tau", label = "Treatment Effect:", value = 5, min=0),
               hr(),
               checkboxInput(inputId="var1", label = "Variable 1", value = TRUE),
               checkboxInput(inputId="var2", label = "Variable 2", value = TRUE),
               checkboxInput(inputId="var3", label = "Variable 3", value = TRUE),
               checkboxInput(inputId="var4", label = "Variable 4", value = TRUE),
               checkboxInput(inputId="var5", label = "Variable 5", value = TRUE),
               numericInput(inputId="sampsize", label="Sample Size:", value = 1000, min = 10)
             ), # <END lm-simulator-sidebarLayout-sidebarPanel>
             
             mainPanel(
               div(    id = "tab-lm-simulator-info",
                       p(tags$b("Ignorability assumption: "),
                         "The distribution of classes across the treatment
                         variable is random with respect to the potential outcomes."),
                       p("If tretment assignment is conditionally independent of 
                         y_0 and y_1, then the treatment assignment is said 
                         to be strongly ignorable."),
                       p("$$Y_0, Y_1 \\perp T | X$$")
               ),
               actionButton(inputId="generate", label = "Generate Data and Fit Model"),                 
               br(),
               plotOutput("plot0", width = "80%"),
               br(),
               verbatimTextOutput("res0"),
               br()
             ) # <END lr-simulator-sidebarLayout-mainPanel>
           ) # <END lr-simulator-sidebarLayout>
  ), # <END lr-simulator>
  
  # Theme ----
  theme = shinytheme("lumen")
  
) # <END navbarPage> 
# <END ui>

# Server -----------------------------------------------------------------------
server <- function (input, output) {
  
  dgp <- eventReactive(
    eventExpr = input$generate, 
    valueExpr = {
      var1 <- rnorm(n = input$sampsize, mean = 5, sd = 2)
      var2 <- rnorm(n = input$sampsize, mean = 6, sd = 2)
      var3 <- rnorm(n = input$sampsize, mean = 7, sd = 2)
      var4 <- rnorm(n = input$sampsize, mean = 8, sd = 2)
      var5 <- rnorm(n = input$sampsize, mean = 9, sd = 2)
      Y <- var1 + var2 + var3 + var4 + var5 + error(input$sampsize)
      data0 <- data.frame(Y)
      if (input$var1) {
        data0 <- cbind(data0, var1)
      }
      if (input$var2) {
        data0 <- cbind(data0, var2)
      }
      if (input$var3) {
        data0 <- cbind(data0, var3)
      }
      if (input$var4) {
        data0 <- cbind(data0, var4)
      }
      if (input$var5) {
        data0 <- cbind(data0, var5)
      }
      return(data0)
    }
  )

  observeEvent(input$generate, {

    # beta0 <- as.numeric(summary(mod_out)$coefficients[1])
    # coefs <- as.numeric(summary(mod_out)$coefficients[2])
    # r_sq <- summary(mod_out)$r.squared
    # 
    # output$plot0 <- renderPlot(
    #   expr = {
    #     data0 <- dgp()
    #     ggplot(data0, aes(x = X, y = Y, color = factor(Z))) +
    #       geom_point() +
    #       xlab("Pretest Scores") +
    #       ylab("Observed Outcome") +
    #       labs(title = "Distribution of Observed Outcomes", color = "Treated?") +
    #       scale_color_manual(values = c(blue, red)) +
    #       theme_minimal(base_line_size = 0.4)
    #   }
    # )
    output$res0 <- renderPrint(expr = {
      lm(Y ~ ., data = dgp())
    })
    
  }) # <END server-lr-simulator>
} # <END server>

# Generate the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
