# ----- ----- ----- ----- title
#
# Title: thinkCausal Shiny App
# Subtitle: Page - regression for causal
# 
# Data Created: 03/15/2021
# Data Modified: 04/28/2021
#
# ----- ----- ----- ----- version
# 
# Ver: 1.03
# 
# Description: illustrate why ignorability is important for causal inference 
#              using regression analysis.

# Dependencies -----------------------------------------------------------------
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

# Color palette ----------------------------------------------------------------
blue <- "#335C81"
red <- "#F42C04"

# UI ---------------------------------------------------------------
ui <- navbarPage(

  title = "Dev - Page - Regression for Causal",
  
  #      Fit a regression model with simulated data
  tabPanel(title="Ignorability", class = "reg-ignor",
           withMathJax(),
           div(    id = "reg-ignor-title",
                   h3("The Ignorability Assumption in Causal Inference")
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
               checkboxInput(inputId="var6", label = "Variable 6", value = TRUE),
               checkboxInput(inputId="var7", label = "Variable 7", value = TRUE),
               checkboxInput(inputId="var8", label = "Variable 8", value = FALSE),
               checkboxInput(inputId="var9", label = "Variable 9", value = TRUE),
               checkboxInput(inputId="var_bi", label = "Variable 10 (Binary)", value = TRUE),
               numericInput(inputId="sampsize", label="Sample Size:", value = 1000, min = 10),
               actionButton(inputId="gen", label = "Resample")
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
               br(),
               plotOutput("plot0", width = "80%"),
               br(),
               tableOutput("res0"),
               br(),
               verbatimTextOutput("res1"),
               br(),
               verbatimTextOutput("res2")
             ) # <END lr-simulator-sidebarLayout-mainPanel>
           ) # <END lr-simulator-sidebarLayout>
  ), # <END lr-simulator>
  
  # Theme ----
  theme = shinytheme("lumen")
  
) # <END navbarPage> 
# <END ui>

# Server -----------------------------------------------------------------------
server <- function (input, output) {
  
  toListen <- reactive({
    list(
      input$var1,
      input$var2,
      input$var3,
      input$var4,
      input$var5,
      input$var6,
      input$var7,
      input$var8,
      input$var9,
      input$var_bi,
      input$tau,
      input$sampsize,
      input$gen
    )
  })

  observeEvent(
    eventExpr = toListen(), 
    handlerExpr = {
      # Reduce contrast to reveal the noise in the Unbiased category -----------
      X <- rnorm(n = input$sampsize, mean = 5, sd = 1)
      var1 <- rnorm(n = input$sampsize, mean = 1, sd = 3)
      var2 <- rnorm(n = input$sampsize, mean = 1, sd = 3)
      var3 <- rnorm(n = input$sampsize, mean = 1, sd = 3)
      var4 <- rnorm(n = input$sampsize, mean = 1, sd = 3)
      var5 <- rnorm(n = input$sampsize, mean = 1, sd = 3)
      var6 <- rnorm(n = input$sampsize, mean = 1, sd = 4)
      var7 <- rnorm(n = input$sampsize, mean = 1, sd = 4)
      var8 <- rnorm(n = input$sampsize, mean = 1, sd = 4)
      var9 <- rnorm(n = input$sampsize, mean = 1, sd = 44)
      var_bi <- sample(c(0, 50), size = input$sampsize, replace = TRUE)
      Y0 <- X + error(input$sampsize)
      if (input$var1) {
        Y0 <- Y0 + var1
      }
      if (input$var2) {
        Y0 <- Y0 + var2
      }
      if (input$var3) {
        Y0 <- Y0 + var3
      }
      if (input$var4) {
        Y0 <- Y0 + var4
      }
      if (input$var5) {
        Y0 <- Y0 + var5
      }
      if (input$var6) {
        Y0 <- Y0 + var6
      }
      if (input$var7) {
        Y0 <- Y0 + var7
      }
      if (input$var8) {
        Y0 <- Y0 + var8
      }
      if (input$var9) {
        Y0 <- Y0 + var9
      }
      if (input$var_bi) {
        Y0 <- Y0 + var_bi
      }
      Y_0 <- X + var1 + var2 + var3 + var4 + var5 + 
        var6 + var7 + var8 + var9 + var_bi + error(input$sampsize)
      Y1 <- X + var1 + var2 + var3 + var4 + var5 + 
                var6 + var7 + var8 + var9 + var_bi + input$tau + error(input$sampsize)
      Z <- sample(c(0, 1), size = input$sampsize, replace = TRUE)
      Y <- ifelse(
        Z == 1, 
        yes = Y1,
        no = Y0
      )
      data0 <- data.frame(Y, X, Z)
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
      if (input$var6) {
        data0 <- cbind(data0, var6)
      }
      if (input$var7) {
        data0 <- cbind(data0, var7)
      }
      if (input$var8) {
        data0 <- cbind(data0, var8)
      }
      if (input$var9) {
        data0 <- cbind(data0, var9)
      }
      if (input$var_bi) {
        data0 <- cbind(data0, var_bi)
      }
      Y <- ifelse(
        Z == 1, 
        yes = Y1,
        no = Y_0
      )
      data1 <- data.frame(
        X, var1, var2, var3, var4, var5, var6, var7, var8, var9, var_bi, Z, Y
      )
      
      mod_lm <- lm(Y ~ ., data = data0)
      mod_lm_full <- lm(Y ~ ., data = data1)
      
      unbiased_Z <- summary(mod_lm_full)$coefficients['Z', 'Estimate']
      sd_unbiased <- summary(mod_lm_full)$coefficients['Z', 'Std. Error']
      estimated_Z <- summary(mod_lm)$coefficients['Z', 'Estimate']
      sd_estimated <- summary(mod_lm)$coefficients['Z', 'Std. Error']
      
      dat_out <- data.frame(
        Category = c("Actual", "Unbiased", "Problematic"),
        Estimate = c(input$tau, unbiased_Z, estimated_Z),
        Std.Dev = c(0, sd_unbiased, sd_estimated)
      )

    output$plot0 <- renderPlot(
      ggplot(dat_out, aes(x = Category, y = Estimate)) + 
        geom_point(aes(color = Category), size = 5) + 
        geom_errorbar(aes(ymin = Estimate - Std.Dev, ymax = Estimate + Std.Dev), 
                      width = .2,
                      position = position_dodge(.9),
                      size = .5) +
        theme_minimal() + 
        xlab("") +
        ylab("Estimated Treatment Effects")
    )
    output$plot1 <- renderPlot(
      ggplot() +
        geom_point(data = data0, aes(x = X, y = Y0), color = "blue", size = 1) +
        geom_point(data = data1, aes(x = X, y = Y1), color = "red", size = 1) +
        theme_minimal() + 
        xlab("Pretest") +
        ylab("Outcome")
    )
    output$res0 <- renderTable(expr = {
      xtable(dat_out)
    })
    
  }) # <END server-lr-simulator>
} # <END server>

# Generate the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
