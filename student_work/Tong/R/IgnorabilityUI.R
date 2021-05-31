# UI ---------------------------------------------------------------
IgnorabilityUI <- function(id){
  ns <- NS(id)
  navbarPage(
  
  title = "Concepts - All Confounders Measured",
  
  tabPanel(title="Ignorability", class = "reg-ignor",
           withMathJax(),
           div( id = "reg-ignor-title",
                h3("The Ignorability Assumption in Causal Inference")),
           hr(), 
           # Sidebar: a slider and selection inputs ----
           sidebarLayout(
             sidebarPanel(
               numericInput(inputId=ns("tau"), label = "Treatment Effect:", value = 5, min=0),
               hr(),
               fluidRow(
               column(6,
                      p("Non-Confounders:"),
                      checkboxInput(inputId=ns("var1"), label = "Variable 1", value = FALSE),
                      checkboxInput(inputId=ns("var3"), label = "Variable 3", value = FALSE),
                      checkboxInput(inputId=ns("var5"), label = "Variable 5", value = FALSE)
               ),
               column(6,
                      p("Confounders"),
                      checkboxInput(inputId=ns("var2"), label = "Variable 2", value = FALSE),
                      checkboxInput(inputId=ns("var4"), label = "Variable 4", value = FALSE),
                      checkboxInput(inputId=ns("var6"), label = "Variable 6", value = FALSE),
                      checkboxInput(inputId=ns("var7"), label = "Variable 7", value = FALSE),
                      checkboxInput(inputId=ns("var8"), label = "Variable 8", value = FALSE),
                      checkboxInput(inputId=ns("var9"), label = "Variable 9", value = FALSE),
                      checkboxInput(inputId=ns("var_bi"), label = "Variable 10 (Binary)", value = FALSE))
             ),
               sliderInput(inputId = ns("sampsize"), label = "Sample Size:",
                           min = 10, max = 1000, value = 500, step = 10),
               actionButton(inputId=ns("gen"), label = "Resample")
             ), 
             
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
               plotOutput(ns("plot0"), width = "80%"),
               br(),
               tableOutput(ns("res0")),
               br(),
               verbatimTextOutput(ns("res1")),
               br(),
               verbatimTextOutput(ns("res2"))
             ) 
           ) 
  ), 
  
  # Theme ----
  theme = shinytheme("lumen")
  
) 
}