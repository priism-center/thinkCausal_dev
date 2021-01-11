fit <- navbarMenu(title = "Fit Causal models", 
                    tabPanel(title = "Load Data", 
                             sidebarLayout(
                               
                               # Sidebar Panel
                               sidebarPanel(
                                 
                                 # Input: file type
                                 selectInput("filetype", "Select File Type", 
                                             choices = c("csv" = "csv", 
                                                         "dta" = "dta",
                                                         "xlsx" = "xlsx",
                                                         "txt" = "txt",
                                                         "spss" = "spss")),
                                 
                                 # Input: file
                                 fileInput("file", "Choose File",
                                           multiple = FALSE,
                                           accept = NULL),
                                 
                                 hr(),
                                 
                                 # Input: header
                                 checkboxInput("header", "Header", TRUE),
                                 
                                 hr(),
                                 
                                 # Column Selection for X and Y
                                 selectInput("xcol", "Select Covariates (X) Columns", 
                                             choices = NULL, multiple = TRUE),
                                 selectInput("ycol", "Select Response (Y) Column", choices = NULL),
                                 
                                 # Column Selection for Z, and identify treatment
                                 selectInput("zcol", "Select Treatment (Z) Column", choices = NULL),
                                 
                                 #conditionalPanel(
                                 #  condition = "input.zcol",
                                 selectInput("trt.ind", "Select the Value Representing Receiving Treatment", 
                                             choices = NULL),
                                 #)
                                 
                                 # Grouping Variable
                                 checkboxInput("gvarcheck", "Include Grouping Variable?", FALSE), 
                                 
                                 conditionalPanel(
                                   condition = "input.gvarcheck",
                                   selectInput("gvar", "Select Grouping Variable", choices = NULL)
                                 )
                                 
                               ),
                               
                               # Main Panel
                               mainPanel(
                                 
                                 # Output: Data file
                                 h4("Status"),
                                 textOutput("uploadconfirm"),
                                 #textOutput("variableconfirm"),
                                 hr(),
                                 h4("Data"),
                                 DT::dataTableOutput("uploads")
                               )
                             )
                    ),
                  
                  
                    tabPanel(title = "Specify Model", 
                             sidebarPanel(
                               width = 4,
                               h3("Insert Options"),
                               h5("some options")))
                  )
  
 