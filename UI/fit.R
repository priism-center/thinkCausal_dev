fit <- navbarMenu(title = "Fit Causal models", 
                  

# Load Data ---------------------------------------------------------------
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
                                 
                                 # Describe Data
                                 h4("Describe Data"),
                                 
                                 # Grouping Variable
                                 checkboxInput("gvarcheck", "Include Grouping Variable?", FALSE), 
                                 
                                 conditionalPanel(
                                   condition = "input.gvarcheck",
                                   selectInput("gvar", "Select Grouping Variable", choices = NULL)
                                 ),
                                 
                                 # Randomized
                                 radioButtons("rand", label = h6("Treatment Assignment"),
                                              choices = list("Non-Random" = 1, "Random" = 2), 
                                              selected = 1),
                                 
                                 # blocked option if random is selected
                                 conditionalPanel(
                                   condition = 'input.rand == 2',
                                   selectInput('blockcheck', 'Blocked Design:', 
                                               choices = c('No', 'Yes'))
                                 ),
                                 
                                 # select blocking variables (if appropriate)
                                 conditionalPanel(
                                   condition = "input.blockcheck == 'Yes'", 
                                   selectInput("blockvar", "Select Blocking Variable(s)", choices = NULL)
                                 ),

                                 # Column Selection for Z, and identify treatment
                                 selectInput("zcol", "Select Treatment (Z) Column", choices = NULL),
                                 
                                 #  condition = "input.zcol",
                                 selectInput("trt.ind", "Type the Value Representing Receiving Treatment", 
                                             choices = NULL),
                                 
                                 # select outcome (Y) variable
                                 selectInput("ycol", "Select Outcome (Y) Column", choices = NULL)
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
                  

# Specify Model -----------------------------------------------------------
tabPanel(title = "Specify Model",
         sidebarLayout(
           sidebarPanel(radioButtons(
             "estimand",
             label = h6("Select An Estimatamd:"),
             choices = list(
               "ATE" = 1,
               "ATT" = 2,
               "ATC" = 3,
               "CATE" = 4
             )
           )),
           
           mainPanel(
             # Output: Data file
             h4("Status"),
             textOutput("uploadconfirm"),
             #textOutput("variableconfirm"),
             hr(),
             h4("Data"),
             DT::dataTableOutput("uploads")
           )
           
         ))
)
  
 