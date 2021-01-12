fit <- navbarMenu(title = "Fit Causal models",
                  
# Load Data ---------------------------------------------------------------
tabPanel("Load Data",
         #sidebar layout
         sidebarLayout(
           
           #sidebar panel
           sidebarPanel(
             # input: file type
             selectInput(
               "filetype",
               "Select File Type",
               choices = c(
                 "csv" = "csv",
                 "dta" = "dta",
                 "xlsx" = "xlsx",
                 "txt" = "txt",
                 "spss" = "spss"
               )
             ),
             # find and select input file
             fileInput("file", "Choose File",
                       multiple = FALSE,
                       accept = NULL),
             
             # is there a header col?
             checkboxInput("header", "Header", TRUE)
             
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
         ))
         
         ), 

# Study Design ------------------------------------------------------------
tabPanel(title = "Describe Data", 
         sidebarLayout(
           sidebarPanel(
             # study design
             radioButtons("design", 
                          label = h6("Study Design:"), 
                          choices = list(
                            "Observational Study" = "obs",
                            "Randomized Experement" = 'rct',
                            "Natural Experement" = "nat"
                          ), selected = character(0)), 
             
             hr(), 
             # If random, is randomization blocked? 
             conditionalPanel(condition = "input.design == 'rct'", 
                              radioButtons("blocked", 
                                           label = h6("Blocked Design"),
                                           choices = c("no", "yes"))), 
             
             conditionalPanel(condition = "input.blocked == 'yes' && input.design== 'rct'", 
                              selectInput("blockvar", 
                                          label = h6("Select Blocking Variable(s):"), 
                                          choices = NULL)), 
             
             radioButtons("gvarcheck",
                           label = h6("Grouping Variable(s):"), 
                          choices = c("No", "Yes"), 
                          selected = character(0)),
             
             conditionalPanel(condition = "input.gvarcheck == 'Yes'",
                              selectInput("gvar", "Select Grouping Variable(s):", 
                                          choices = NULL))
             
           ), 
           mainPanel()
         ))



)


