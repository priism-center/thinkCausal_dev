data_node <- tabPanel(
  title = 'Data', 
         tabsetPanel(
           tabPanel("Load", fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel( # input: file type
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
                        div(id = "upload_file_div",
                          fileInput(inputId = "file", 
                                    label = "Choose File",
                                    buttonLabel = 'Browse',
                                    multiple = FALSE,
                                    accept = NULL),
                        ),
                        
                        # is there a header col?
                        checkboxInput("header", "Header", TRUE)
                      ),
                      mainPanel(
                        # insert output
                      )
                    )
           ),
           # 
           # tabPanel("Transform Data", fluid = TRUE,
           #          h4("Pivoting Data Wide/Long or Transposing from row to column form"),
           #          sidebarLayout(
           #            sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
           #            mainPanel(
           #              # insert output
           #            )
           #          )
           # ),
           
           
           tabPanel("Select Data", fluid = TRUE,
                    hr('Indicate Treatment Variable, Outcome Variable and Counfounders'),
                    sidebarLayout(
                      sidebarPanel(h4("Select Variables:"),  
                                   # Column Selection for Z, and identify treatment
                                   selectInput("zcol", "Select Treatment (Z) Column", choices = NULL),
                                   
                                   selectInput("trt.ind", "Select the Value Representing Receiving Treatment",
                                               choices = NULL),      
                                   
                                   selectInput("ycol", "Select Response (Y) Column", 
                                               choices = NULL), 
                                   
                                   selectInput("xcol", "Select Covariates (X) Columns", 
                                               choices = NULL, 
                                               multiple = TRUE)), 
                      mainPanel(
                        # insert output
                      )
                    )
           ), 
           
           tabPanel("Study Design", fluid = TRUE,
                    sidebarLayout(
                      sidebarPanel(width = 5, 
                      h4("Indicate Study Design"),
                      radioButtons(inputId = "exp.dsn", label = 'Select Assignment of Treatment (Z):', 
                                   choices = c('Non-Random (Observational)', 
                                               'Random (Experemental)', 
                                               'Quasi-Random (Natural Experement)'))),
                      mainPanel(
                        # insert output
                      )
                    )
           )
         ))
