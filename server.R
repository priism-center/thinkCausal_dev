library(shiny)
require(foreign)
require(readstata13)
require(openxlsx)
require(tidyverse)
require(DT)

shinyServer(function(input, output, session) {
    
my_data <- reactive({
    req(input$file, input$filetype)
    if (input$filetype == "csv") {
        df <- read.csv(input$file$datapath, header = input$header)
    }
    
    else if (input$filetype == "dta") {
        df <- read.dta13(input$file$datapath)
    }
    
    else if (input$filetype == "xlsx") {
        df <- read.xlsx(input$file$datapath, colNames = input$header)
    }
    
    else if (input$filetype == "txt") {
        df <- read.table(input$file$datapath, header = input$header)
    }
    
    else if (input$filetype == "spss") {
        df <- read.spss(input$file$datapath, to.data.frame = T)
    }
    
    df
})

# Upload Data tab outputs
output$uploads <- DT::renderDataTable({
    req(input$file)
    req(my_data())
    return(my_data())
})

output$uploadconfirm <- renderText({
    # dataset validation
    datacheck <- function(data, confound, trt, resp) {
        if (is.null(data)) {
            "Please Upload a dataset"
        }
        
        else {
            
            if (is.null(confound) || trt == "" || resp == "") {
                "Please identify X, Y, Z"
            }
            
            else {
                
                if (length(unique(my_data()[, which(names(my_data()) == trt)])) > 2) {
                    "Please check treatment variable selection, and/or missing values"
                }
                
                else {
                    NULL
                }
            }
        }
    }
    
    validate(
        datacheck(input$file, input$xcol, input$zcol, input$ycol)
    )
    # datacheck in functions.R
    paste("Upload complete")
})



# Updating column selection
observe({
    req(input$file)
    vars <- names(my_data())
    updateSelectInput(session, "idcol", choices = vars)
    updateSelectInput(session, "xcol", choices = vars)
    updateSelectInput(session, "zcol", choices = vars)
    updateSelectInput(session, "ycol", choices = vars)
    updateSelectInput(session, "blockvar", choices = vars)
    updateSelectInput(session, "gvar", choices = vars)
})

})