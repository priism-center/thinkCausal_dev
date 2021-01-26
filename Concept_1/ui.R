library(shiny)
library(shinythemes)
library(shinyBS)
shinyUI(navbarPage(
    id = "nav",
    title = 'thinkCausal:',
    analysis_dropdown
))







# library(shiny)
# library(plotly)
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#     tabsetPanel(
#         tabPanel("Map", fluid = TRUE,
#                  sidebarLayout(
#                      sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
#                      mainPanel(
#                          htmlOutput("Attacks")
#                      )
#                  )
#         ),
#         tabPanel("plot", fluid = TRUE,
#                  sidebarLayout(
#                      sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
#                      mainPanel(fluidRow(
#                          column(7,  plotlyOutput("")),
#                          column(5, plotlyOutput(""))   
#                      )
#                      )
#                  )
#         )
#     )
# ))
