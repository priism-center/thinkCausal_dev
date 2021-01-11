library(shiny)
library(shinythemes)

shinyUI(navbarPage(
    id = "nav",
    title = 'Options:',
    introduction, 
    learn_ci, 
    fit
))
