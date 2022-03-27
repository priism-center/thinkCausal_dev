require(shiny)
require(shinyjs)
require(tidyr)
require(DT)
# store_l_potential_outcomes <- list()

# set path
# store_l_potential_outcomes$path_to_here <- file.path('modules', 'learning', 'potential-outcomes-new')


# UI ----------------------------------------------------------------------

ui_learning_potential_outcomes_new <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      useShinyjs(),
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        # includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes1.md')),
        includeMarkdown(file.path('markdowns', 'potential_outcomes1.md')),
        fluidRow(column(width = 6,
                        selectInput(inputId = ns("which_outcome"), label = "Which shoes will Alex use:", 
                                    choices = c("Alex uses the HyperShoe" = 'hyper',
                                                "Alex does not use the HyperShoe" = 'normal'), 
                                    multiple = F),
                        actionButton(ns('submit'), 'Submit'))),
        br(),br(),
        uiOutput(ns('output_based_on_selection'))
      )
    )
  )
}

server_learning_potential_outcomes_new <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$submit, {
        
        # only allow user to choose once
        shinyjs::disable("which_outcome")
        
        # user chooses Alex uses HyperShoe
        if(input$which_outcome == 'hyper'){
          output$output_based_on_selection <- renderUI({
            tagList(
            img(src='PO_treated.png', align = "center", height="100%", width="100%"),
            br(),br(),
            # includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes2.md')),
            includeMarkdown(file.path('markdowns', 'potential_outcomes2_treated.md')),
            actionButton(ns("hyper_conterfactual"), "Show me what if Alex wasn't using the HyperShoe"),
            br(),br(),
            imageOutput(ns('hyper_conterfactual_illustration')),
            br(),br(),br(),br(),br(),br(),
            uiOutput(ns("hyper_text_1")),
            imageOutput(ns('hyper_hist_illustration')),
            br(),br(),br(),br(),
            uiOutput(ns("hyper_text_2")),
            imageOutput(ns('hyper_eight_runners_1')),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            imageOutput(ns('hyper_eight_runners_2')),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            uiOutput(ns("hyper_po_table_text")),
            uiOutput(ns("hyper_which_po_table")),
            dataTableOutput(ns("hyper_po_table")),
            uiOutput(ns("hyper_po_notation_text")),
            dataTableOutput(ns("hyper_po_notation_table"))
            )
          })
        }else{ # user chooses Alex uses normal shoe
          output$output_based_on_selection <- renderUI({
            tagList(
            img(src='PO_control.png', align = "center", height="100%", width="100%"),
            br(),br(),
            # includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes2.md')),
            includeMarkdown(file.path('markdowns', 'potential_outcomes2_control.md')),
            actionButton(ns("normal_conterfactual"), "Show me what if Alex was using the HyperShoe"),
            br(),br(),
            imageOutput(ns('normal_conterfactual_illustration')),
            br(),br(),br(),br(),br(),br(),
            uiOutput(ns("normal_text_1")),
            imageOutput(ns('normal_hist_illustration')),
            br(),br(),br(),br(),
            uiOutput(ns("normal_text_2")),
            imageOutput(ns('normal_eight_runners_1')),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            imageOutput(ns('normal_eight_runners_2')),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            uiOutput(ns("normal_po_table_text")),
            uiOutput(ns("normal_which_po_table")),
            dataTableOutput(ns("normal_po_table")),
            uiOutput(ns("normal_po_notation_text")),
            dataTableOutput(ns("normal_po_notation_table"))
            )
          })
        }
        
      })
      
# Alex uses HyperShoe ---------------------------------------------------------------
      
      observeEvent(input$hyper_conterfactual,{
        
          output$hyper_text_1 <- renderUI({
            includeMarkdown(file.path('markdowns', 'potential_outcomes3_treated.md'))
          })
          
          output$hyper_text_2 <- renderUI({
            includeMarkdown(file.path('markdowns', 'potential_outcomes4.md'))
          })
          
          output$hyper_po_table_text <- renderUI({
            includeMarkdown(file.path('markdowns', 'potential_outcomes5.md'))
          })
          
          output$hyper_po_notation_text <- renderUI({
            includeMarkdown(file.path('markdowns', 'potential_outcomes6.md'))
          })
          
          output$hyper_which_po_table <- renderUI({
            selectInput(inputId = ns("hyper_which_po_mode"), label = "Switch mode:", 
                        choices = c("Omniscience mode" = 'omniscience',
                                    "Researcher mode" = 'researcher'), 
                        selected = "omniscience",
                        multiple = F)
          })
          
          output$hyper_conterfactual_illustration <- renderImage({
            list(
              src = file.path('www', 'Factual_conterfactual_treated.png'),
              width = "100%", height = "120%"
            )
          }, deleteFile = FALSE)
          
          output$hyper_hist_illustration <- renderImage({
            list(
              src = file.path('www', 'hist_treated.png'),
              width = "100%", height = "120%"
            )
          }, deleteFile = FALSE)
          
          output$hyper_eight_runners_1 <- renderImage({
            list(
              src = file.path('www', 'eight_runners_1.png'),
              width = "100%", height = "200%"
            )
          }, deleteFile = FALSE)
          
          output$hyper_eight_runners_2 <- renderImage({
            list(
              src = file.path('www', 'eight_runners_2.png'),
              width = "100%", height = "200%"
            )
          }, deleteFile = FALSE)
          
          output$hyper_po_notation_table <- DT::renderDataTable({ 
            data.frame(
              runner = 1:9,
              shoe = c(0,1,1,0,1,1,0,1,0),
              y0 = c("3.10", "2.90", "2.95", "3.40", "3.20", "3.00", "2.95", "3.05", "2.95"),
              y1 = c("3.00", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80", "3.00"),
              y = c("3.10", "2.85", "2.80", "3.40", "2.85", "2.70", "2.95", "2.80", "2.95")
            ) %>% 
              DT::datatable(
                selection = 'none',
                rownames = FALSE,
                options = list(
                  paging = FALSE,
                  lengthChange = FALSE, #  removes option to change n observations shown
                  sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
                  scrollX = TRUE,
                  info = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
                ))
          })
      })
      
      # create po table
      observeEvent(input$hyper_which_po_mode,{
        
        if (input$hyper_which_po_mode %in% 'omniscience'){
          standard_shoe <- c("3.10", "2.90", "2.95", "3.40", "3.20", "3.00", "2.95", "3.05", "2.95")
          hypershoe <- c("3.00", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80", "3.00")
        }else {
          standard_shoe <- c("3.10", "", "", "3.40", "", "", "", "3.05", "2.95")
          hypershoe <- c("3.00", "2.85", "2.80", "", "2.85", "2.70", "2.95", "", "")
        }
          
        output$hyper_po_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:9,
            shoe = c("Standard shoe", "HyperShoe", "HyperShoe", "Standard shoe",
                     "HyperShoe", "HyperShoe", "HyperShoe", "Standard shoe", "Standard shoe"),
            standard_shoe = standard_shoe,
            hypershoe = hypershoe,
            observed = c("3.10", "2.85", "2.80", "3.40", "2.85", "2.70", "2.95", "2.80", "2.95")
          ) %>% 
            rename("Finishing time with standard shoe" = standard_shoe, 
                   "Finishing time with HyperShoe" = hypershoe,
                   "Observed Finishing Time" = observed) %>% 
            DT::datatable(
              selection = 'none',
              rownames = FALSE,
              options = list(
                paging = FALSE,
                lengthChange = FALSE, #  removes option to change n observations shown
                sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
                scrollX = TRUE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
        })
      })
      
      
      
# Alex uses standard shoe ----------------------------------------------------------------
      
      observeEvent(input$normal_conterfactual,{
        
        output$normal_text_1 <- renderUI({
          includeMarkdown(file.path('markdowns', 'potential_outcomes3_control.md'))
        })
        
        output$normal_text_2 <- renderUI({
          includeMarkdown(file.path('markdowns', 'potential_outcomes4.md'))
        })
        
        output$normal_po_table_text <- renderUI({
          includeMarkdown(file.path('markdowns', 'potential_outcomes5.md'))
        })
        
        output$normal_po_notation_text <- renderUI({
          includeMarkdown(file.path('markdowns', 'potential_outcomes6.md'))
        })
        
        output$normal_which_po_table <- renderUI({
          selectInput(inputId = ns("normal_which_po_mode"), label = "Switch mode:", 
                      choices = c("Omniscience mode" = 'omniscience',
                                  "Researcher mode" = 'researcher'), 
                      selected = 'omniscience',
                      multiple = F)
        })
        
        output$normal_conterfactual_illustration <- renderImage({
          list(
            src = file.path('www', 'Factual_conterfactual_control.png'),
            width = "100%", height = "120%"
          )
        }, deleteFile = FALSE)
        
        output$normal_hist_illustration <- renderImage({
          list(
            src = file.path('www', 'hist_control.png'),
            width = "100%", height = "120%"
          )
        }, deleteFile = FALSE)
        
        output$normal_eight_runners_1 <- renderImage({
          list(
            src = file.path('www', 'eight_runners_1.png'),
            width = "100%", height = "200%"
          )
        }, deleteFile = FALSE)
        
        output$normal_eight_runners_2 <- renderImage({
          list(
            src = file.path('www', 'eight_runners_2.png'),
            width = "100%", height = "200%"
          )
        }, deleteFile = FALSE)
        
        output$normal_po_notation_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:9,
            shoe = c(0,1,1,0,1,1,0,1,0),
            y0 = c("3.10", "2.90", "2.95", "3.40", "3.20", "3.00", "2.95", "3.05", "2.95"),
            y1 = c("3.00", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80", "3.00"),
            y = c("3.10", "2.85", "2.80", "3.40", "2.85", "2.70", "2.95", "2.80", "2.95")
          ) %>% 
            DT::datatable(
              selection = 'none',
              rownames = FALSE,
              options = list(
                paging = FALSE,
                lengthChange = FALSE, #  removes option to change n observations shown
                sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
                scrollX = TRUE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
        })
      })
      
      
      observeEvent(input$normal_which_po_mode,{
        
        if (input$normal_which_po_mode %in% 'omniscience'){
          standard_shoe <- c("3.10", "2.90", "2.95", "3.40", "3.20", "3.00", "2.95", "3.05", "2.95")
          hypershoe <- c("3.00", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80", "3.00")
        }else {
          standard_shoe <- c("3.10", "", "", "3.40", "", "", "", "3.05", "2.95")
          hypershoe <- c("3.00", "2.85", "2.80", "", "2.85", "2.70", "2.95", "", "")
        }
        
        # create table
        output$normal_po_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:9,
            shoe = c("Standard shoe", "HyperShoe", "HyperShoe", "Standard shoe",
                     "HyperShoe", "HyperShoe", "HyperShoe", "Standard shoe", "Standard shoe"),
            standard_shoe = standard_shoe,
            hypershoe = hypershoe,
            observed = c("3.10", "2.85", "2.80", "3.40", "2.85", "2.70", "2.95", "2.80", "2.95")
          ) %>% 
            rename("Finishing time with standard shoe" = standard_shoe, 
                   "Finishing time with HyperShoe" = hypershoe,
                   "Observed Finishing Time" = observed) %>% 
            DT::datatable(
              selection = 'none',
              rownames = FALSE,
              options = list(
                paging = FALSE,
                lengthChange = FALSE, #  removes option to change n observations shown
                sDom  = '<"top">lrt<"bottom">ip', # removes the search bar
                scrollX = TRUE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
        })
      })
  
    }
  )
}


ui <- fluidPage(
  ui_learning_potential_outcomes_new(id = "potential_outcomes")
)

server <- function(input, output, session) {
  server_learning_potential_outcomes_new(id = "potential_outcomes")
}

shinyApp(ui, server)




