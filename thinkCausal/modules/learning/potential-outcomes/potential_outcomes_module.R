# this defines the potential_outcomes module
# this script was created by `create_learning_module()`
# the article name is 'Potential outcomes' and the short name is 'potential_outcomes'

require(shiny)
require(shinyjs)
require(tidyr)
require(DT)
# require(tibble)
# require(ggplot2)


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_potential_outcomes <- list()

# set path
store_l_potential_outcomes$path_to_here <- file.path('modules', 'learning', 'potential-outcomes')

# read in randomization df
# store_l_potential_outcomes$potential_outcomes_df <- readr::read_csv(
#   file.path(store_l_potential_outcomes$path_to_here, 'data', 'potential_outcomes_df.csv'),
#   col_types = readr::cols(
#     h1 = readr::col_double(),
#     z = readr::col_logical(),
#     bugs = readr::col_logical()
#   )
# )
# store_l_potential_outcomes$potential_outcomes_df$z_as_text <- ifelse(store_l_potential_outcomes$potential_outcomes_df$z, 'Treatment', 'Control')


# namespace ---------------------------------------------------------------

# pay close attention to how the namespace is managed. Since the quiz module
# is inside the post-treatment module, its namespace is a 'child' of the post-treatment
# module. The post-treatment module namespace id is set within module_ids object in global.R.
# The quiz module id is always 'quiz' which then becomes 'learning_post_treatment-quiz'
# UI and server elements outside of the quiz can be treated as you would normal modules

# ns id is set as 'learning_post_treatment' per module_ids$learning$post_treatment

# set namespace for UI quiz elements
store_l_potential_outcomes$ns_quiz <- NS(NS(module_ids$learning$potential_outcomes)('quiz'))


# source quiz and R functions ---------------------------------------------

purrr::walk(list.files(file.path(store_l_potential_outcomes$path_to_here, 'R'), full.names = TRUE), source)


# UI ----------------------------------------------------------------------

ui_learning_potential_outcomes <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      class = 'learning-page',
      useShinyjs(),
      
      # load custom css
      includeCSS(file.path('www', 'learning', 'potential-outcomes', 'css', 'potential-outcomes.css')),
      
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes1.md')),
        fluidRow(column(width = 6,
                        selectInput(inputId = ns("which_outcome"), label = 
                                      "Which shoes will Alex use:", 
                                    choices = c("Alex uses the HyperShoe" = 'hyper',
                                                "Alex does not use the HyperShoe" = 'normal'), 
                                    multiple = F),
                        actionButton(ns('submit'), 
                                     'Submit'))),
        br(),br(),
        uiOutput(ns('output_based_on_selection')),
        br(),br(),br(),br(),br(),
        wellPanel(includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes_related.md'))),
        includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes_citations.md'))
      ),
      
      # the quiz UI
      # can be moved within the UI; recommended to place at top of UI if not embedding
      # ui_quiz(id = ns('quiz')) # comment out this line if quiz is not required

    )
  )
}


# server ------------------------------------------------------------------

server_learning_potential_outcomes <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz", # this should always be quiz
        id_parent = module_ids$learning$potential_outcomes,
        question_texts = store_l_potential_outcomes$quiz()$question_texts,
        question_prompts = store_l_potential_outcomes$quiz()$question_prompts,
        correct_answers = store_l_potential_outcomes$quiz()$correct_answers,
        message_correct = store_l_potential_outcomes$quiz()$message_correct,
        message_wrong = store_l_potential_outcomes$quiz()$message_wrong,
        message_skipped = store_l_potential_outcomes$quiz()$message_skipped,
        embed_quiz = TRUE
      )
      
     
      observeEvent(input$submit, {
        
        # only allow user to choose once
        shinyjs::disable("which_outcome")
        
        # user chooses Alex uses HyperShoe
        if(input$which_outcome == 'hyper'){
          output$output_based_on_selection <- renderUI({
            tagList(
              img(
                src = "learning/potential-outcomes/illustrations/PO_treated.png",
                align = "center", width = "90%"),
              br(),br(),
              includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes2_treated.md')),
              actionButton(ns("hyper_conterfactual"), "Show me what if Alex wasn't using the HyperShoe", style="width: min(100%, 400px)"),
              br(),br(),
              imageOutput(ns('hyper_conterfactual_illustration'), height = NULL),
              br(),br(),br(),br(),br(),br(),
              uiOutput(ns("hyper_text_1")),
              imageOutput(ns('hyper_hist_illustration'), height = NULL),
              br(),br(),br(),br(),
              uiOutput(ns("hyper_text_2")),
              br(),
              imageOutput(ns('hyper_eight_runners_1'), width = "100%", height = NULL),
              br(),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              imageOutput(ns('hyper_eight_runners_2'), width = "100%", height = NULL),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              br(),br(),br(),
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
              img(
                src = 'learning/potential-outcomes/illustrations/PO_control.png',
                align = "center", width="90%"),
              br(),br(),
              includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes2_control.md')),
              actionButton(ns("normal_conterfactual"), "Show me what if Alex was using the HyperShoe"),
              br(),br(),
              imageOutput(ns('normal_conterfactual_illustration'), height = NULL),
              br(),br(),br(),br(),br(),br(),
              uiOutput(ns("normal_text_1")),
              imageOutput(ns('normal_hist_illustration'), height = NULL),
              br(),br(),br(),br(),
              uiOutput(ns("normal_text_2")),
              br(),
              imageOutput(ns('normal_eight_runners_1'), width = "100%", height = NULL),
              br(),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              imageOutput(ns('normal_eight_runners_2'), width = "100%", height = NULL),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              br(),br(),br(),
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
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes3_treated.md'))
        })
        
        output$hyper_text_2 <- renderUI({
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes4.md'))
        })
        
        output$hyper_po_table_text <- renderUI({
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes5.md'))
        })
        
        output$hyper_po_notation_text <- renderUI({
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, "markdowns", 'potential_outcomes6.md'))
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
            src = './www/learning/potential-outcomes/illustrations/Factual_conterfactual_treated.png',
            width = "90%"
          )
        }, deleteFile = FALSE)
        
        output$hyper_hist_illustration <- renderImage({
          list(
            src = './www/learning/potential-outcomes/illustrations/hist_treated.png',
            width = "90%"
          )
        }, deleteFile = FALSE)
        
        output$hyper_eight_runners_1 <- renderImage({
          list(
            src = './www/learning/potential-outcomes/illustrations/eight_runners_1_hyper.png',
            style = "max-width: 90%; max-height: 800px;"
          )
        }, deleteFile = FALSE)
        
        output$hyper_eight_runners_2 <- renderImage({
          list(
            src = './www/learning/potential-outcomes/illustrations/eight_runners_2.png',
            style = "max-width: 90%; max-height: 800px;"
          )
        }, deleteFile = FALSE)
        
        output$hyper_po_notation_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:8,
            shoe = c(1,0,0,1,0,1,0,0),
            y0 = c("3.20", "2.95", "2.95", "3.30", "3.00", "2.80", "3.00", "2.90"),
            y1 = c("3.10", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80"),
            y = c("3.10", "2.95", "2.95", "3.20", "3.00", "2.70", "3.00", "2.90")
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
          standard_shoe <- c("3.20", "2.95", "2.95", "3.30", "3.00", "2.80", "3.00", "2.90")
          hypershoe <- c("3.10", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80")
        }else {
          standard_shoe <- c("", "2.95", "2.95", "", "3.00", "", "3.00", "2.90")
          hypershoe <- c("3.10", "", "", "3.20", "", "2.70", "", "")
        }
        
        output$hyper_po_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:8,
            shoe = c("HyperShoe", "Standard shoe", "Standard shoe", "HyperShoe", 
                     "Standard shoe", "HyperShoe", "Standard shoe", "Standard shoe"),
            standard_shoe = standard_shoe,
            hypershoe = hypershoe,
            observed = c("3.10", "2.95", "2.95", "3.20", "3.00", "2.70", "3.00", "2.90")
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
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, 'markdowns', 'potential_outcomes3_control.md'))
        })
        
        output$normal_text_2 <- renderUI({
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, 'markdowns', 'potential_outcomes4.md'))
        })
        
        output$normal_po_table_text <- renderUI({
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, 'markdowns', 'potential_outcomes5.md'))
        })
        
        output$normal_po_notation_text <- renderUI({
          includeMarkdown(file.path(store_l_potential_outcomes$path_to_here, 'markdowns', 'potential_outcomes6.md'))
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
            src = './www/learning/potential-outcomes/illustrations/Factual_conterfactual_control.png',
            width = "90%"
          )
        }, deleteFile = FALSE)
        
        output$normal_hist_illustration <- renderImage({
          list(
            src = './www/learning/potential-outcomes/illustrations/hist_control.png',
            width = "90%"
          )
        }, deleteFile = FALSE)
        
        output$normal_eight_runners_1 <- renderImage({
          list(
            src = './www/learning/potential-outcomes/illustrations/eight_runners_1_normal.png',
            style = "max-width: 90%; max-height: 800px;"
          )
        }, deleteFile = FALSE)
        
        output$normal_eight_runners_2 <- renderImage({
          list(
            src = './www/learning/potential-outcomes/illustrations/eight_runners_2.png',
            style = "max-width: 90%; max-height: 800px;"
          )
        }, deleteFile = FALSE)
        
        output$normal_po_notation_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:8,
            shoe = c(0,0,0,1,0,1,0,0),
            y0 = c("3.20", "2.95", "2.95", "3.30", "3.00", "2.80", "3.00", "2.90"),
            y1 = c("3.10", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80"),
            y = c("3.20", "2.95", "2.95", "3.20", "3.00", "2.70", "3.00", "2.90")
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
          standard_shoe <- c("3.20", "2.95", "2.95", "3.30", "3.00", "2.80", "3.00", "2.90")
          hypershoe <- c("3.10", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80")
        }else {
          standard_shoe <- c("3.20", "2.95", "2.95", "", "3.00", "", "3.00", "2.90")
          hypershoe <- c("", "", "", "3.20", "", "2.70", "", "")
        }
        
        # create table
        output$normal_po_table <- DT::renderDataTable({ 
          data.frame(
            runner = 1:8,
            # Alex (the first one) wears standard shoe
            shoe = c("Standard shoe", "Standard shoe", "Standard shoe", "HyperShoe", 
                     "Standard shoe", "HyperShoe", "Standard shoe", "Standard shoe"),
            standard_shoe = standard_shoe,
            hypershoe = hypershoe,
            observed = c("3.20", "2.95", "2.95", "3.20", "3.00", "2.70", "3.00", "2.90")
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
