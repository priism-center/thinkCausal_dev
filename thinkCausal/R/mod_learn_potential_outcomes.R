#' learn_potential_outcomes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_potential_outcomes_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = 'learning-page',
      # shinyjs::useShinyjs(),

      # load custom css
      # includeCSS(file.path('www', 'learning', 'potential-outcomes', 'css', 'potential-outcomes.css')),

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes1.md')),
        fluidRow(column(width = 6,
                        selectInput(inputId = ns("which_outcome"), label =
                                      "Which shoes will Alex use:",
                                    choices = c("Alex uses the HyperShoe" = 'hyper',
                                                "Alex does not use the HyperShoe" = 'normal'),
                                    multiple = F),
                        actionButton(inputId = ns('submit'),
                                     label = 'Submit',
                                     class = 'nav-path',
                                     style = 'width: min(100%, 350px)')
        )),
        br(),br(),
        uiOutput(ns('output_based_on_selection')),
        br(),br(),br(),br(),br(),
        # includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes_related.md')),
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes_citations.md'))
      )

    )
  )
}

#' learn_potential_outcomes Server Functions
#'
#' @noRd
mod_learn_potential_outcomes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$submit, {

      # only allow user to choose once
      shinyjs::disable("which_outcome")

      # user chooses Alex uses HyperShoe
      if(input$which_outcome == 'hyper'){
        output$output_based_on_selection <- renderUI({
          tagList(
            img(
              src = file.path("www", "learn", "potential-outcomes", "illustrations", "PO_treated.png"),
              align = "center", width = "90%"),
            br(),br(),
            includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes2_treated.md')),
            actionButton(inputId = ns("hyper_conterfactual"),
                         label = "Show me what if Alex wasn't using the HyperShoe",
                         class = 'nav-path',
                         style = "width: min(100%, 350px)"),
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
            reactable::reactableOutput(ns("hyper_po_table")),
            uiOutput(ns("hyper_po_notation_text")),
            reactable::reactableOutput(ns("hyper_po_notation_table"))
          )
        })
      }else{ # user chooses Alex uses normal shoe
        output$output_based_on_selection <- renderUI({
          tagList(
            img(
              src = file.path("www", "learn", "potential-outcomes", "illustrations", "PO_control.png"),
              align = "center", width="90%"),
            br(),br(),
            includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes2_control.md')),
            actionButton(inputId = ns("normal_conterfactual"),
                         label = "Show me what if Alex was using the HyperShoe",
                         class = 'nav-path',
                         style = "width: min(100%, 350px)"),
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
            reactable::reactableOutput(ns("normal_po_table")),
            uiOutput(ns("normal_po_notation_text")),
            reactable::reactableOutput(ns("normal_po_notation_table"))
          )
        })
      }

    })

    # Alex uses HyperShoe ---------------------------------------------------------------

    observeEvent(input$hyper_conterfactual,{

      output$hyper_text_1 <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes3_treated.md'))
      })

      output$hyper_text_2 <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes4.md'))
      })

      output$hyper_po_table_text <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes5.md'))
      })

      output$hyper_po_notation_text <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes6.md'))
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
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "Factual_conterfactual_treated.png"),
          width = "90%"
        )
      }, deleteFile = FALSE)

      output$hyper_hist_illustration <- renderImage({
        list(
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "hist_treated.png"),
          width = "90%"
        )
      }, deleteFile = FALSE)

      output$hyper_eight_runners_1 <- renderImage({
        list(
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "eight_runners_1_hyper.png"),
          style = "max-width: 90%; max-height: 800px;"
        )
      }, deleteFile = FALSE)

      output$hyper_eight_runners_2 <- renderImage({
        list(
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "eight_runners_2.png"),
          style = "max-width: 90%; max-height: 800px;"
        )
      }, deleteFile = FALSE)

      output$hyper_po_notation_table <- reactable::renderReactable({
        data.frame(
          runner = 1:8,
          shoe = c(1,0,0,1,0,1,0,0),
          y0 = c("3.20", "2.95", "2.95", "3.30", "3.00", "2.80", "3.00", "2.90"),
          y1 = c("3.10", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80"),
          y = c("3.10", "2.95", "2.95", "3.20", "3.00", "2.70", "3.00", "2.90")
        ) %>%
          reactable::reactable()
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

      output$hyper_po_table <- reactable::renderReactable({
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
          reactable::reactable()
      })
    })



    # Alex uses standard shoe ----------------------------------------------------------------

    observeEvent(input$normal_conterfactual,{

      output$normal_text_1 <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes3_control.md'))
      })

      output$normal_text_2 <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes4.md'))
      })

      output$normal_po_table_text <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes5.md'))
      })

      output$normal_po_notation_text <- renderUI({
        includeMarkdown(app_sys("app", "www", "learn", "potential-outcomes", "markdowns", 'potential_outcomes6.md'))
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
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "Factual_conterfactual_control.png"),
          width = "90%"
        )
      }, deleteFile = FALSE)

      output$normal_hist_illustration <- renderImage({
        list(
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "hist_control.png"),
          width = "90%"
        )
      }, deleteFile = FALSE)

      output$normal_eight_runners_1 <- renderImage({
        list(
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "eight_runners_1_normal.png"),
          style = "max-width: 90%; max-height: 800px;"
        )
      }, deleteFile = FALSE)

      output$normal_eight_runners_2 <- renderImage({
        list(
          src = app_sys("app", "www", "learn", "potential-outcomes", "illustrations", "eight_runners_2.png"),
          style = "max-width: 90%; max-height: 800px;"
        )
      }, deleteFile = FALSE)

      output$normal_po_notation_table <- reactable::renderReactable({
        data.frame(
          runner = 1:8,
          shoe = c(0,0,0,1,0,1,0,0),
          y0 = c("3.20", "2.95", "2.95", "3.30", "3.00", "2.80", "3.00", "2.90"),
          y1 = c("3.10", "2.85", "2.80", "3.20", "2.85", "2.70", "2.95", "2.80"),
          y = c("3.20", "2.95", "2.95", "3.20", "3.00", "2.70", "3.00", "2.90")
        ) %>%
          reactable::reactable()
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
      output$normal_po_table <- reactable::renderReactable({
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
          reactable::reactable()
      })
    })

  })
}
