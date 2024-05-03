#' rct_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_confounder_ui <- function(id){
  ns <- NS(id)
  tagList(

    use_scrollytell(ns = ns),

    div(
      class = 'learning-page',

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',

        h1('Analyzing Data from Observational Studies'),
        includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'intro.md')),
        br(),br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section1.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section2.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section3.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 4,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section4.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 5,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section5.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 6,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section6.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 7,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section7.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 8,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section8.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 9,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section9.md')),
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 10,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section10.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 11,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section11.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 12,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section12.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 13,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section13.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 14,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section14.md')),
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 15,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section15.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 16,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section16.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 17,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section17.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 18,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section18.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 19,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section19.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 20,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section20.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 21,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section21.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 22,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section22.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 23,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section23.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 24,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section24.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 25,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section25.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 26,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section26.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 27,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section27.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 28,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section28.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 29,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section29.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 30,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section30.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 31,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section31.md')),
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 32,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section32.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 33,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section33.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 34,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section34.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 35,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section35.md')),
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 36,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section36.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 37,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section37.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 38,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section38.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 39,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section39.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 40,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section40.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 41,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section41.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 42,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section42.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 43,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section43.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 44,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section44.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 45,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section45.md')),
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 46,
            div(
              mod_quiz_ui(id = ns('quiz')),
              br(),
              br()
            )
          ),
          scroll_ui_quiz_section(
            ns = ns,
            position = 47,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section47.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 48,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section48.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 49,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section49.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 50,
            includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section50.md')),
          )
        ),
        scroll_ui_visual(ns = ns, clickable = F)
      ),
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        includeMarkdown(app_sys("app", "www", "learn", "confounder", "markdowns", 'section51.md')),
        sidebarLayout(
        sidebarPanel(
          sortable::bucket_list(
            header = NULL, #HTML('<b><center>Variables included in the analysis</center></b>'),
            #"Drag the variables to their respective roles",
            group_name = ns("dragdrop"),
            orientation = "horizontal",
            class = 'default-sortable sortable-wide',
            sortable::add_rank_list(
              input_id = ns("dragdrop_included"),
              text = "Covariates included in the analysis:",
              labels = NULL,
              #auto_columns$X,
              options = sortable::sortable_options(multiDrag = TRUE)
            ),
            sortable::add_rank_list(
              input_id = ns("dragdrop_avalable"),
              text = "Covariates excluded from the analysis:",
              labels = c("bw", "b.head", "preterm", "birth.o", "nnhealth", "momage",
                         "sex", "twin", "b.marr", "mom.lths", "mom.hs", "mom.scoll", "cig",
                         "first", "booze", "drugs", "work.dur", "prenatal"),
              #auto_columns$X,
              options = sortable::sortable_options(multiDrag = TRUE)
            ))
        ),
        mainPanel(
          plotOutput(outputId = ns('bias'))
        )),
        includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational_learn_more.md')),
        includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational_citations.md'))
      )
    )
  )
}


#' learn_scrolly_example Server Functions
#'
#' @noRd
mod_learn_confounder_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # run the quiz
    shinyQuiz::quiz_server(quiz_content_confounding)

    output$scroll_visual <- renderUI({

      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p1.png'),
               contentType = 'image/png',
               width = '80%'
              )
        }, deleteFile = F)
      )

      items$position2 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p2.rds'))
        })
      )

      items$position3 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p3.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position4 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p4.rds'))
        })
      )

      items$position5 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p5.rds'))
        })
      )

      items$position6 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p6.rds'))
        })
      )

      items$position7 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p7.rds'))
        })
      )

      items$position8 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p8.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position9 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p9.rds'))
        })
      )

      items$position10 <-  div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p10.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position11 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p11.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position12 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p12.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position13 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p13.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position14 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p14.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )
      items$position15 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p15.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position16 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p16.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position17 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p17.rds'))
        })
        )

      items$position18 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p18.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position19 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p19.rds'))
        })
      )

      items$position20 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p20.rds'))
        })
      )

      items$position21 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p21.rds'))
        })
      )

      items$position22 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p22.rds'))
        })
      )

      items$position23 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p23.rds'))
        })
      )

      items$position24 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p24.rds'))
        })
      )

      items$position25 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p25.rds'))
        })
      )

      items$position26 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p26.rds'))
        })
      )

      items$position27 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p27.rds'))
        })
      )

      items$position28 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p28.rds'))
        })
      )

      items$position29 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p29.rds'))
        })
      )

      items$position30 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p30.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position31 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p31.rds'))
        })
      )

      items$position32 <- div(
        style = 'visibility: hidden;',
        NULL
      )

      items$position33 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p33.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position34 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p34.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position35 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p35.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F)
      )

      items$position36 <- div(
        NULL # no image
      )

      items$position37 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p37.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F),
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p37.rds'))
        })
      )


      items$position38 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p38.rds'))
        })
      )

      items$position39 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p39.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F),
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p39.rds'))
        })

      )

      items$position40 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p40.rds'))
        })
      )


      items$position41 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p41.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F),
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p41.rds'))
        })

      )

      items$position42 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p42.rds'))
        })
      )

      items$position43 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/confounder/plots/p43.png'),
               contentType = 'image/png',
               width = '80%'
          )
        }, deleteFile = F),
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p43.rds'))
        })

      )

      items$position44 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p44.rds'))
        })
      )

      items$position45 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p45.rds'))
        })
      )

      items$position46 <- div(
        style = 'visibility: hidden;',
        NULL # quiz
      )

      items$position47 <- div(
        style = 'visibility: hidden;',
        NULL # quiz
      )

      items$position48 <- div(
        style = 'visibility: hidden;',
        renderUI({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p48.rds'))
        })
      )

      items$position49 <- div(
        style = 'visibility: hidden;',
        renderUI({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p49.rds'))
        })
      )

      items$position50 <- div(
        style = 'visibility: hidden;',
        renderUI({
          readr::read_rds(app_sys('app', 'www/learn/confounder/plots/p50.rds'))
          })

      )



      return(items)
    })


    ihdp_obs <- readr::read_rds(app_sys('app', 'www/learn/confounder/ihdp_obs.rds'))

    output$bias <- renderPlot({
      call <- ifelse(length(input$dragdrop_included) == 0L,
             'y~ihdp',
             paste0(c('y~ihdp', input$dragdrop_included), collapse = '+'))
      fit <- lm(as.formula(call), data = ihdp_obs)
      est <- fit$coef['ihdp']

      tibble(est) |>
      ggplot2::ggplot(aes(est, 'estimated ate')) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_vline(aes(xintercept = 4, linetype = 'True ATE')) +
        ggplot2::scale_linetype_manual(values = 2) +
        ggplot2::geom_linerange(aes(xmin = confint(fit)['ihdp', 1],
                                    xmax = confint(fit)['ihdp', 2])) +
        ggplot2::xlim(c(-5, 30)) +
        labs(x = 'estimated ATE', y = NULL, title = 'Observational Study Results') +
        ggplot2::theme_classic() +
        theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())



    })

  })
}

