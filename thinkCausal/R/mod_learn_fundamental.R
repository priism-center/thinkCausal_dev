#' mod_learn_fundemental UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_fundamental_ui <- function(id){
  ns <- NS(id)
  tagList(
    use_scrollytell(ns = ns),
    div(class = 'learning-page',
        div(
          class = ns('learning-content'), # required
          class = 'learning-content',  # required
          style = 'display: block;',
          includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'intro.md')),
          br(),br(),br(),br()
        ),

        scroll_ui_container(
          ns = ns,
          scroll_ui_text(
            ns = ns,
            scroll_ui_text_section(
              ns = ns,
              position = 1,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 2,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section2.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 3,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section3.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 4,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section4.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 5,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section5.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 6,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section6.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 7,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section7.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 8,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section8.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 9,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section9.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 10,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section10.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 11,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section11.md'))
            ),
            # scroll_ui_text_section(
            #   ns = ns,
            #   position = 12,
            #   includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section12.md'))
            # ),
            scroll_ui_text_section(
              ns = ns,
              position = 12,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section13.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 13,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section14.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 14,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section15.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 15,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section16.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 16,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section17.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 17,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section18.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 18,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section19.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 19,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section19.md'))
            )
          ),
          scroll_ui_visual(ns = ns, clickable = T)
          ),

        br(),br(),br(),br(),
        div(
          class = ns('learning-content'), # required
          class = 'learning-content',  # required
          style = 'display: block;',
          #includeMarkdown(app_sys("app", "www", "learn", "fundamental", "markdowns", 'intro.md')),
          br(),br(),br(),br()
        )
      )

  )
}

#' mod_learn_fundamental Server Functions
#'
#' @noRd
mod_learn_fundamental_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # load in everything we need
    datImputed <- readr::read_csv(app_sys('extdata/fundamental_table2.csv')) %>%
      dplyr::mutate(estITE = Y1 - Y0)
    datTruth <- readr::read_csv(app_sys('extdata/truth.csv'))
    datCombined <- data.frame(
      datImputed[, 1:3],
      estY0 = datImputed$Y0,
      Y0 = datTruth$Y0,
      estY1 = datImputed$Y1,
      Y1 = datTruth$Y1,
      Y = datImputed$Y,
      estITE = datImputed$Y1 - datImputed$Y0
    )
    shinyquiz::quiz_server(quiz_content_fundamental)

    tab1 <- readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
      dplyr::filter(`prior races` == 2) %>%
      dplyr::mutate(Y0 = ifelse(hyperShoe == 1, '?', Y0),
                    Y1 = ifelse(hyperShoe == 0, '?', Y1))



    correct1 <- c(250, 259, 250, 259, 259, 259, 259)
    interactive_table1 <- create_interactive_table(tab1, correct_answers = correct1)

    tab2 <-  readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
      dplyr::filter(`prior races` == 3) %>%
      dplyr::mutate(Y0 = ifelse(hyperShoe == 1, '?', Y0),
                    Y1 = ifelse(hyperShoe == 0, '?', Y1))
    correct2 <- c(250, 250, 223)
    interactive_table2 <- create_interactive_table(tab2, correct_answers = correct2)



    output$scroll_visual <- renderUI({
      items <- list()
      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        reactable::renderReactable({
          readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
            reactable::reactable(defaultPageSize = 10)
        })
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_csv('inst/extdata/fundamental_table1.csv')[1,] %>%
            reactable::reactable()
        })
      )

      # item 3
      items$position3 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          readr::read_csv('inst/extdata/fundamental_table1.csv')[2,] %>%
            reactable::reactable()
        })
      )


      items$position4 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable(
          readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
            reactable::reactable(defaultPageSize = 5)
        ),
        renderImage(
          list(src = app_sys('app', 'www/learn/fundemental/plots/p1.png'),
               contentType = 'image/png',
               width = 640,
               height = 400)
        , deleteFile = F)
      )

      items$position5 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable(
          readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
            dplyr::filter(`prior races` == 0) %>%
            dplyr::select(-Y, -Y1) %>%
            reactable::reactable(defaultPageSize = 7)
        ),
        renderImage(
          list(src = app_sys('app', 'www/learn/fundemental/plots/p2.png'),
               contentType = 'image/png',
               width = 640,
               height = 400)
          , deleteFile = F)
      )

      imputedY0 <- "#2297E6"
      Y0_pal <- function(x) ifelse(dat3$Z == 1, imputedY0, '#FFFFFF')
      dat6 <- readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
        dplyr::filter(`prior races` == 0) %>%
        dplyr::select(-Y, -Y1)
      dat6$Y0[is.na(dat6$Y0)] <- 281

      items$position6 <- div(
          style = 'visibility: hidden;',
          renderImage({
            list(src = app_sys('app', 'www/learn/fundemental/plots/p3.png'),
                 contentType = 'image/png',
                 width = 800,
                 height = 500)
          }, deleteFile = F)
        )

      items$position7 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(dat6, defaultPageSize = 7, columns = list(
            Y0 = reactable::colDef(
              style = function(value, index) {
                if (dat6$hyperShoe[index] == 1) {
                  color <- imputedY0
                } else if (dat6$hyperShoe[index] == 0) {
                  color <- NULL
                }
                list(color = color)
              })
          ))
        })
      )

      items$position8 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable(
          readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
            dplyr::filter(`prior races` == 0) %>%
            dplyr::select(-Y, -Y0) %>%
            reactable::reactable(defaultPageSize = 7)
        ),
        renderImage(
          list(src = app_sys('app', 'www/learn/fundemental/plots/p_pre4.png'),
               contentType = 'image/png',
               width = 640,
               height = 400)
          , deleteFile = F)
      )

      items$position9 <- div(
        style = 'visibility: hidden;',
        renderImage(
          list(src = app_sys('app', 'www/learn/fundemental/plots/p4.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
          , deleteFile = F)
      )


      imputedY1 <- "#DF536B"
      dat10 <- readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
        dplyr::filter(`prior races` == 0) %>%
        dplyr::select(-Y, -Y0)
      dat10$Y1[is.na(dat10$Y1)] <- 270

      items$position10 <-  div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(dat10, defaultPageSize = 7, columns = list(
            Y1 = reactable::colDef(
              style = function(value, index) {
                if (dat10$hyperShoe[index] == 0) {
                  color <- imputedY1
                } else if (dat10$hyperShoe[index] == 1) {
                  color <- NULL
                }
                list(color = color)
              })
          ))
        })
      )

      dat11 <- readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
        dplyr::filter(`prior races` == 0)

      dat11$Y1[is.na(dat11$Y1)] <- 270
      dat11$Y0[is.na(dat11$Y0)] <- 281


      items$position11 <-  div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(dat11, defaultPageSize = 7, columns = list(
            Y1 = reactable::colDef(
              style = function(value, index) {
                if (dat11$hyperShoe[index] == 0) {
                  color <- imputedY1
                } else if (dat11$hyperShoe[index] == 1) {
                  color <- NULL
                }
                list(color = color)
              }),
            Y0 = reactable::colDef(
              style = function(value, index) {
                if (dat11$hyperShoe[index] == 1) {
                  color <- imputedY0
                } else if (dat11$hyperShoe[index] == 0) {
                  color <- NULL
                }
                list(color = color)
              }
            )
          ))
        })
      )

      # dat12 <- readr::read_csv('inst/extdata/fundamental_table1.csv')
      # dat12$Y1[is.na(dat12$Y1) & dat12$`prior races` == 0] <- 270
      # dat12$Y0[is.na(dat12$Y0) & dat12$`prior races` == 0] <- 281
      #
      # items$position12 <- div(
      #   style = 'visibility: hidden;',
      #   reactable::renderReactable({
      #     reactable::reactable(dat12, defaultPageSize = 5, columns = list(
      #       Y0 = reactable::colDef(
      #         style = function(value, index) {
      #           if (dat12$hyperShoe[index] == 1 & dat12$`prior races`[index] == 0) {
      #             color <- imputedY0
      #           } else {
      #             color <- NULL
      #           }
      #           list(color = color)
      #         }),
      #       Y1 = reactable::colDef(
      #         style = function(value, index) {
      #           if (dat12$hyperShoe[index] == 0 & dat12$`prior races`[index] == 0) {
      #             color <- imputedY1
      #           } else {
      #             color <- NULL
      #           }
      #           list(color= color)
      #         })
      #     ))
      #   }),
      #   renderImage(
      #     list(src = app_sys('app', 'www/learn/fundemental/plots/p1.png'),
      #          contentType = 'image/png',
      #          width = 640,
      #          height = 400)
      #     , deleteFile = F)
      # )


      dat13 <- readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
        dplyr::filter(`prior races` == 1)
      dat13$Y1[is.na(dat13$Y1) & dat13$`prior races` == 1] <- 273
      dat13$Y0[is.na(dat13$Y0) & dat13$`prior races` == 1] <- 280

      items$position12 <- div(
        style = 'visibility: hidden;',
        renderImage(
          list(src = app_sys('app', 'www/learn/fundemental/plots/p5.png'),
               contentType = 'image/png',
               width = 640,
               height = 400)
          , deleteFile = F),
        reactable::renderReactable({
          reactable::reactable(dat13, columns = list(
            Y0 = reactable::colDef(
              style = function(value, index) {
                if (dat13$hyperShoe[index] == 1 & dat13$`prior races`[index] == 1) {
                  color <- imputedY0
                } else {
                  color <- NULL
                }
                list(color = color)
              }),
            Y1 = reactable::colDef(
              style = function(value, index) {
                if (dat13$hyperShoe[index] == 0 & dat13$`prior races`[index] == 1) {
                  color <- imputedY1
                } else {
                  color <- NULL
                }
                list(color = color)
              })
          ))
        })
      )

      items$position13 <- div(
        style = 'visibility: hidden;',
        fluidRow(
          column(8,
                 renderUI({
                   HTML(interactive_table1)
                 })
                 ),
          column(4,
                 renderPlot({
                    readr::read_rds(app_sys('app', 'www/learn/fundemental/plots/p6.rds'))
                 }, height = 500, width = 400)
                 )
        )

      )

      items$position14 <- div(
        style = 'visibility: hidden;',
        fluidRow(
          column(8,
                 renderUI({
                   HTML(interactive_table2)
                 })
          ),
          column(4,
                 renderPlot({
                   readr::read_rds(app_sys('app', 'www/learn/fundemental/plots/p7.rds'))
                 }, height = 500, width = 400)
          )
        )

      )


      items$position15 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(
            datImputed %>% dplyr::select(-estITE),
            defaultPageSize = 20,
            #class = 'small',
            theme = reactable::reactableTheme(cellPadding = "1px 6px"),
            columns = list(
              Y0 = reactable::colDef(
                style = function(value, index) {
                  if (datImputed$hyperShoe[index] == 1) {
                    color <- imputedY0
                  } else {
                    color <- NULL
                  }
                  list(color = color)
                }
              ),
              Y1 = reactable::colDef(
                style = function(value, index) {
                  if (datImputed$hyperShoe[index] == 0) {
                    color <- imputedY1
                  } else {
                    color <- NULL
                  }
                  list(color = color)
                }
              )
            )
          )
        })
      )


      items$position16 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(
            datImputed,
            fullWidth = FALSE,
            defaultPageSize = 20,
            #class = 'small',
            theme = reactable::reactableTheme(cellPadding = "1px 6px"),
            defaultColDef = reactable::colDef(
              footerStyle = list(fontWeight = "bold",
                                 color = list(NULL, imputedY0, imputedY1, NULL)
                                 ),
            ),
            columns = list(
              Y0 = reactable::colDef(
                style = function(value, index) {
                  if (datImputed$hyperShoe[index] == 1) {
                    color <- imputedY0
                  } else {
                    color <- NULL
                  }
                  list(color = color)
                }
              ),
              Y1 = reactable::colDef(
                style = function(value, index) {
                  if (datImputed$hyperShoe[index] == 0) {
                    color <- imputedY1
                  } else {
                    color <- NULL
                  }
                  list(color = color)
                }
              ),
              estITE = reactable::colDef(
                name = 'estimated ITE'
              )
            )
          )
        })
      )



      items$position17 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(
            datImputed,
            fullWidth = FALSE,
            defaultPageSize = 20,
            #class = 'small',
            theme = reactable::reactableTheme(cellPadding = "1px 6px"),
            defaultColDef = reactable::colDef(
              footerStyle = list(fontWeight = "bold",
                                 color = list(NULL, imputedY0, imputedY1, NULL)
              ),
            ),
            columns = list(
              runner = reactable::colDef(
                footer = 'Average'
              ),
              Y0 = reactable::colDef(
                footer = round(mean(datImputed$Y0), 1),
                style = function(value, index) {
                  if (datImputed$hyperShoe[index] == 1) {
                    color <- imputedY0
                  } else {
                    color <- NULL
                  }
                  list(color = color)
                }
              ),
              Y1 = reactable::colDef(
                footer = round(mean(datImputed$Y1), 1),
                style = function(value, index) {
                  if (datImputed$hyperShoe[index] == 0) {
                    color <- imputedY1
                  } else {
                    color <- NULL
                  }
                  list(color = color)
                }
              ),
              estITE = reactable::colDef(
                name = 'estimated ITE',
                footer = round(mean(datImputed$Y1 - datImputed$Y0), 1)
              )
            )
          )
        })
      )

      items$position18 <- div(
        fluidRow(
        renderUI({
          shinyWidgets::radioGroupButtons(ns('view'), label = NULL,
                                          choices = c('Researcher', 'Parallel Universe', 'Oracle'),
                                          selected = 'Researcher',
                                          individual = T
          )
        })
        # ,renderUI({
        #   selectizeInput(
        #     inputId = ns('cols'),
        #     label = 'pick some ol columns:',
        #     choices = c(names(datCombined), 'estITE', 'ITE'),
        #     selected = c(names(datCombined)[3:length(datCombined)], 'estITE', 'ITE'),
        #     multiple = TRUE,
        #     options = list(maxItems = 7,
        #                    plugins = list('remove_button', 'drag_drop'))
        #   )
        # })
        ),
        renderUI({
          shinyWidgets::materialSwitch(
            inputId = ns("imputed"),
            label = "Show imputed potential outcomes",
            status = "primary",
            right = TRUE,
            value = TRUE
          )
        }),
        reactable::renderReactable({
          req(input$view)
          switch (
            input$view,
            'Researcher' = create_table_researcher(df = datImputed, imputed = input$imputed),
            'Parallel Universe' = create_table_parallel(df = datTruth, rows = 20),
            'Oracle' = create_table_oracle(df = datCombined, imputed = input$imputed, .show = input$cols, rows = 20)
            )
        })

      )



      # # item 13
      # items$position13 <- div(
      #   style = 'visibility: hidden;',
      #   renderUI({
      #     HTML(interactive_table1)
      #   })
      # )

      # # item 14
      # items$position14 <- div(
      #   style = 'visibility: hidden;',
      #   reactable::renderReactable({
      #     imputed <- "#ff9f1a"
      #     orange_pal <- function(x) ifelse(dat3$Z == 1, imputed, '#FFFFFF')
      #     dat14 <- readr::read_csv('inst/extdata/fundemental_table3.csv') %>%
      #       mutate(Y1 = ifelse(hyperShoe == 1, Y1, 256),
      #              Y0 = ifelse(hyperShoe == 0, Y0, 258))
      #
      #     reactable::reactable(dat14, defaultPageSize = 6, columns = list(
      #         Y0 = reactable::colDef(
      #           style = function(value, index) {
      #             if (dat14$hyperShoe[index] == 1) {
      #               color <- imputed
      #             } else if (dat14$hyperShoe[index] == 0) {
      #               color <- 'white'
      #             }
      #             list(background = color)
      #           }),
      #         Y1 = reactable::colDef(
      #           style = function(value, index) {
      #             if (dat14$hyperShoe[index] == 0) {
      #               color <- imputed
      #             } else if (dat14$hyperShoe[index] == 1) {
      #               color <- 'white'
      #             }
      #             list(background = color)
      #           })
      #       ))
      #   })
      # )

      # item 15
      # items$position15 <- div(
      #   style = 'visibility: hidden;',
      #   renderImage({
      #     list(src = app_sys('app', 'www/learn/fundemental/plots/p9.png'),
      #          contentType = 'image/png',
      #          width = 800,
      #          height = 500)
      #   }, deleteFile = F)
      # )
      #
      # # item 16
      # items$position16 <- div(
      #   style = 'visibility: hidden;',
      #   renderImage({
      #     list(src = app_sys('app', 'www/learn/fundemental/plots/p10.png'),
      #          contentType = 'image/png',
      #          width = 800,
      #          height = 500)
      #   }, deleteFile = F)
      # )
      #
      # # item 17
      # items$position17 <- div(
      #   style = 'visibility: visible;',
      #   reactable::renderReactable({
      #     readr::read_csv('inst/extdata/fundemental_table4.csv') %>%
      #       reactable::reactable(defaultPageSize = 6)
      #   })
      # )
      #
      # # item 18
      # items$position18 <- div(
      #   style = 'visibility: hidden;',
      #   renderImage({
      #     list(src = app_sys('app', 'www/learn/fundemental/plots/p11.png'),
      #          contentType = 'image/png',
      #          width = 800,
      #          height = 500)
      #   }, deleteFile = F)
      # )
      #
      # # item 19
      # items$position19 <- div(
      #   style = 'visibility: hidden;',
      #   renderImage({
      #     list(src = app_sys('app', 'www/learn/fundemental/plots/p12.png'),
      #          contentType = 'image/png',
      #          width = 800,
      #          height = 500)
      #   }, deleteFile = F)
      # )
      #
      # # item 20
      # items$position20 <- div(
      #   style = 'visibility: hidden;',
      #   renderUI({
      #     HTML(interactive_table2)
      #   })
      # )

      # item 21
      # items$position21 <- div(
      #   style = 'visibility: hidden;',
      #   reactable::renderReactable({
      #     imputed <- "#ff9f1a"
      #     orange_pal <- function(x) ifelse(dat3$Z == 1, imputed, '#FFFFFF')

          # dat21 <- readr::read_csv('inst/extdata/fundemental_table4.csv') %>%
          #   mutate(Y1 = case_when(
          #     hyperShoe == 0 & first.race == 'yes' ~ 260,
          #     hyperShoe == 0 & first.race == 'no' ~ 245,
          #     TRUE ~ Y1
          #   ),
          #   Y0 = case_when(
          #     hyperShoe == 1 & first.race == 'yes' ~ 265,
          #     hyperShoe == 1 & first.race == 'no' ~ 250,
          #     TRUE ~ Y0
          #   ))
          #
          # reactable::reactable(dat21, defaultPageSize = 6, columns = list(
          #   Y0 = reactable::colDef(
          #     style = function(value, index) {
          #       if (dat21$hyperShoe[index] == 1 & dat21$first.race[index] == 'yes') {
          #         color <- "#4479E4"
          #       } else if(dat21$hyperShoe[index] == 1 & dat21$first.race[index] == 'no'){
          #         color <- "#9DB9F1"
          #       }else if (dat21$hyperShoe[index] == 0) {
          #         color <- 'white'
          #       }
          #       list(background = color)
          #     }),
          #   Y1 = reactable::colDef(
          #     style = function(value, index) {
          #       if (dat21$hyperShoe[index] == 0 & dat21$first.race[index] == 'yes') {
          #         color <- "#4479E4"
          #       } else if(dat21$hyperShoe[index] == 0 & dat21$first.race[index] == 'no'){
          #         color <- "#9DB9F1"
          #       }else if (dat21$hyperShoe[index] == 1) {
          #         color <- 'white'
          #       }
          #       list(background = color)})
          # ))
          #
          # }))



      return(items)

    })

  })
}

## To be copied in the UI
# mod_learn_fundemental_ui("mod_learn_fundemental_1")

## To be copied in the server
# mod_learn_fundemental_server("mod_learn_fundemental_1")
