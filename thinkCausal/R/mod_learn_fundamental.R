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
            )
          ),
          scroll_ui_visual(ns = ns, clickable = T)
          ),

        div(
          class = ns('learning-content'), # required
          class = 'learning-content',  # required
          style = 'display: block;',
          includeMarkdown(app_sys("app", "www", "learn", "estimands2", "markdowns", 'intro.md')),
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

    shinyQuiz::quiz_server(quiz_content_fundamental)

    tab1 <- readr::read_csv('inst/extdata/fundamental_table1.csv')[1:6, ] %>%
      dplyr::mutate(Y0 = ifelse(hyperShoe == 1, '?', Y0),
                    Y1 = ifelse(hyperShoe == 0, '?', Y1))



    correct1 <- c(258, 256, 256, 258, 258, 256)
    interactive_table1 <- create_interactive_table(tab1, correct_answers = correct1)

    tab2 <- readr::read_csv('inst/extdata/fundemental_table4.csv')[1:6, ] %>%
      dplyr::mutate(Y0 = ifelse(hyperShoe == 1, '?', Y0),
                    Y1 = ifelse(hyperShoe == 0, '?', Y1))
    correct2 <- c(265, 245, 260, 265, 250, 245)
    interactive_table2 <- create_interactive_table(tab2, correct_answers = correct2)



    output$scroll_visual <- renderUI({
      items <- list()
      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        reactable::renderReactable({
          readr::read_csv('inst/extdata/fundamental_table1.csv') %>%
            reactable::reactable(defaultPageSize = 20)
        })
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/fundemental/plots/p1.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
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
