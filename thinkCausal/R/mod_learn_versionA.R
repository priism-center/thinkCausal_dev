#' version A UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_versionA_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    use_scrollytell(ns = ns),
    div(class = 'learning-page',
        div(
          class = ns('learning-content'), # required
          class = 'learning-content',  # required
          style = 'display: block;',
          p('Filler Text'),
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
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 3,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 4,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 5,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 6,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 7,
              includeMarkdown(app_sys("app", "www", "learn", "fundemental", "markdowns", 'section1.md'))
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

#' version A Server Functions
#'
#' @noRd
mod_learn_versionA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

      items$position2 <- div(
        style = 'visibility: hidden;',
        renderImage({
          list(src = app_sys('app', 'www/learn/fundemental/plots/p3.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )

      imputedY0 <- "#2297E6"
      imputedY1 <- "#DF536B"

      dat18 <- readr::read_csv(app_sys('extdata/fundamental_table2.csv'))
      items$position3 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(dat18,
                               #theme = reactable::reactableTheme(cellPadding = "1px 6px"),
                               #class = 'small',
                               defaultPageSize = 10,
                               columns = list(
            Y0 = reactable::colDef(
              style = function(value, index) {
                if (dat18$hyperShoe[index] == 1) {
                  color <- imputedY0
                } else {
                  color <- 'white'
                }
                list(background = color)
              }),
            Y1 = reactable::colDef(
              style = function(value, index) {
                if (dat18$hyperShoe[index] == 0) {
                  color <- imputedY1
                } else {
                  color <- 'white'
                }
                list(background = color)
              })
          ))
        })

      )

      truth <- readr::read_csv(app_sys('extdata/truth.csv'))
      truth$ITE <- with(truth, Y1 - Y0)
      imputed <- readr::read_csv(app_sys('extdata/fundamental_table2.csv'))
      imputed$ITE <- with(imputed, Y1 - Y0)
      combined <- cbind(truth[, 1:3], imputed[, 4], truth[, 4], imputed[, 5], truth[, 5], truth[, 6])
      combined[combined$hyperShoe == 0, 4] <- NA
      combined[combined$hyperShoe == 1, 6] <- NA
      names(combined)[c(4, 6)] <- paste0('imputed', names(combined)[c(4, 6)])
      combined$est.ITE <- imputed$ITE
      combined$ITE <- truth$ITE

      items$position4 <- div(
        style = 'visibility: hidden;',
        reactable::renderReactable({
          reactable::reactable(imputed, fullWidth = F, columns = list(
            Y0 = reactable::colDef(
              style = function(value, index) {
                if (dat18$hyperShoe[index] == 1) {
                  color <- imputedY0
                } else {
                  color <- 'white'
                }
                list(background = color)
              }),
            Y1 = reactable::colDef(
              style = function(value, index) {
                if (dat18$hyperShoe[index] == 0) {
                  color <- imputedY1
                } else {
                  color <- 'white'
                }
                list(background = color)
              }),

            ITE = reactable::colDef(
              name = 'est.ITE'
            )
          ))
        })
      )




      dat <- reactive({
        if (input$view == 'Researcher' & input$imputed == FALSE) {
          dat <- dplyr::mutate(truth,
                        Y0 = ifelse(hyperShoe == 1, NA, Y0),
                        Y1 = ifelse(hyperShoe == 1, Y1, NA),
                        ) %>%
            dplyr::select(-ITE)
        }

        if (input$view == 'Researcher' & input$imputed == TRUE) {
          dat <- imputed
          names(dat)[names(dat) == 'ITE'] <- 'est.ITE'
        }

        if(input$view == 'Parallel Universe') {
          dat <- dplyr::mutate(truth,
                        hyperShoe = abs(1 - hyperShoe),
                        Y0 = ifelse(hyperShoe == 1, NA, Y0),
                        Y1 = ifelse(hyperShoe == 1, Y1, NA),
                        Y = ifelse(hyperShoe == 1, Y1, Y0))

          dat <- dat %>% dplyr::select(-ITE)
        }

        if(input$view == 'Oracle' & input$imputed == FALSE) {
          dat <- truth
        }

        if(input$view == 'Oracle' & input$imputed == TRUE) {
          dat <- combined
        }

        dat

      })

      background <- reactive({
        hyperShoe <- switch (input$view,
              'Researcher' = 'white',
              'Parallel Universe' = 'black',
              'Oracle' = 'white'
        )

        Y0 <- switch (input$view,
          'Researcher' = switch (as.character(input$imputed),
              'TRUE' =  ifelse(truth$hyperShoe == 1, imputedY0, 'white'),
              'FALSE' = 'white'),
          'Parallel Universe' = rep('black', nrow(truth)),
          'Oracle' = ifelse(dat()[['hyperShoe']] == 1, 'black', 'white')
        )

        Y1 <- switch (input$view,
          'Researcher' = switch (as.character(input$imputed),
                                 'TRUE' =  ifelse(truth$hyperShoe == 0, imputedY1, 'white'),
                                 'FALSE' = 'white'),
          'Parallel Universe' = rep('black', nrow(truth)),
          'Oracle' = ifelse(dat()[['hyperShoe']] == 0, 'black', 'white')
        )

        Y <- switch (input$view,
                      'Researcher' = 'white',
                      'Parallel Universe' = 'black',
                      'Oracle' = 'white'
        )


        list(hyperShoe = hyperShoe,
             Y0 = Y0,
             Y1 = Y1,
             Y = Y)
      })

      observeEvent(input$view, {
        if(input$view == 'Parallel Universe'){
          shinyjs::disable('imputed')
        }else{
          shinyjs::enable('imputed')
        }
      })


      items$position5 <- div(
        style = 'visibility: hidden;',
        renderUI({
          shinyWidgets::radioGroupButtons(ns('view'), label = 'select a view:',
                       choices = c('Researcher', 'Parallel Universe', 'Oracle'),
                       individual = T
                       )
        }),
        renderUI({
          shinyWidgets::materialSwitch(
            inputId = ns("imputed"),
            label = "Show imputed potential outcomes",
            status = "primary",
            right = TRUE

          )
        }),
        renderUI({
          if(input$view == 'Oracle' & input$imputed == TRUE){
            default <- c('hyperShoe', 'Y0', 'Y1', 'Y', 'est.ITE', 'ITE')
          }else{
            default <- names(dat())
          }
          default <-
          selectInput(inputId = ns('show'),
                      label = 'show',
                      choices = names(dat()),
                      selected = default,
                      multiple = TRUE,
                      width = '55%'
                      )
        }),
        reactable::renderReactable({
        if(input$imputed == TRUE & input$view == 'Oracle'){
         reactable::reactable(dat(),fullWidth = FALSE,
                              columns = list(
           runner = reactable::colDef(
             show = 'runner' %in% input$show
           ),
           `prior races` = reactable::colDef(
             show = 'prior races' %in% input$show
           ),
            hyperShoe = reactable::colDef(
              show = 'hyperShoe' %in% input$show,
              style = function(value, index) {
                list(background = background()[['hyperShoe']])
              }),
            imputedY0 = reactable::colDef(
              show = 'Y0' %in% input$show,
              name = 'imputed',
              style = function(value, index) {
                list(background = ifelse(truth$hyperShoe == 1, imputedY0, 'white')[index])
              }),
            Y0 = reactable::colDef(
              show = 'Y0' %in% input$show,
              name = 'true',
              headerStyle = list(backgoundColor = 'black'),
              style = function(value, index) {
                list(background = background()[['Y0']][index])
              }),
            imputedY1 = reactable::colDef(
              show = 'Y1' %in% input$show,
              name = 'imputed',
              style = function(value, index) {
                list(background = ifelse(truth$hyperShoe == 0, imputedY1, 'white')[index])
              }),
            Y1 = reactable::colDef(
              name = 'true',
              show = 'Y1' %in% input$show,
              #headerStyle = list(backgroundColor = 'green'),
              style = function(value, index) {
                list(background = background()[['Y1']][index])
              }),
            Y = reactable::colDef(
              show = 'Y' %in% input$show,
              style = function(value, index) {
                list(background = background()[['Y']])
              }),
           est.ITE = reactable::colDef(
             name = 'estimated'
            ),
           ITE = reactable::colDef(
             name = 'true',
             style = function(value, index) {
               list(background = 'black')
             })
          ),
          columnGroups = list(
            reactable::colGroup(name = "Y0", columns = c("imputedY0", "Y0")),
            reactable::colGroup(name = "Y1", columns = c("imputedY1", "Y1")),
            reactable::colGroup(name = 'ITE', columns = c('est.ITE', 'ITE'))
          )
          )

        }else if(input$imputed == FALSE & input$view == 'Oracle'){
          reactable::reactable(dat(),fullWidth = FALSE, columns = list(
            runner = reactable::colDef(
              show = 'runner' %in% input$show
            ),
            `prior races` = reactable::colDef(
              show = 'prior races' %in% input$show
            ),
            hyperShoe = reactable::colDef(
              show = 'hyperShoe' %in% input$show,
              style = function(value, index) {
                list(background = background()[['hyperShoe']])
              }),
            Y0 = reactable::colDef(
              show = 'Y0' %in% input$show,
              name = 'true',
              headerStyle = list(backgoundColor = 'black'),
              style = function(value, index) {
                list(background = background()[['Y0']][index])
              }),
            Y1 = reactable::colDef(
              name = 'true',
              show = 'Y1' %in% input$show,
              #headerStyle = list(backgroundColor = 'green'),
              style = function(value, index) {
                list(background = background()[['Y1']][index])
              }),
            Y = reactable::colDef(
              show = 'Y' %in% input$show,
              style = function(value, index) {
                list(background = background()[['Y']])
              }),
            ITE = reactable::colDef(
              show = 'ITE' %in% input$show,
              style = function(value, index) {
                list(background = 'black')
              })
          )
          )
        }else{
            reactable::reactable(dat(),fullWidth = FALSE, columns = list(
              runner = reactable::colDef(
                show = 'runner' %in% input$show
              ),
              `prior races` = reactable::colDef(
                show = 'prior races' %in% input$show
                ),
              hyperShoe = reactable::colDef(
                show = 'hyperShoe' %in% input$show,
                style = function(value, index) {
                  list(background = background()[['hyperShoe']])
                }),
              Y0 = reactable::colDef(
                show = 'Y0' %in% input$show,
                headerStyle = list(backgoundColor = 'black'),
                style = function(value, index) {
                  list(background = background()[['Y0']][index])
                }),
              Y1 = reactable::colDef(
                show = 'Y1' %in% input$show,
                #headerStyle = list(backgroundColor = 'green'),
                style = function(value, index) {
                  list(background = background()[['Y1']][index])
                }),
              Y = reactable::colDef(
                show = 'Y' %in% input$show,
                style = function(value, index) {
                  list(background = background()[['Y']])
                })
            ))
        }

        })
      )

      items$position6 <- div(
        style = 'visibility: hidden;',
        renderUI({
          shinyWidgets::radioGroupButtons(ns('view6'), label = 'select a view:',
                                          choices = c('Researcher', 'Oracle'),
                                          individual = T,
                                          selected = 'Oracle'
          )
        }),
        renderImage({
          if(input$view6 == 'Oracle') path <- 'www/learn/fundemental/plots/cf1.png'
          if(input$view6 != 'Oracle') path <- 'www/learn/fundemental/plots/factual1.png'
          list(src = app_sys('app', path),
               contentType = 'image/png',
               width = 800,
               height = 500)
        }, deleteFile = F)
      )


      items$position7 <- div(
        style = 'visibility: hidden;',
        renderUI({
          shinyWidgets::radioGroupButtons(ns('view7'), label = NULL,
                                          choices = c('Researcher', 'Oracle'),
                                          individual = T,
                                          selected = 'Oracle'
          )
        }),
        renderUI({
          selectInput(ns('viz7'), label = NULL,
                      choices = c('Plot', 'Table')
                      )
        }),
        renderUI({
        if(input$viz7 == 'Plot'){
          renderImage({
            if(input$view7 == 'Oracle') path <- 'www/learn/fundemental/plots/cf2.png'
            if(input$view7 != 'Oracle') path <- 'www/learn/fundemental/plots/factual2.png'
            list(src = app_sys('app', path),
                 contentType = 'image/png',
                 width = 800,
                 height = 500)
          }, deleteFile = F)
        }else{
          reactable::renderReactable({
            reactable::reactable(combined[, c('runner', 'est.ITE', 'ITE')],
                                 defaultPageSize = 20,
                                 class = 'small',
                                 theme = reactable::reactableTheme(
                                   cellPadding = "1px 6px"
                                 ),
                                 columns = list(
                                   runner = reactable::colDef(footer = 'Average'),
                                   est.ITE = reactable::colDef(footer = 12.14),
                                   ITE = reactable::colDef(
                                     show = input$view7 == 'Oracle',
                                     footer = 12.75,
                                                           style = list(background = 'black',
                                                                        color = 'white'),
                                                           footerStyle = list(background = 'black',
                                                                              color = 'white')
                                                           )
                                 ),
                                 defaultColDef = reactable::colDef(
                                   footerStyle = list(fontWeight = "bold"),
                                   )
                                 )

          })
        }
        })

        )

      return(items)
    })

  })
}

## To be copied in the UI
# mod_version A_ui("version A_1")

## To be copied in the server
# mod_version A_server("version A_1")
