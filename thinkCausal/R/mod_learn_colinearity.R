#' learn_colinearity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_colinearity_ui <- function(id){
  ns <- NS(id)
  tagList(
    use_scrollytell(ns = ns),
    div(class = 'learning-page',
        div(
          class = ns('learning-content'), # required
          class = 'learning-content',  # required
          style = 'display: block;',
          includeMarkdown(app_sys("app", "www", "learn", "colinearity", "markdowns", 'intro.md')),
          br(), br(), br(), br(),
        ),
        scroll_ui_container(
          ns = ns,
          scroll_ui_text(
            ns = ns,
            scroll_ui_text_section(
              ns = ns,
              position = 1,
              includeMarkdown(app_sys("app", "www", "learn", "colinearity", "markdowns", 'section1.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 2,
              includeMarkdown(app_sys("app", "www", "learn", "colinearity", "markdowns", 'section2.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 3,
              includeMarkdown(app_sys("app", "www", "learn", "colinearity", "markdowns", 'section3.md'))
            ),
            scroll_ui_text_section(
              ns = ns,
              position = 4,
              includeMarkdown(app_sys("app", "www", "learn", "colinearity", "markdowns", 'section3.md'))
            )
          ),
          scroll_ui_visual(ns = ns, clickable = T)
        )
    )
  )
}

#' learn_colinearity Server Functions
#'
#' @noRd
mod_learn_colinearity_server <- function(id, id_parent = 'learn_variable_selection'){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #ns <- NS(NS(id_parent)(id))

    dat <- readr::read_csv('inst/extdata/colinearity.csv')
    dat$ITE <- with(dat, Y1 - Y0)
    dat$runner <- 1:500
    dat <- dat %>% dplyr::select(runner, dplyr::everything())

    output$scroll_visual <- renderUI({
      items <- list()

      items$position1 <- div(
        style = 'visibility: visible;',
        reactable::renderReactable({
          reactable::reactable(data = dat %>%
                                 dplyr::mutate(Y0 = ifelse(hyperShoe == 1, NA, Y0),
                                               Y1 = ifelse(hyperShoe == 0, NA, Y1)
                                               ))
        })
      )

      items$position2 <- div(
        style = 'visibility: hidden;',
        renderImage(
          list(src = app_sys('app', 'www/learn/colinearity/plots/p1.png'),
               contentType = 'image/png',
               width = 800,
               height = 500)
          , deleteFile = F)
      )


    items$position3 <- div(
      style = 'visibility: hidden;',
      renderUI({
        shinyWidgets::radioGroupButtons(ns('view'), label = NULL,
                                        choices = c('Researcher', 'Parallel Universe', 'Oracle'),
                                        selected = 'Researcher',
                                        individual = T
        )
      }),
      renderUI({
         radioButtons(
          'show',
          label = NULL,
          choices = c('table', 'plot'),
          inline = TRUE
        )
      }),
      reactable::renderReactable(
        switch (input$view,
          'Researcher' =  create_table_researcher(df = dat[, names(dat)!='ITE'], imputed = F, rows = 10),
          'Parallel Universe' = create_table_parallel(df = dat[, names(dat)!='ITE'], rows = 10),
          'Oracle' = create_table_oracle(df = dat, rows = 10, imputed = FALSE)
        )

      )
    )

    items$position4 <- div(
      renderUI({
        checkboxGroupInput('analysis', label = 'Analyses Options')
      })
    )

    return(items)
  })



  })
}

## To be copied in the UI
# mod_learn_colinearity_ui("learn_colinearity_1")

## To be copied in the server
# mod_learn_colinearity_server("learn_colinearity_1")
