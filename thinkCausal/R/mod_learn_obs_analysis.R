#' rct_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_obs_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

    # shinyjs::useShinyjs(),
    use_scrollytell(ns = ns),

    div(
      class = 'learning-page',

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',

        h1('Analyzing Data from Observation Studies'),
        includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis1.md')),
        br(),br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis2.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis3.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis4.md')),

          ),
          scroll_ui_text_section(
            ns = ns,
            position = 4,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis5.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 5,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis6.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 6,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis7.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 7,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis8.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 8,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis9.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 9,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis10.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 10,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis11.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 11,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis12.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 12,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis13.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 13,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis14.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 14,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis15.md')),
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 15,
            includeMarkdown(app_sys("app", "www", "learn", "observational-analysis", "markdowns", 'observational-analysis16.md')),
          )
        ),
        scroll_ui_visual(ns = ns)
      ),

      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',
        h2('Ending section'),
        p('Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem. Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor. Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.'),
        p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem. Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec.')
      )
    )
  )
}


#' learn_scrolly_example Server Functions
#'
#' @noRd
mod_learn_obs_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # set plot theme
    theme_set(theme_bw() + theme(panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 text = element_text(size = 16)
    ))

    # load all the objects needed to make every plot
    ## load df
    # dat1 <- readr::read_csv(
    #   app_sys('extdata', 'runners_obs1.csv', mustWork = TRUE),
    #   show_col_types = FALSE)
    # dat1$z <- as.factor(dat1$z)
    #
    # dat2 <- readr::read_csv(
    #   app_sys('extdata', 'runners_obs2.csv', mustWork = TRUE),
    #   show_col_types = FALSE)
    # dat2$z <- as.factor(dat2$z)


    p <- readr::read_rds(
      app_sys('extdata', 'obs_plots.rds', mustWork = TRUE)
      )


    #table 1
    comp1 <- data.frame(model = c('difference in means', 'regression with all confounders', 'BART with all confounders'),
                        estimate = c(-15.4, -5.0, -4.9),
                        lower.ci = c(-17.45981, -5.417691, -5.327),
                        upper.ci = c(-13.35439, -4.677999, -4.386))

    comp1$`interval length` <-  with(comp1, upper.ci - lower.ci)
    comp1$model <- factor(comp1$model, levels = comp1$model)

    # table 2
    comp2 <- data.frame(model = c('difference in means', 'regression with all confounders', 'BART with all confounders'),
                        estimate = c(-12.6, -6.6, -8.0),
                        lower.ci = c(-13.65272, -7.231296, -8.573),
                        upper.ci = c(-11.57668, -5.968704, -7.444))

    comp2$interval.length <- with(comp2, upper.ci - lower.ci)
    comp2$model <- factor(comp2$model, levels = comp2$model)


    output$scroll_visual <- renderUI({

      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderCachedPlot(
          p[[1]] +
            ggplot2::labs(title = 'Section A'),
          cacheKeyExpr = { list(1) })
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[2]] +
            ggplot2::labs(title = 'Section B'),
          cacheKeyExpr = { list(2) })
        #renderTable(data.frame(x = 1:4, y = 3:6, z = 5:8))
      )

      # item 3
      items$position3 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[3]]+
            ggplot2::labs(title = 'Section C'),
          cacheKeyExpr = { list(3) })
      )

      items$position4 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[4]] +
            ggplot2::labs(title = 'Section F'),
          cacheKeyExpr = { list(4) })
      )

      items$position5 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[5]],
          cacheKeyExpr = { list(5) })
      )

      items$position6 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[6]] +
            ggplot2::labs(title = 'Section H'),
          cacheKeyExpr = { list(6) })
      )

      items$position7 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[7]] +
            ggplot2::labs(title = 'Section I'),
          cacheKeyExpr = { list(7) })
      )

      items$position8 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
            p[[8]] +
            ggplot2::labs(title = 'Section K'),
          cacheKeyExpr = { list(8) })
      )

      items$position9 <- div(
        style = 'visibility: hidden;',

        renderCachedPlot(
          p[[9]],
          cacheKeyExpr = { list(9) })
        #renderTable(round(comp1, 1))

      )

      items$position10 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[10]],
          cacheKeyExpr = { list(10) })
      )

      items$position11 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p[[11]] +
            ggplot2::labs(title = 'Section N'),
          cacheKeyExpr = { list(11) })
      )

      items$position12 <- div(
        style = 'visibility: hidden;',

        renderCachedPlot(
          p[[12]] +
            ggplot2::labs(title = 'Section O'),
          cacheKeyExpr = { list(12) })
      )

      items$position13 <- div(
        style = 'visibility: hidden;',

        renderCachedPlot(
          p[[13]] +
            ggplot2::labs(title = 'Section P'),
          cacheKeyExpr = { list(13) })
      )

      items$position14 <- div(
        style = 'visibility: hidden;',

        renderCachedPlot(
          p[[14]] +
            ggplot2::labs(title = 'Section P'),
          cacheKeyExpr = { list(14) })
      )

      items$position15 <- div(
        style = 'visibility: hidden;',

        renderCachedPlot(
          p[[15]] +
            ggplot2::labs(y = element_blank(), x = 'Estimated ATE', title = 'Section P'),
          cacheKeyExpr = { list(15) })
      )
      return(items)
    })
  })
}

