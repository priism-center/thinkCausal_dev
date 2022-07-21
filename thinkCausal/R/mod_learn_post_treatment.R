#' learn_post_treatment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_post_treatment_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(
      class = 'learning-page',

      # the quiz UI
      mod_quiz_ui(id = ns('quiz')),

      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_1.md')),
        br(),
        radioButtons(inputId = ns('include_pt'),
                     label = h4('Control for post-treatment variable bugs?'),
                     choices = c('Do not control for bugs', 'Control for bugs'),
                     selected = 'Do not control for bugs',
                     inline = TRUE
        ),
        plotOutput(outputId = ns('posttreatment_plot'), height = 500),
        wellPanel(
          conditionalPanel(
            condition = "input.include_pt == 'Do not control for bugs'",
            ns = ns,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_2.md'))
          ),
          conditionalPanel(
            condition = "input.include_pt == 'Control for bugs'",
            ns = ns,
            includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_3.md'))
          )
        ),
        br(),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_4.md')),
        br(),
        plotOutput(ns('measured')),
        plotOutput(ns('observed')),
        br(), br(),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_5.md')),
        br(),
        wellPanel(includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_related.md'))),
        includeMarkdown(app_sys("app", "www", "learn", "post-treatment", "markdowns", 'post_treatment_citations.md'))
      )
    )

  )
}

#' learn_post_treatment Server Functions
#'
#' @noRd
mod_learn_post_treatment_server <- function(id, store){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # run the quiz
    mod_quiz_server(
      id = "quiz", # this should always be quiz
      id_parent = module_ids$learn$post_treatment,
      question_texts = quiz_content_post_treatment$question_texts,
      question_prompts = quiz_content_post_treatment$question_prompts,
      correct_answers = quiz_content_post_treatment$correct_answers,
      message_correct = quiz_content_post_treatment$message_correct,
      message_wrong = quiz_content_post_treatment$message_wrong,
      message_skipped = quiz_content_post_treatment$message_skipped,
      embed_quiz = FALSE
    )

    # TODO: would get better performance if we preprocess and store these dataframes
    plants_df <- reactive({
      plants_df <- readr::read_csv(
        app_sys('extdata', 'post_treatment_plants_df.csv', mustWork = TRUE),
        col_types = readr::cols(
          height = readr::col_double(),
          h1 = readr::col_double(),
          h0 = readr::col_double(),
          bugs = readr::col_logical(),
          bugs1 = readr::col_logical(),
          bugs0 = readr::col_logical(),
          pest_control = readr::col_logical()
        )
      )

      plants_df <- plants_df %>%
        mutate(pest_control_as_jitter = pest_control + runif(n(), -0.25, 0.25),
               pest_control_as_text = ifelse(pest_control, 'Pest control', 'No pest control'),
               bugs_as_text = ifelse(bugs, 'Has bugs', 'No bugs'))


      return(plants_df)
    })

    timeline_df <- reactive({
      timeline_df <- readr::read_csv(
        app_sys('extdata', 'post_treatment_timeline.csv', mustWork = TRUE)
      )

      # set factor order of status for colors
      timeline_df$status <- factor(
          timeline_df$status,
          levels = c('pre-treatment', 'post-treatment', 'outcome')
        )

      return(timeline_df)
    })

    output$posttreatment_plot <- renderPlot({

      # estimates
      y.z1 <- sum(lm(height~pest_control, data = plants_df())$coeff)
      y.z0 <- lm(height~pest_control, data = plants_df())$coeff[1]
      y.z1.b1 <- sum(lm(height~pest_control + bugs, data = plants_df())$coeff)
      y.z1.b0 <- sum(lm(height~pest_control + bugs, data = plants_df())$coeff[1:2])
      y.z0.b0 <- lm(height~pest_control + bugs, data = plants_df())$coeff[1]
      y.z0.b1 <- sum(lm(height~pest_control + bugs, data = plants_df())$coeff[-2])

      # plot based on user input
      if (input$include_pt == 'Control for bugs') {

        # create plot with post-treatment variable
        p <- ggplot(data = plants_df(),
                    aes(x = pest_control_as_jitter, y = height,
                        col = pest_control_as_text, shape = bugs_as_text)) +
          geom_point(alpha = 0.8) +
          geom_segment(x = -0.25,  xend = 0.25, y = y.z0.b0,  yend = y.z0.b0,
                       size = 1, col = 'black', linetype = 2) +
          geom_segment(x = -0.25, xend = 0.25, y = y.z0.b1, yend = y.z0.b1,
                       size = 1, col = 'black', linetype = 3) +
          geom_segment(x =  0.75, xend = 1.25, y = y.z1.b0, yend = y.z1.b0,
                       size = 1, col = 'black', linetype = 2) +
          geom_segment(x = 0.75, xend = 1.25, y = y.z1.b1, yend = y.z1.b1,
                       size = 1, col = 'black', linetype = 3) +
          labs(title = 'Controlling for bugs')

      } else {

        # create plot without post-treatment variable
        p <- ggplot(data = plants_df(),
                    aes(x = pest_control_as_jitter, y = height,
                        col = pest_control_as_text)) +
          geom_point(alpha = 0.8) +
          geom_segment(x = -0.25, xend = 0.25, y = y.z0, yend = y.z0,
                       size = 1, col = 'black') +
          geom_segment(x =  0.75, xend = 1.25, y = y.z1, yend = y.z1,
                       size = 1, col = 'black') +
          labs(title = 'Not controlling for bugs')
      }

      # add scales and labs
      p <- p +
        scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
        scale_color_manual(values = c(4, 2)) +
        scale_shape_manual(values = c(1, 19)) +
        labs(x = 'Treatment status',
             y = 'Plant height',
             color = NULL,
             shape = NULL)

      # add theme
      p <- p + store$options$theme_custom #plot_theme()

      return(p)
    })

    output$measured <- renderPlot({
      p <- ggplot(data = timeline_df(),
                  aes(x = measured, y = 0)) +
        geom_hline(yintercept = 0, color = "black") +
        geom_segment(aes(
          x = 0,
          xend = 0,
          y = .2,
          yend = -.2
        ), linetype = 'dashed') +
        geom_segment(aes(
          x = measured,
          xend = measured,
          y = 0,
          yend = .yend
        )) +
        geom_point(aes(y = 0, x = measured, col = status), size = 3) +
        coord_cartesian(xlim = c(-3, 9), ylim = c(-.2, .2)) +
        geom_text(aes(
          x = measured,
          y = (.yend + .yend * .12),
          label = variable
        ), size = 3.5) +
        geom_text(aes(x = 0, y = .21, label = "pre-school program begins")) +
        labs(title = 'Timeline of Variable Measurment', col = NULL) +
        theme_timeline() +
        scale_color_manual(values = c(4, 2, 1))

      return(p)

    })

    output$observed <- renderPlot({
      p <- ggplot(data = timeline_df(),
                  aes(x = occured, y = 0)) +
        geom_hline(yintercept = 0, color = "black") +
        geom_segment(aes(
          x = 0,
          xend = 0,
          y = .2,
          yend = -.2
        ), linetype = 'dashed') +
        geom_segment(aes(
          x = occured,
          xend = occured,
          y = 0,
          yend = .yend
        )) +
        geom_point(aes(y = 0, x = occured, col = status), size = 3) +
        coord_cartesian(xlim = c(-3, 9), ylim = c(-.2, .2)) +
        geom_text(aes(
          x = occured,
          y = (.yend + .yend * .12),
          label = variable
        ), size = 3.5) +
        geom_text(aes(x = 0, y = .21, label = "pre-school program begins")) +
        labs(title = 'Timeline of Variable Occurance', col = NULL) +
        theme_timeline() +
        scale_color_manual(values = c(4, 2, 1))

      return(p)
    })

    # data table
    treatment_switcher <- reactiveValues(value = FALSE)
    observeEvent(input$flip_treatment_button, {
      treatment_switcher$value <- ifelse(treatment_switcher$value, FALSE, TRUE)
    })
    output$po_table <- reactable::renderReactable({

      # set values based on switcher
      if (treatment_switcher$value){
        z <- c(0, 1, 0, 1)
        gym <- c(2, 6, 3, 7)
        strength <- c(135, 140, 150, 167)
      } else {
        z <- c(1, 0, 1, 0)
        gym <- c(4, 4, 5, 5)
        strength <- c(145, 130, 160, 157)
      }

      # create table
      po_table <- data.frame(
        Individual = c('Cong', 'Andi', 'Lindsey', 'Alex'),
        z = z,
        Baseline = c(150, 140, 200, 190),
        Gym = gym,
        Strength = strength
      ) %>%
        reactable::reactable()

      return(po_table)
    })

    #
    output$zoom_table <- reactable::renderReactable({
      tribble(
        ~Individual,~z, ~gym, ~`gym if z = 0`, ~`gym if z = 1`,
        'Cong',          1,     4,         "2",                "**4**",
        'Andi',          0,     4,         "**4**",                "6",
        'Lindsey',          1,     5,         "3",                "**5**",
        'Alex',          0,     5,         "**5**",                "7",
      ) %>%
        as.data.frame()  %>%
        reactable::reactable()
    })

  })
}

## To be copied in the UI
# mod_learn_post_treatment_ui("learn_post_treatment_1")

## To be copied in the server
# mod_learn_post_treatment_server("learn_post_treatment_1")
