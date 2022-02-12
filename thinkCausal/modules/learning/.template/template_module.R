# this defines the template for a learning module
# `create_learning_module` function creates new modules based on this template
# any text "template" is replaced with the new module short_name

require(shiny)
# require(tidyr)
# require(tibble)
# require(ggplot2)


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_template <- list()

# set path
store_l_template$path_to_here <- file.path('modules', 'learning', '.template')

# read in randomization df
store_l_template$template_df <- readr::read_csv(
  file.path(store_l_template$path_to_here, 'data', 'template_df.csv'),
  col_types = readr::cols(
    h1 = readr::col_double(),
    z = readr::col_logical(),
    bugs = readr::col_logical()
  )
)
store_l_template$template_df$z_as_text <- ifelse(store_l_template$template_df$z, 'Treatment', 'Control')


# namespace ---------------------------------------------------------------

# pay close attention to how the namespace is managed. Since the quiz module
# is inside the post-treatment module, its namespace is a 'child' of the post-treatment
# module. The post-treatment module namespace id is set within module_ids object in global.R.
# The quiz module id is always 'quiz' which then becomes 'learning_post_treatment-quiz'
# UI and server elements outside of the quiz can be treated as you would normal modules

# ns id is set as 'learning_post_treatment' per module_ids$learning$post_treatment

# set namespace for UI quiz elements
store_l_template$ns_quiz <- NS(NS(module_ids$learning$template)('quiz'))


# source quiz and R functions ---------------------------------------------

purrr::walk(list.files(file.path(store_l_template$path_to_here, 'R'), full.names = TRUE), source)


# UI ----------------------------------------------------------------------

ui_learning_template <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        includeMarkdown(file.path(store_l_template$path_to_here, "markdowns", 'template_1.md')),
        br(),
        radioButtons(inputId = ns('include_pt'),
                     label = 'Include post-treatment variable bugs?',
                     choices = c('Include bugs', 'Do not include bugs'),
                     selected = 'Do not include bugs'),
        plotOutput(outputId = ns('posttreatment_plot'), 
                   height = 500),
        br()
      ),
      
      # the quiz UI
      ui_quiz(id = ns('quiz')),
      
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        class = ns('learning-content-blur'), # required for blur
        class = 'learning-content-blur', # required for blur
        br(),
        includeMarkdown(file.path(store_l_template$path_to_here, "markdowns", 'template_2.md'))
      )
    )
  )
}


# server ------------------------------------------------------------------

server_learning_template <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz", # this should always be quiz
        id_parent = module_ids$learning$template,
        question_texts = store_l_template$quiz()$question_texts,
        question_prompts = store_l_template$quiz()$question_prompts,
        correct_answers = store_l_template$quiz()$correct_answers,
        message_correct = store_l_template$quiz()$message_correct,
        message_wrong = store_l_template$quiz()$message_wrong,
        embed_quiz = TRUE
      )
      
      # example plot
      output$posttreatment_plot <- renderPlot({

        if (input$include_pt == 'Include bugs') {
          
          # create plot with post-treatment variable
          p <- store_l_template$template_df %>% 
            group_by(bugs, z) %>% 
            mutate(mean = mean(h1)) %>% 
            ggplot(aes(x = jitter(as.numeric(z)), y = h1, 
                       group = bugs, shape = bugs, col = z_as_text)) +
            geom_point() +
            # TODO: fix this
            geom_hline(aes(yintercept = mean)) +
            scale_color_manual(values = c(4, 2)) +
            scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
            scale_shape_manual(values = c(19, 1)) + 
            labs(title = 'TBD',
                 subtitle = 'TBD',
                 x = 'Treatment status',
                 y = 'Plant height',
                 color = NULL)
          
        } else {
          
          # create plot without post-treatment variable
          p <- ggplot(data = store_l_template$template_df,
                      aes(x = jitter(as.numeric(z)), y = h1, col = z_as_text)) +
            geom_point() +
            scale_color_manual(values = c(4, 2)) +
            scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
            labs(title = 'TBD',
                 subtitle = 'TBD',
                 x = 'Treatment status',
                 y = 'Plant height',
                 color = NULL)
        }

        # add theme
        p <- p + plot_theme()

        return(p)
      })
    }
  )
}


# clean up ----------------------------------------------------------------


