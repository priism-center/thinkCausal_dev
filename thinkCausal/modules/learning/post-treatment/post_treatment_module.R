# this defines the post-treatment learning module under Learning

require(shiny)
require(tidyr)
require(tibble)
require(ggplot2)
set.seed(44)


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_post_treatment <- list()

# set path
store_l_post_treatment$path_to_here <- file.path('modules', 'learning', 'post-treatment')

# read in randomization df
store_l_post_treatment$plants_df <- readr::read_csv(
  file.path(store_l_post_treatment$path_to_here, 'data', 'plants_df.csv'),
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

# add vars for plotting
store_l_post_treatment$plants_df <- store_l_post_treatment$plants_df %>% 
  mutate(pest_control_as_jitter = pest_control + runif(n(), -0.25, 0.25),
         pest_control_as_text = ifelse(pest_control, 'Pest control', 'No pest control'),
         bugs_as_text = ifelse(bugs, 'Has bugs', 'No bugs'))


# namespace ---------------------------------------------------------------

# pay close attention to how the namespace is managed. Since the quiz module
# is inside the post-treatment module, its namespace is a 'child' of the post-treatment
# module. The post-treatment module namespace id is set within module_ids object in global.R.
# The quiz module id is always 'quiz' which then becomes 'learning_post_treatment-quiz'
# UI and server elements outside of the quiz can be treated as you would normal modules

# ns id is set as 'learning_post_treatment' per module_ids$learning$post_treatment

# set namespace for UI quiz elements
store_l_post_treatment$ns_quiz <- NS(NS(module_ids$learning$post_treatment)('quiz'))


# quiz structure ----------------------------------------------------------

source(file.path(store_l_post_treatment$path_to_here , 'R', 'post_treatment_quiz.R'))
store_l_post_treatment$question_texts <- list(question_1, question_2)
store_l_post_treatment$question_prompts <- list(question_prompt_1, question_prompt_2)
store_l_post_treatment$correct_answers <- list(correct_answer_1, correct_answer_2)
store_l_post_treatment$message_correct <- "Well done! You got all of them correct. Please read on below if you'd like to learn more about post-treatment variables."
store_l_post_treatment$message_wrong <- "Hmmm, bummer! You got at least one wrong. Please take a minute to learn more about post-treatment variables below."

# clean up otherwise may have conflicts with other learning modules
rm(question_1, question_2, question_prompt_1, question_prompt_2, correct_answer_1, correct_answer_2)


# UI ----------------------------------------------------------------------

ui_learning_post_treatment <- function(id) {
  ns <- NS(id)
  tagList(
    ui <- fluidPage(
      
      # the quiz UI
      ui_quiz(id = ns('quiz')),
      
      # UI content for the learning module
      div(
        class = ns('learning-content'), # required
        class = 'learning-content', # required
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_1.md')),
        DT::DTOutput(outputId = ns('data_preview')),
        br(), br(),
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_2.md')),
        br(),
        radioButtons(inputId = ns('include_pt'),
                     label = h4('Include post-treatment variable bugs?'),
                     choices = c('Include bugs', 'Do not include bugs'),
                     selected = 'Do not include bugs',
                     inline = TRUE),
        plotOutput(outputId = ns('posttreatment_plot'), 
                   height = 500),
        br(),
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_3.md')),
        DT::DTOutput(outputId = ns('data_bias')),
        br(), br(),
        includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_4.md'))
      )
    )
  )
}


# server ------------------------------------------------------------------

server_learning_post_treatment <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # run the quiz
      server_quiz(
        id = "quiz", # this should always be quiz
        id_parent = module_ids$learning$post_treatment,
        question_texts = store_l_post_treatment$question_texts,
        question_prompts = store_l_post_treatment$question_prompts,
        correct_answers = store_l_post_treatment$correct_answers,
        message_correct = store_l_post_treatment$message_correct,
        message_wrong = store_l_post_treatment$message_wrong,
        embed_quiz = FALSE
      )
      
      # data table preview
      output$data_preview <- DT::renderDataTable({
        store_l_post_treatment$plants_df %>% 
          mutate(bugs = as.numeric(bugs),
                 pest_control = as.numeric(pest_control)) %>% 
          select(height, bugs, `pest control` = pest_control) %>%
          create_datatable(info = FALSE, selection = "none", pageLength = 10)
      })
      
      # plot jitter
      output$posttreatment_plot <- renderPlot({
        
        # estimates
        y.z1 <- sum(lm(height~pest_control, data = store_l_post_treatment$plants_df)$coeff)
        y.z0 <- lm(height~pest_control, data = store_l_post_treatment$plants_df)$coeff[1]
        
        sum(lm(height~pest_control + bugs, data = store_l_post_treatment$plants_df)$coeff)
        y.z1.b1 <- sum(lm(height~pest_control + bugs, data = store_l_post_treatment$plants_df)$coeff)
        y.z1.b0 <- sum(lm(height~pest_control + bugs, data = store_l_post_treatment$plants_df)$coeff[1:2])
        y.z0.b0 <- lm(height~pest_control + bugs, data = store_l_post_treatment$plants_df)$coeff[1]
        y.z0.b1 <- sum(lm(height~pest_control + bugs, data = store_l_post_treatment$plants_df)$coeff[-2])

        # plot based on user input
        if (input$include_pt == 'Include bugs') {
          
          # create plot with post-treatment variable
          p <- ggplot(data = store_l_post_treatment$plants_df, 
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
            labs(title = 'Including bugs variable')
          
        } else {
          
          # create plot without post-treatment variable
          p <- ggplot(data = store_l_post_treatment$plants_df, 
                      aes(x = pest_control_as_jitter, y = height, 
                          col = pest_control_as_text)) +
            geom_point(alpha = 0.8) +
            geom_segment(x = -0.25, xend = 0.25, y = y.z0, yend = y.z0,
                         size = 1, col = 'black') +
            geom_segment(x =  0.75, xend = 1.25, y = y.z1, yend = y.z1,
                         size = 1, col = 'black') +
            labs(title = 'Not including bugs variable')
        }

        # add scales and labs
        p <- p +
          scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
          scale_color_manual(values = c(4, 2)) +
          scale_shape_manual(values = c(19, 1)) + 
          labs(x = 'Treatment status',
               y = 'Plant height',
               color = NULL,
               shape = NULL)
        
        # add theme
        p <- p + plot_theme()

        return(p)
      })
      
      # data table
      output$data_bias <- DT::renderDataTable({
        
        tribble(
          ~Individual, ~z, ~gym, ~`gym if z = 0`, ~`gym if z = 1`, ~strength, 
          "1", "0", "3", "**3**", "5", "\u22ee",
          "2", "1", "3", "1", "**3**","\u22ee", 
          "\u22ee","\u22ee","\u22ee","\u22ee","\u22ee","\u22ee",
        ) %>% 
          create_datatable(info = FALSE, selection = "none", bPaginate = FALSE)
      })
    }
  )
}


# clean up ----------------------------------------------------------------


