# this defines the post-treatment learning module under Learning

require(shiny)
require(tidyr)
require(tibble)
require(ggplot2)


# objects -----------------------------------------------------------------

# list to store all post-treatment objects in
store_l_post_treatment <- list()

# set path
store_l_post_treatment$path_to_here <- file.path('modules', 'learning', 'post-treatment')

# load functions
purrr::map(
  list.files(file.path(store_l_post_treatment$path_to_here, 'R'), pattern = "*.R$", full.names = TRUE), 
  source
)

# read in randomization df
store_l_post_treatment$path_to_data <- file.path(store_l_post_treatment$path_to_here, 'data', 'plants_df.csv')
store_l_post_treatment$plants_df <- readr::read_csv(
  store_l_post_treatment$path_to_data,
  col_types = readr::cols(
    h1 = readr::col_double(),
    z = readr::col_logical(),
    bugs = readr::col_logical()
  )
)
store_l_post_treatment$plants_df$z_as_text <- ifelse(store_l_post_treatment$plants_df$z, 'Treatment', 'Control')


# UI ----------------------------------------------------------------------

ui_post_treatment <- function(id) {
  ns <- NS(id)
  tagList(
    wrap_with_quiz(
      ns = ns,
      quiz = create_quiz_post_treatment(ns = ns),
      includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_1.md')),
      br(),
      plotOutput(ns('posttreatment_plot'), height = 500),
      br(),
      includeMarkdown(file.path(store_l_post_treatment$path_to_here, "markdowns", 'post_treatment_2.md'))
    )
  )
}


# server ------------------------------------------------------------------

server_post_treatment <- function(id, plot_theme = ggplot2::theme_get) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # plot jitter
      output$posttreatment_plot <- renderPlot({
        
        # create plot
        p <- ggplot(data = store_l_post_treatment$plants_df, 
                    aes(x = jitter(as.numeric(z)), y = h1, col = z_as_text)) + 
          geom_point() + 
          scale_color_manual(values = c(4, 2)) +
          scale_x_continuous(labels = c('Control', 'Treatment'), breaks = c(0, 1)) +
          labs(title = 'TBD',
               subtitle = 'TBD',
               x = 'Treatment status',
               y = 'Plant height',
               color = NULL)
        
        # add theme
        p <- p + plot_theme()
        
        return(p)
      })
    }
  )
}


# clean up ----------------------------------------------------------------


