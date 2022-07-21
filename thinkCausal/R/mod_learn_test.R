#' learn_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_test_ui <- function(id){
  ns <- NS(id)
  tagList(

    scroll_ui_container(
      inputId = ns('scroll_container'),
      scroll_ui_text(
        inputId = ns('scroll_text'),
        scroll_ui_text_section(
          inputId = ns('text-1'),
          h2('Section 1'),
          p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Sit amet facilisis magna etiam tempor orci. Amet purus gravida quis blandit. Diam sit amet nisl suscipit adipiscing bibendum. Mauris rhoncus aenean vel elit scelerisque mauris pellentesque. In est ante in nibh. Neque viverra justo nec ultrices dui sapien eget mi proin. Ut enim blandit volutpat maecenas. Aliquam nulla facilisi cras fermentum odio eu feugiat pretium. Orci a scelerisque purus semper eget duis. Tristique sollicitudin nibh sit amet commodo nulla facilisi nullam vehicula. Turpis massa tincidunt dui ut ornare lectus sit. Eget dolor morbi non arcu risus quis varius. Et malesuada fames ac turpis egestas sed.')
        ),
        scroll_ui_text_section(
          inputId = ns('text-2'),
          h2('Section 2'),
          p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem. Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
        ),
        scroll_ui_text_section(
          inputId = ns('text-3'),
          h2('Section 3'),
          p('Libero id faucibus nisl tincidunt eget nullam non nisi. Tempus urna et pharetra pharetra. Eget sit amet tellus cras adipiscing. Ac tortor vitae purus faucibus. Eget gravida cum sociis natoque penatibus et magnis dis parturient. Vitae justo eget magna fermentum iaculis eu non diam phasellus. Nunc sed id semper risus. Neque laoreet suspendisse interdum consectetur libero id faucibus nisl. Non quam lacus suspendisse faucibus interdum posuere lorem ipsum dolor. Risus pretium quam vulputate dignissim suspendisse in est. Sagittis vitae et leo duis ut diam quam. Amet risus nullam eget felis eget nunc lobortis. In fermentum et sollicitudin ac orci phasellus egestas tellus. Sed faucibus turpis in eu mi bibendum neque egestas. Elit at imperdiet dui accumsan sit. Venenatis a condimentum vitae sapien pellentesque habitant morbi tristique senectus. Maecenas accumsan lacus vel facilisis. Pellentesque habitant morbi tristique senectus et netus et malesuada fames. Donec enim diam vulputate ut pharetra sit amet aliquam id.')
        )
      ),
      scroll_ui_visual(outputId = ns('scroll_visual'))
    )

  )
}

#' learn_test Server Functions
#'
#' @noRd
mod_learn_test_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$scroll_visual <- scroll_render(input, {
    output$scroll_visual <- renderUI({

      items <- list()

      # content
      items$p1 <- ggplot2::qplot(1:10) + ggplot2::labs(title = 'Section 1')
      items$p2 <- #tagList(
        ggplot2::qplot(1:10) + ggplot2::labs(title = 'Section 2')
        # data.frame(1:10)
      #)
      items$p3 <- ggplot2::qplot(1:10) + ggplot2::labs(title = 'Section 3')

      # content names to match inputIds
      ids <- glue::glue('text-{1:3}')
      names(items) <- glue::glue('scroll-text-{ids}')

      # chose item based on scroll position from JavaScript
      scroll_index <- input$scroll_index
      if (is.null(scroll_index)) scroll_index <- 1
      selected_item <- items[scroll_index]

      html <- renderPlot(selected_item)

      # TODO: should render all the plots but only {display: show} one of them

      return(html)
    })

  })
}
