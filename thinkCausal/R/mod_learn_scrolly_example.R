#' learn_scrolly_example UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_scrolly_example_ui <- function(id){
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

        h1('Title header'),
        p('Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor. Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem. Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec.'),
        p('Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem. Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor. Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus.'),
        br(),br(),br(),br()
      ),

      scroll_ui_container(
        ns = ns,
        scroll_ui_text(
          ns = ns,
          scroll_ui_text_section(
            ns = ns,
            position = 1,
            h2('Section 1'),
            p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Sit amet facilisis magna etiam tempor orci. Amet purus gravida quis blandit. Diam sit amet nisl suscipit adipiscing bibendum. Mauris rhoncus aenean vel elit scelerisque mauris pellentesque. In est ante in nibh. Neque viverra justo nec ultrices dui sapien eget mi proin. Ut enim blandit volutpat maecenas. Aliquam nulla facilisi cras fermentum odio eu feugiat pretium. Orci a scelerisque purus semper eget duis.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 2,
            h2('Section 2'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 3,
            h2('Section 3'),
            p('Libero id faucibus nisl tincidunt eget nullam non nisi. Tempus urna et pharetra pharetra. Eget sit amet tellus cras adipiscing. Ac tortor vitae purus faucibus.'),
            p('Eget gravida cum sociis natoque penatibus et magnis dis parturient. Vitae justo eget magna fermentum iaculis eu non diam phasellus. Nunc sed id semper risus. Neque laoreet suspendisse interdum consectetur libero id faucibus nisl. Non quam lacus suspendisse faucibus interdum posuere lorem ipsum dolor. Risus pretium quam vulputate dignissim suspendisse in est. Sagittis vitae et leo duis ut diam quam. Amet risus nullam eget felis eget nunc lobortis. In fermentum et sollicitudin ac orci phasellus egestas tellus. Sed faucibus turpis in eu mi bibendum neque egestas. Elit at imperdiet dui accumsan sit. Venenatis a condimentum vitae sapien pellentesque habitant morbi tristique senectus. Maecenas accumsan lacus vel facilisis.')
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
mod_learn_scrolly_example_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # add your list of content to show
    # items must be same length as the number of scroll_ui_text_section()
    # must be "output$scroll_visual"
    output$scroll_visual <- renderUI({

      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderCachedPlot(ggplot2::qplot(1:10) + ggplot2::labs(title = 'Section A'),
                                   cacheKeyExpr = { list(1) })
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(ggplot2::qplot(1:10, 1:10) + ggplot2::labs(title = 'Section B'),
                         cacheKeyExpr = { list(2) }),
        renderTable(data.frame(x = 1:4, y = 3:6, z = 5:8))
      )

      # item 3
      items$position3 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(ggplot2::qplot(rnorm(10), 1:10) + ggplot2::labs(title = 'Section C'),
                                   cacheKeyExpr = { list(3) })
      )

      return(items)
    })
  })
}
