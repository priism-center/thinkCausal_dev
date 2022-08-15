#' rct_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_rct_analysis_ui <- function(id){
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

        h1('Analyzing Randomized Experements'),
        includeMarkdown(app_sys("app", "www", "learn", "randomized-analysis", "markdowns", 'rct_analysis1.md')),
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
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 4,
            h2('Section 4'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 5,
            h2('Section 5'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 6,
            h2('Section 6'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 7,
            h2('Section 7'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 8,
            h2('Section 8'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 9,
            h2('Section 9'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 10,
            h2('Section 10'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 11,
            h2('Section 11'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 12,
            h2('Section 12'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 13,
            h2('Section 13'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 14,
            h2('Section 14'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 15,
            h2('Section 15'),
            p('Semper eget duis at tellus at urna condimentum mattis pellentesque. Purus sit amet luctus venenatis lectus. Enim ut tellus elementum sagittis. Duis at tellus at urna condimentum mattis. Lorem ipsum dolor sit amet. Sapien et ligula ullamcorper malesuada proin libero nunc consequat interdum. Iaculis nunc sed augue lacus viverra. Vehicula ipsum a arcu cursus. Vulputate ut pharetra sit amet. Velit egestas dui id ornare arcu odio ut sem.'),
            p('Ipsum dolor sit amet consectetur. Nec feugiat in fermentum posuere urna nec. Eu tincidunt tortor aliquam nulla facilisi cras fermentum odio. Faucibus a pellentesque sit amet porttitor.')
          ),
          scroll_ui_text_section(
            ns = ns,
            position = 16,
            h2('Section 16'),
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
mod_learn_rct_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # set plot theme
    theme_set(theme_bw() + theme(panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 text = element_text(size = 16)
                                 ))

    # load all the objects needed to make every plot
    ## load df
    runners_rct <- readr::read_csv(
      app_sys('extdata', 'runners_rct.csv', mustWork = TRUE),
      show_col_types = FALSE)


    ## values from analysis
    # point estimates of ATE
    diff_point_est <- -5.6
    reg_point_est <- -5.6
    bart_point_est <- -5.4

    # this is for regression plot
    reg_yend <- 174.0727
    reg_ystart <- 179.7049

    # for table
    sds <- c(.5, .4, .15)
    lcls <- c(-6.7, -6.5, -5.6)
    ucls <- c(-4.7, -4.8 ,-5.0)
    ci_len = ucls - lcls
    ate = with(runners_rct, mean(y1 - y0))
    model = c('difference in means', 'regression with covariates', 'bart with covariates')
    est = c(diff_point_est, reg_point_est, bart_point_est)
    order <- 1:3


    p1 <- ggplot2::ggplot(runners_rct, aes(age)) +
      ggplot2::geom_density(aes(fill = as.factor(z)), alpha = .5) +
      ggplot2::scale_fill_manual(values = c('blue', 'red')) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(title = 'Section G')

    p2 <- ggplot2::ggplot(runners_rct, aes(age, y, col = as.factor(z))) +
      ggplot2::scale_color_manual(values = c('blue', 'red')) +
      ggplot2::geom_point()

    p3 <- patchwork::plot_layout(p1/p2)
    # add your list of content to show
    # items must be same length as the number of scroll_ui_text_section()
    # must be "output$scroll_visual"
    output$scroll_visual <- renderUI({

      items <- list()

      # item 1
      items$position1 <- div(
        style = 'visibility: visible;',
        renderCachedPlot(
          runners_rct %>%
            dplyr::select(y, y.cf, dplyr::everything()) %>%
            dplyr::rename(
              `factual running time` = y,
              `counter factual running time` = y.cf
            ) %>%
            tidyr::pivot_longer(cols = c(1:2)) %>%
            dplyr::rename(y_name = name, y_value = value) %>%
            dplyr::mutate(z =
                            dplyr::if_else(
                              y_name == 'factual running time',
                              z,
                              abs(z - 1)
                            )) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(age, y_value, col = as.factor(z), shape = y_name)) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::scale_shape_manual(values = c(21, 19)) +
            ggplot2::geom_line(aes(age, true.1)) +
            ggplot2::geom_line(aes(age, true.0)) +
            ggplot2::labs(title = 'Section A'),
          cacheKeyExpr = { list(1) })
      )

      # item 2
      items$position2 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          runners_rct %>%
            dplyr::arrange(age) %>%
            dplyr::slice(c(10, 100, 180)) %>%
            dplyr::mutate(lb.y = (y + y.cf)/2,
                          lb = paste0('ICE = ', round(y1 - y0, 1))) %>%
            dplyr::select(y, y.cf, everything()) %>%
            dplyr::rename(`factual running time` = y,
                          `counter factual funning time` = y.cf) %>%
            tidyr::pivot_longer(cols = c(1:2)) %>%
            dplyr::rename(y_name = name, y_value = value) %>%
            dplyr::mutate(z = dplyr::if_else(y_name == 'factual running time', z, abs(z-1))) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(age, y_value, col = as.factor(z), shape = y_name)) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::scale_shape_manual(values = c(21, 19)) +
            ggplot2::coord_cartesian(xlim = c(18.00000, 51.88059), ylim = c(170.3687, 193.5359)) +
            ggplot2::geom_line(data = plot_dat, aes(x = age, y = true.1)) +
            ggplot2::geom_line(data = plot_dat, aes(x = age, y = true.0)) +
            ggplot2::geom_segment(aes(x = age, xend = age, y = y1, yend = y0))+
            ggplot2::geom_label(aes(x = age, y = lb.y, label = lb),  size = 4)+
            ggplot2::labs(title = 'Section B'),
          cacheKeyExpr = { list(2) })
        #renderTable(data.frame(x = 1:4, y = 3:6, z = 5:8))
      )

      # item 3
      items$position3 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          runners_rct %>%
            dplyr::select(y, y.cf, dplyr::everything()) %>%
            dplyr::rename(`factual running time` = y,
                          `counter factual funning time` = y.cf) %>%
            tidyr::pivot_longer(cols = c(1:2)) %>%
            dplyr::rename(y_name = name, y_value = value) %>%
            dplyr::mutate(z = dplyr::if_else(y_name == 'factual running time', z, abs(z-1))) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(age, y_value, col = as.factor(z), shape = y_name)) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::scale_shape_manual(values = c(21, 19)) +
            ggplot2::coord_cartesian(xlim = c(18.00000, 51.88059), ylim = c(170.3687, 193.5359)) +
            ggplot2::geom_line(data = runners_rct, aes(x = age, y = true.1)) +
            ggplot2::geom_line(data = runners_rct, aes(x = age, y = true.0)) +
            ggplot2::geom_segment(data = runners_rct, aes(x = age, xend = age, y = y, yend = y.cf)) +
            ggplot2::labs(title = 'Section C'),
          cacheKeyExpr = { list(3) })
      )

      items$position4 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          runners_rct %>%
            dplyr::mutate(tau = y1 - y0) %>%
            dplyr::arrange(tau) %>%
            dplyr::mutate(ord = dplyr::row_number(), y.cf = 0) %>%
            dplyr::select(tau, ord, z) %>%
            ggplot2::ggplot() +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_segment(aes(x = ord, xend = ord, y = 0, yend = tau)) +
            ggplot2::geom_hline(aes(yintercept = mean(tau))) +
            ggplot2::geom_label(aes(x = 125, y = mean(tau), label = paste0('True ATE =', round(mean(tau), 1))), size = 5) +
            ggplot2::labs(title = 'Section D'),
          cacheKeyExpr = { list(4) })
      )

      items$position5 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          ggplot2::ggplot(runners_rct, aes(age, y, col = as.factor(z))) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_point() +
            ggplot2::labs(title = 'Section F'),
          cacheKeyExpr = { list(5) })
      )

      items$position6 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          p3,
          cacheKeyExpr = { list(6) })
      )

      items$position7 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          runners_rct %>%
            dplyr::mutate(
              y0mean = mean(y[plot_dat$z == 0]),
              y1mean = mean(y[plot_dat$z == 1])) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = age, y = y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = mean_diff, col = as.factor(z)))+
            ggplot2::geom_segment(aes(x = 35, xend = 35, y = y0mean, yend = y1mean)) +
            ggplot2::geom_label(aes(x = 35, y = 177, label = paste('Estimated ATE =', diff_point_est)), size = 5) +
            ggplot2::labs(title = 'Section H'),
          cacheKeyExpr = { list(7) })
      )

      items$position8 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          ggplot2::ggplot(data = runners_rct) +
            ggplot2::geom_point(aes(x = age, y = y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = reg, col = as.factor(z))) +
            ggplot2::geom_segment(aes(x = 35, xend = 35, y = reg_ystart, yend = reg_yend)) +
            ggplot2::geom_label(aes(x = 35, y = 177, label = paste0('Estimated ATE =', reg_point_est)), size = 5) +
            ggplot2::labs(title = 'Section I'),
          cacheKeyExpr = { list(8) })
      )

      items$position9 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          ggplot2::ggplot(data = runners_rct) +
            ggplot2::geom_point(aes(age, y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = bart_mu1), col = 'red') +
            ggplot2::geom_line(aes(x = age, y = bart_mu0), col = 'blue')+
            ggplot2::labs(title = 'Section J'),
          cacheKeyExpr = { list(9) })
      )

      items$position10 <- div(
        style = 'visibility: hidden;',
        renderCachedPlot(
          ggplot2::ggplot(data = runners_rct) +
            ggplot2::geom_point(aes(age, y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = bart_mu1), col = 'red') +
            ggplot2::geom_line(aes(x = age, y = bart_mu0), col = 'blue') +
            ggplot2::geom_segment(aes(x = age, xend = age, y = bart_mu1, yend = bart_mu0)) +
            ggplot2::geom_label(aes(x = 35, y = 182, label = paste('Estimated ATE =', bart_point_est)), size = 5) +
            ggplot2::labs(title = 'Section K'),
          cacheKeyExpr = { list(10) })
      )

      items$position11 <- div(
        renderCachedPlot(
          tibble::tibble(model,est, lcls, ucls, order) %>%
            ggplot2::ggplot(aes(est, factor(model, levels = model))) +
            ggplot2::geom_point(size = 3) +
            ggplot2::geom_vline(aes(xintercept = ate), linetype = 'dashed') +
            ggplot2::geom_label(aes(x = ate, y = 3.3, label = paste('Ture ATE =', round(ate, 1)))) +
            ggplot2::coord_cartesian(xlim = c(-7, -3.5)) +
            ggplot2::labs(title = 'Section L'),
          cacheKeyExpr = { list(11) })
      )

      items$position12 <- div(
        renderCachedPlot(
          ggplot2::ggplot(data = runners_rct) +
            ggplot2::geom_point(aes(x = age, y = y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = mean_diff, col = as.factor(z))) +
            ggplot2::geom_segment(aes(x = age, xend = age, y = y, yend = mean_diff)) +
            ggplot2::labs(title = 'Section M'),
          cacheKeyExpr = { list(12) })
      )

      items$position13 <- div(
        renderCachedPlot(
          ggplot2::ggplot(data = runners_rct) +
            ggplot2::geom_point(aes(x = age, y = y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = reg, col = as.factor(z))) +
            ggplot2::geom_segment(aes(x = age, xend = age, y = y, yend = reg)) +
            ggplot2::labs(title = 'Section N'),
          cacheKeyExpr = { list(13) })
      )

      items$position14 <- div(
        renderCachedPlot(
          ggplot2::ggplot(data = runners_rct) +
            ggplot2::geom_point(aes(age, y, col = as.factor(z)), alpha = .3) +
            ggplot2::scale_color_manual(values = c('blue', 'red')) +
            ggplot2::geom_line(aes(x = age, y = bart, col = as.factor(z))) +
            ggplot2::geom_segment(aes(x = age, xend = age, y = y, yend = bart)) +
            ggplot2::labs(title = 'Section O'),
          cacheKeyExpr = { list(14) })
      )

      items$position15 <- div(
        renderCachedPlot(
          runners_rct %>%
            dplyr::mutate(
              `difference in means` = mean_diff - y ,
              `regression` = reg - y,
              `bart` = bart - y) %>%
            dplyr::select(`difference in means`,`regression`, `bart`) %>%
            tidyr::pivot_longer(cols = 1:3) %>%
            ggplot2::ggplot(aes(value)) +
            #geom_density()
            ggplot2::geom_density(aes(fill = name), alpha = .5) +
            ggplot2::labs(title = 'Section P'),
          cacheKeyExpr = { list(15) })
      )

      items$position16 <- div(
        renderCachedPlot(
          tibble::tibble(model,est, lcls, ucls, order) %>%
            ggplot2::ggplot(aes(est, factor(model, levels = model))) +
            ggplot2::geom_point(size = 3) +
            ggplot2::geom_errorbarh(aes(xmin = lcls, xmax = ucls), height = .1) +
            ggplot2::geom_vline(aes(xintercept = ate), linetype = 'dashed') +
            ggplot2::geom_label(aes(x = ate, y = 3.3, label = paste('Ture ATE =', round(ate, 1)))) +
            ggplot2::coord_cartesian(xlim = c(-7, -3.5)) +
            ggplot2::labs(y = element_blank(), x = 'Estimated ATE', title = 'Section P'),
          cacheKeyExpr = { list(16) }),
        renderTable(data.frame(model = model, estimate = est, `lower ci` = lcls, `upper ci` =  ucls, `interval length` = ci_len))
      )
    return(items)
    })
  })
}