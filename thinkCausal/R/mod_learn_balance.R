#' mod_learn_balance_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_learn_balance_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = 'learning-page',
      div(
        class = ns('learning-content'), # required
        class = 'learning-content',  # required
        style = 'display: block;',

        includeMarkdown(app_sys("app", "www", "learn", "balance", "markdowns", 'balance1.md')),
        br(),br(),
        div(
          tags$script(
            HTML(
              "
            $(document).ready(function() {setTimeout(function() {
              supElement = document.getElementById('mean').parentElement;
              $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
            }, 50);})
          "
            )
          ),

          tags$script(
            HTML(
              "
            $(document).ready(function() {setTimeout(function() {
              supElement = document.getElementById('var').parentElement;
              $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
            }, 50);})
          "
            )
          ),

        fluidRow(
          column(11, sliderInput('mean',
                                        label = 'Mean',
                                        min = 1, max = 2, value = 2, step = .1, ticks = F,
                                        width = '100%'), offset = 1)),
        fluidRow(
          column(1, h5('Imbalanced')),
          column(1, h5('Balanced'), offset = 10)
        ))
        ,

        fluidRow(
          column(11, sliderInput('var',
                                 label = 'Variance:',
                                 min = 1, max = 4, value = 4, step = .5, ticks = F,
                                 width = '100%'), offset = 1)),
        fluidRow(
          column(1, h5('Imbalanced')),
          column(1, h5('Balanced'), offset = 10)
        ),

        verbatimTextOutput('meanText'),
        p('Notice that with more imbalance the means become further and further away from one another.'),
        br(),
        p('Continuous covaraites can still be imabalance when the means are equal but the variance differs between the treatment and control conditions. Use the slider to see how the distributions change across levels of balance.'),
        verbatimTextOutput('varText'),
        p('Even though the treatment and control group have the same mean, they are imbalanced if they have different variance.'),
        fluidRow(
          sidebarPanel(
            selectInput(
              'variable',
              label = 'Variable:',
              choices = c('pre_test', 'major'),
              width = '70%'
            ),
            radioButtons(
              'design',
              label = 'Treatment assignment:',
              choices = c('Random', 'Non-random'),
              inline = FALSE
            ),
            radioButtons(
              'response',
              label = 'Response surface:',
              choices = c('Linear', 'Non-linear'),
              inline = FALSE
            ),
            sliderInput(
              'sample',
              label = 'Sample Size:',
              value = 200,
              min = 50,
              max = 2000,
              step = 50
            ),
            actionButton('resample',
                         'Resample')
          ),
          mainPanel(
            plotOutput('scatter', width = '100%'),
            plotOutput('histogram', width = '100%')
          )
        )
      )
    )
  )
}

#' learn_post_treatment Server Functions
#'
#' @noRd
#'

mod_learn_balance_server <- function(id, store){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # dat <- readr::read_rds('inst/extdata/balance_dat.rds')
    dgp <- reactive({
      #set.seed(21)
      input$resample
      if(input$design == 'Random') param <- 2 else param <- 1.3
      stat1_t <- rbeta(input$sample/2, 2, param)
      stat1_c <- rbeta(input$sample/2, param, 2)


      if(input$design == 'Random') prob_t <- .4 else prob_t <- .6
      if(input$design == 'Random') prob_c <- .4 else prob_c <- .2

      major_t <- rbinom(input$sample/2, 1, prob_t)
      major_c <- rbinom(input$sample/2, 1, prob_c)
      z <- c(rep('treatment', input$sample/2), rep('control', input$sample/2))

      dat <- data.frame(z,
                        major = c(major_t, major_c),
                        pre_test = c(stat1_t, stat1_c))

      if(input$response == 'Linear'){
        y1 <- 75 + dat$pre_test*23 + dat$major*1 + rnorm(input$sample/2, 0, 2)
        y0 <- 70 + dat$pre_test*23 + dat$major*3 + rnorm(input$sample/2, 0, 2)
        y <- ifelse(dat$z == 'treatment', y1, y0)
      }else{
        y0 <- 70 + dat$pre_test*23 + dat$major*3 + rnorm(input$sample/2, 0, 2)
        y1 <- 82 + exp((.07*(dat$pre_test*40))) + dat$major*1 + rnorm(input$sample/2, 0, 2)
        y <- ifelse(z == 'treatment', y1, y0)

      }

      dat <- cbind.data.frame(y1, y0, y, dat)
      dat$pre_test <- (dat$pre_test*200)/2
      return(dat)
    })

    mean_dat <- reactive({
      set.seed(2)
      treatment <- rbeta(15000,input$mean,2)*100
      control <- rbeta(15000,2, input$mean)*100
      mean_dat <- tibble::tibble(treatment, control) %>%
        tidyr::pivot_longer(1:2) %>%
        dplyr::group_by(name) %>%
        mutate(average = mean(value))

      return(mean_dat)
    })


    output$meanPlot <- renderPlot({
      mean_dat() %>%
        ggplot2::ggplot(aes(value, fill = name)) +
        ggplot2::geom_density(alpha = .6, adjust = 1.5) +
        ggplot2::geom_vline(aes(xintercept = average, col = name)) +
        ggplot2::scale_fill_manual(values = c(4, 2)) +
        ggplot2::scale_color_manual(values = c(4, 2)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title = element_blank(), legend.position = 'top')

    })

    output$meanText <- renderText({
      out <- mean_dat() %>%
        summarise(average = mean(average),
                  variance = sd(value))

      paste0('Mean of students in the control group = ', round(out[1, 2], 1),
             ' with standard deviation = ', round(out[1, 3], 1), '\n',
             'Mean of students in the treatment group = ', round(out[2, 2], 1),
             ' with standard deviation = ', round(out[2, 3], 1))


      })

    var_dat <- reactive({
      set.seed(2)
      treatment <- rbeta(15000,4,4)*100
      control <- rbeta(15000,input$var, input$var)*100
      tibble::tibble(treatment, control) %>%
        tidyr::pivot_longer(1:2) %>%
        dplyr::group_by(name) %>%
        mutate(average = mean(value))
    })

    output$varPlot <- renderPlot({
        var_dat() %>%
        ggplot2::ggplot(aes(value, fill = name)) +
        ggplot2::geom_density(alpha = .6, adjust = 1.5) +
        ggplot2::geom_vline(aes(xintercept = average, col = name)) +
        ggplot2::scale_fill_manual(values = c(4, 2)) +
        ggplot2::scale_color_manual(values = c(4, 2)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title = element_blank(), legend.position = 'top')

    })

    output$varText <- renderText({
      out <- var_dat() %>%
        summarise(average = mean(average),
                  variance = sd(value))

      paste0('Mean of students in the control group = ', round(out[1, 2], 1),
             ' with standard deviation = ', round(out[1, 3], 1), '\n',
             'Mean of students in the treatment group = ', round(out[2, 2], 1),
             ' with standard deviation = ', round(out[2, 3], 1))


    })

    output$scatter<- renderPlot({
        dgp() %>%
          ggplot2::ggplot(aes(pre_test, y, col = z)) +
          geom_point(size = 3) +
          geom_vline(aes(xintercept = dgp() %>% filter(z == 'treatment') %>% summarise(mean(pre_test)) %>% purrr::as_vector()), col = 2) +
          geom_vline(aes(xintercept = dgp() %>% filter(z != 'treatment') %>% summarise(mean(pre_test)) %>% purrr::as_vector()), col = 4) +
          scale_color_manual(values = c(4, 2)) +
          coord_cartesian(xlim = c(0, 100)) +
          theme(legend.position = 'top')
    })

    output$histogram <- renderPlot({
      dgp() %>%
        ggplot2::ggplot(aes(pre_test, fill = as.factor(z))) +
        geom_histogram(position = 'identity', bins = 10, alpha = .6, col = 'black') +
        geom_vline(aes(xintercept = dgp() %>% filter(z == 'treatment') %>% summarise(mean(pre_test)) %>% purrr::as_vector()), col = 2) +
        geom_vline(aes(xintercept = dgp() %>% filter(z != 'treatment') %>% summarise(mean(pre_test)) %>% purrr::as_vector()), col = 4) +
        scale_fill_manual(values = c(4, 2)) +
        coord_cartesian(xlim = c(0, 100)) +
        theme(legend.position = 'none')
    })


  })
}
