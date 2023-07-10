#' learn_randomization_dist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_randomization_dist_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = 'learning-page',
        div(
          #class = ns('learning-content'), # required
          #class = 'learning-content',  # required
          style = 'display: block;',
          fluidRow(
            column(width = 6,
              reactable::reactableOutput(outputId = ns('data'))
            ),
            column(width = 6,
              reactable::reactableOutput(outputId = ns('new_data'))
            )
          ),
          plotOutput(ns('plot')),

          br(),br(),br(),br()
        ))
  )
}

#' learn_randomization_dist Server Functions
#'
#' @noRd
mod_learn_randomization_dist_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # load true data
    dat_truth <- readr::read_csv('inst/extdata/truth.csv')
    dat_obs <- dat_truth
    dat_obs$sample <- 1
    dat_obs$Y0[dat_obs$hyperShoe == 1] <- NA
    dat_obs$Y1[dat_obs$hyperShoe == 0] <- NA

    est_ate <- function(df){
      fit <- lm(Y ~ hyperShoe*as.factor(`prior races`), data = df)
      cf_dat <- df
      cf_dat$hyperShoe <- abs(df$hyperShoe - 1)
      imputed <- predict(fit, newdata = cf_dat)
      Y1 <- ifelse(df$hyperShoe == 1, df$Y, imputed)
      Y0 <- ifelse(df$hyperShoe == 0, df$Y, imputed)
      mean(Y1 - Y0)

    }

    rand_dat <- reactiveValues(data = dat_obs,
                               est = est_ate(dat_obs)
    )

    new_sample <- reactive({
      rank = 0
      while (rank < 8) {
        new_dat <- dat_truth
        new_dat$hyperShoe <- rbinom(nrow(dat_truth), 1, .5)
        new_dat$Y <- ifelse(new_dat$hyperShoe == 1, dat_truth$Y1, dat_truth$Y0)
        new_dat$Y0[new_dat$hyperShoe == 1] <- NA
        new_dat$Y1[new_dat$hyperShoe == 0] <- NA
        fit <- lm(Y ~ hyperShoe*as.factor(`prior races`), data = df)
        rank <- fit$rank
      }

      cf_dat <- df
      cf_dat$hyperShoe <- abs(df$hyperShoe - 1)
      imputed <- predict(fit, newdata = cf_dat)
      Y1 <- ifelse(df$hyperShoe == 1, df$Y, imputed)
      Y0 <- ifelse(df$hyperShoe == 0, df$Y, imputed)
      estimate <- mean(Y1 - Y0)


      new_dat$sample <- max(rand_dat$data$sample) + 1
      rand_dat$data <- rbind(rand_dat$data, new_dat)
      rand_dat$est <- c(rand_dat$est, estimate)
      return(new_dat)
    })

    output$data <- reactable::renderReactable({
      reactable::reactable(data = dat_obs[, 1:6],
                           fullWidth = FALSE,
                           defaultPageSize = 20,
                           defaultColDef = reactable::colDef(minWidth = 75),
                           class = 'small',
                           theme = reactable::reactableTheme(cellPadding = "1px 6px")
                           )
    })

    output$new_data <- reactable::renderReactable({
      reactable::reactable(data = tail(rand_dat$data, 20),
                           defaultColDef = reactable::colDef(minWidth = 75),
                           fullWidth = FALSE,
                           defaultPageSize = 20,
                           class = 'small',
                           theme = reactable::reactableTheme(cellPadding = "1px 6px")
      )
    })






    output$plot <- renderPlot({
      dplyr::as_tibble(rand_dat$est) %>%
      ggplot2::ggplot(aes(value)) +
        ggplot2::geom_dotplot(binwidth = 10) +
        ggplot2::coord_cartesian(ylim = c(0, 1000))
    })


  })
}

## To be copied in the UI
# mod_learn_randomization_dist_ui("learn_randomization_dist_1")

## To be copied in the server
# mod_learn_randomization_dist_server("learn_randomization_dist_1")
