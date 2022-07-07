#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bs4Dash::dashboardPage(
      fullscreen = TRUE,
      header = bs4Dash::dashboardHeader(
        title = tags$a(
          # href = 'https://www.eqt.com/',
          target = "_blank",
          tags$img(
            src = 'www/img/thinkCausal_logo_wide.png',
            width = "100%",
            style = "padding: 8% 12% 8% 12%",
            alt = 'thinkCausal'
          )
        )
      ),

      body = bs4Dash::dashboardBody(
        tags$div(class = 'corner-ribbon', 'BETA'),
        bs4Dash::tabItems(

          bs4Dash::tabItem(
            tabName = 'home',
            mod_home_ui("home_1")
          ),

          bs4Dash::tabItem(
            tabName = 'learn',
            mod_learn_ui("learn_1")
          ),
          bs4Dash::tabItem(
            tabName = 'estimands',
            mod_learn_ui("estimands_1")
          ),

          bs4Dash::tabItem(
            tabName = 'describe_data',
            mod_analysis_design_ui("analysis_design")
          ),
          bs4Dash::tabItem(
            tabName = 'upload_data',
            mod_analysis_upload_data_ui("analysis_upload_data")
          ),

          bs4Dash::tabItem(
            tabName = 'reproduce',
            mod_reproduce_ui("reproduce_1")
          ),

          bs4Dash::tabItem(
            tabName = 'settings_options',
            mod_settings_options_ui("settings_options")
          ),
          bs4Dash::tabItem(
            tabName = 'settings_about',
            mod_settings_about_ui("settings_about")
          ),
          bs4Dash::tabItem(
            tabName = 'settings_reference',
            mod_settings_reference_ui("settings_reference")
          )


        )
      ),

      sidebar = bs4Dash::dashboardSidebar(
        id = 'sideBarMenu',
        skin = 'dark',
        bs4Dash::sidebarMenu(

          bs4Dash::menuItem(
            text = 'thinkCausal',
            tabName = 'home',
            icon = icon("home")
          ),

          bs4Dash::menuItem(
            text = 'Learn',
            tabName = 'learn',
            icon = icon("book")
          ),

          bs4Dash::menuItem(
            text = 'Analyze',
            icon = icon("chart-line"),
            bs4Dash::menuSubItem(
              text = 'Describe data',
              tabName = 'describe_data',
              icon = icon("table")
            ),
            bs4Dash::menuSubItem(
              text = 'Upload data',
              tabName = 'upload_data',
              icon = icon("upload")
            ),
            bs4Dash::menuSubItem(
              text = 'Verify data types',
              tabName = 'verify_data',
              icon = icon("check")
            ),
            bs4Dash::menuSubItem(
              text = 'Visualize data',
              tabName = 'visualize_data',
              icon = icon('chart-line')
            ),
            bs4Dash::menuSubItem(
              text = 'Check balance',
              tabName = 'check_balance',
              icon = icon('chart-line')
            ),
            bs4Dash::menuSubItem(
              text = 'Check overlap',
              tabName = 'check_overlap',
              icon = icon('chart-line')
            ),
            bs4Dash::menuSubItem(
              text = 'Fit model',
              tabName = 'fit_model',
              icon = icon('code')
            ),
            bs4Dash::menuSubItem(
              text = 'Check diagnostics',
              tabName = 'check_diagnostics',
              icon = icon('stethoscope')
            ),
            bs4Dash::menuSubItem(
              text = 'View results',
              tabName = 'results',
              icon = icon('chart-area')
            ),
            bs4Dash::menuSubItem(
              text = 'View subgroup results',
              tabName = 'subgroup_results',
              icon = icon('code-branch')
            )
          ),

          bs4Dash::menuItem(
            text = 'Reproduce',
            tabName = 'reproduce',
            icon = shiny::icon('repeat', verify_fa = FALSE)
          ),

          bs4Dash::menuItem(
            text = 'Settings',
            tabName = 'settings',
            icon = icon('gear', verify_fa = FALSE),
            bs4Dash::menuSubItem(
              text = 'Options',
              tabName = 'settings_options',
              icon = icon('circle-thin', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'About',
              tabName = 'settings_about',
              icon = icon('circle-thin', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Reference',
              tabName = 'settings_reference',
              icon = icon('circle-thin', verify_fa = FALSE)
            )
          )
        )
      ),

      # # TODO: use this instead of shiny widget for data filtering?
      # controlbar = bs4Dash::dashboardControlbar(
      #   mod_data_ui('data_1'),
      #   id = 'data-slideover',
      #   width = 400
      # ),

      # TODO: this is a placeholder
      footer = bs4Dash::dashboardFooter(
        fixed = TRUE,
        left = HTML('<a href="https://steinhardt.nyu.edu/priism" target = "_blank">New York University ')
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "thinkCausal"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$script(src = 'www/ui.js')
  )
}