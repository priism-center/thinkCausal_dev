#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(

    # add external resources
    golem_add_external_resources(),

    # message when server disconnects
    # TODO: this can be removed for native installation
    shinydisconnect::disconnectMessage(
      text = "Your session has disconnected. This usually happens due to inactivity.",
      refresh = "Refresh to start again",
      background = "#1F1F1F",
      colour = "#FFFFFF",
      refreshColour = "#E6C0F0",
      overlayColour = "#000000",
      overlayOpacity = 0.85,
      width = "full",
      top = "center",
      size = 20,
      css = "padding: 15px !important; box-shadow: none !important;"
    ),

    # main UI
    bs4Dash::dashboardPage(
      fullscreen = TRUE,
      dark = NULL,
      header = bs4Dash::dashboardHeader(
        title = tags$a(
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

        # add beta ribbon
        tags$div(
          class = 'cornerRibbon',
          'BETA',
          tags$div(
            tags$a(
              href = 'https://docs.google.com/forms/d/e/1FAIpQLSd7dZjpw4FtoVAFUmovNOgKeW-kxnJrs3zV2r3lJ8kvhdq8lA/viewform?usp=sf_link',
              target = "_blank",
              'Have feedback?'
            )
          )
        ),

        bs4Dash::tabItems(

          bs4Dash::tabItem(
            tabName = 'home',
            mod_home_ui(module_ids$home)
          ),

          # learning articles
          bs4Dash::tabItem(
            tabName = 'learn',
            mod_learn_ui(module_ids$learn$home)
          ),
          bs4Dash::tabItem(
            tabName = 'learn_estimands',
            mod_learn_estimands_ui(module_ids$learn$estimands)
          ),
          bs4Dash::tabItem(
            tabName = 'learn_randomization',
            mod_learn_rct_analysis_ui(module_ids$learn$randomization)
          ),
          bs4Dash::tabItem(
            tabName = 'learn_post_treatment',
            mod_learn_post_treatment_ui(module_ids$learn$post_treatment)
          ),
          bs4Dash::tabItem(
            tabName = 'learn_potential_outcomes',
            mod_learn_potential_outcomes_ui(module_ids$learn$potential_outcomes)
          ),
          # bs4Dash::tabItem(
          #   tabName = 'learn_scrolly',
          #   mod_learn_scrolly_example_ui('learn_scrolly')
          # ),

          # analysis pages
          bs4Dash::tabItem(
            tabName = 'analysis_describe',
            mod_analysis_design_ui(module_ids$analysis$design),
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_upload',
            mod_analysis_upload_ui(module_ids$analysis$upload)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_verify',
            mod_analysis_verify_ui(module_ids$analysis$verify)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_visualize',
            mod_analysis_visualize_ui(module_ids$analysis$visualize)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_balance',
            mod_analysis_balance_ui(module_ids$analysis$balance)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_overlap',
            mod_analysis_overlap_ui(module_ids$analysis$overlap)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_model',
            mod_analysis_model_ui(module_ids$analysis$model)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_diagnostics',
            mod_analysis_diagnostics_ui(module_ids$analysis$diagnostics)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_results',
            mod_analysis_results_ui(module_ids$analysis$results)
          ),
          bs4Dash::tabItem(
            tabName = 'analysis_subgroup',
            mod_analysis_subgroup_ui(module_ids$analysis$subgroup)
          ),


          bs4Dash::tabItem(
            tabName = 'reproduce',
            mod_reproduce_ui("reproduce")
          ),

          bs4Dash::tabItem(
            tabName = 'settings_options',
            mod_settings_options_ui(module_ids$settings$options)
          ),
          bs4Dash::tabItem(
            tabName = 'settings_about',
            mod_settings_about_ui(module_ids$settings$about)
          ),
          bs4Dash::tabItem(
            tabName = 'settings_reference',
            mod_settings_reference_ui(module_ids$settings$reference)
          )


        )
      ),

      sidebar = bs4Dash::dashboardSidebar(
        id = 'sideBarContainer',
        skin = 'dark',
        bs4Dash::sidebarMenu(
          id = 'sidebar',

          bs4Dash::menuItem(
            text = 'thinkCausal',
            tabName = 'home',
            icon = icon("home", verify_fa = FALSE)
          ),

          bs4Dash::menuItem(
            text = 'Learn',
            icon = icon("book", verify_fa = FALSE),
            bs4Dash::menuSubItem(
              text = 'All articles',
              tabName = 'learn',
              icon = icon("table", verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Causal estimands',
              tabName = 'learn_estimands'
            ),
            bs4Dash::menuSubItem(
              text = 'Post treatment variables',
              tabName = 'learn_post_treatment'
            ),
            bs4Dash::menuSubItem(
              text = 'Potential outcomes',
              tabName = 'learn_potential_outcomes'
            ),
            # bs4Dash::menuSubItem(
            #   text = 'Scrollytell example',
            #   tabName = 'learn_scrolly'
            # ),
            bs4Dash::menuSubItem(
              text = 'Randomization',
              tabName = 'learn_randomization'
            )
          ),

          # analysis pages
          bs4Dash::menuItem(
            text = 'Analyze',
            icon = icon("chart-line", verify_fa = FALSE),
            bs4Dash::menuSubItem(
              text = 'Describe data',
              tabName = 'analysis_describe',
              icon = icon("comment", verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Upload data',
              tabName = 'analysis_upload',
              icon = icon("upload", verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Verify data types',
              tabName = 'analysis_verify',
              icon = icon("check", verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Visualize data',
              tabName = 'analysis_visualize',
              icon = icon('chart-bar', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Check balance',
              tabName = 'analysis_balance',
              icon = icon('chart-bar', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Check overlap',
              tabName = 'analysis_overlap',
              icon = icon('chart-bar', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Fit model',
              tabName = 'analysis_model',
              icon = icon('code', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'Check diagnostics',
              tabName = 'analysis_diagnostics',
              icon = icon('stethoscope', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'View results',
              tabName = 'analysis_results',
              icon = icon('chart-area', verify_fa = FALSE)
            ),
            bs4Dash::menuSubItem(
              text = 'View subgroup results',
              tabName = 'analysis_subgroup',
              icon = icon('layer-group', verify_fa = FALSE)
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
              tabName = 'settings_options'
            ),
            bs4Dash::menuSubItem(
              text = 'About',
              tabName = 'settings_about'
            ),
            bs4Dash::menuSubItem(
              text = 'Reference',
              tabName = 'settings_reference'
            )
          )
        )
      ),

      # help menu
      controlbar = bs4Dash::dashboardControlbar(
        mod_help_ui('help'),
        id = 'help-slideover',
        width = 450
      ),

      footer = bs4Dash::dashboardFooter(
        fixed = TRUE,
        left = HTML('<a href="https://steinhardt.nyu.edu/priism" target = "_blank">New York University PRIISM</a>')
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

    # make all links open in a new tab
    # tags$base(target = "_blank"),

    # enable shinyjs
    shinyjs::useShinyjs(),

    # enable shinybrowser
    # TODO: this can be removed for native installation
    shinybrowser::detect(),

    # Add here other external resources
    tags$script(src = app_sys('app/www/ui.js')),

    # download roboto font
    tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic"),

    # load jquery UI, d3, and jstat
    tags$script(src = file.path('www', 'js', 'jquery-ui.js')),
    tags$script(src = file.path('www', 'js', 'd3.v5.js')),
    tags$script(src = file.path('www', 'js', 'jstat.min.js'))
  )
}