# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
# attachment::att_amend_desc()
usethis::use_package('ggplot2', min_version = '3.3.2')
usethis::use_package('dplyr', min_version = '1.0.5')
usethis::use_package('tibble')
usethis::use_package('purrr')
usethis::use_package('tidyr', min_version = '1.1.3')
usethis::use_package('stringr')
usethis::use_package('glue')
usethis::use_package('rlang', min_version = '1.0.3')
usethis::use_package('scales', min_version = '1.1.1')
usethis::use_package('lubridate', min_version = '1.8.0')
usethis::use_package('cli')

usethis::use_package('bs4Dash', min_version = '2.1.0')
usethis::use_package('shinyjs', min_version = '2.1.0')
usethis::use_package('shinyWidgets', min_version = '0.7.4')
usethis::use_package('reactable', min_version = '0.3.0')
usethis::use_package('sortable', min_version = '0.4.5')
usethis::use_package('readr')
usethis::use_package('Hmisc', min_version = '4.7.0')
usethis::use_package('openxlsx', min_version = '4.2.5')
usethis::use_package('readstata13', min_version = '0.10.0')
usethis::use_package('patchwork', min_version = '1.1.1')
usethis::use_package('shinybrowser', min_version = '1.0.0')
usethis::use_package('shinydisconnect', min_version = '0.1.0')
usethis::use_package('htmltools')
usethis::use_package('jsonlite')
usethis::use_package('waiter')
usethis::use_dev_package('shinyFeedback', remote = 'merlinoa/shinyFeedback')

usethis::use_dev_package('plotBart', remote = 'github::priism-center/plotBart')
usethis::use_package('bartCause', min_version = '1.0.6')


## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "home", with_test = FALSE) # Name of the module
golem::add_css_file('home', dir = 'inst/app/www/css')
golem::add_module(name = "learn", with_test = FALSE)
golem::add_module(name = "reproduce", with_test = FALSE)
golem::add_fct("reproduce", with_test = TRUE)
golem::add_module(name = "settings_options", with_test = FALSE)
golem::add_module(name = "settings_about", with_test = FALSE)
golem::add_module(name = "settings_reference", with_test = FALSE)
golem::add_module(name = "help", with_test = FALSE)
golem::add_module(name = 'quiz', with_test = TRUE)
golem::add_css_file('confetti', dir = 'inst/app/www/css')

# scrollytell
golem::add_fct(name = 'scrollytell')
golem::add_css_file('scrollytell', dir = 'inst/app/www/css')
golem::add_js_file('scrollytell', dir = 'inst/app/www/js')

## analysis modules
golem::add_module(name = "analysis_design", with_test = TRUE)
golem::add_module(name = "analysis_upload", with_test = TRUE)
golem::add_css_file('analysis-upload', dir = 'inst/app/www/css')
golem::add_module(name = "analysis_verify", with_test = TRUE)
golem::add_css_file('analysis-verify', dir = 'inst/app/www/css')
golem::add_module(name = "analysis_visualize", with_test = TRUE)
golem::add_module(name = "analysis_balance", with_test = TRUE)
golem::add_module(name = "analysis_overlap", with_test = TRUE)
golem::add_module(name = "analysis_model", with_test = TRUE)
golem::add_module(name = "analysis_diagnostics", with_test = TRUE)
golem::add_module(name = "analysis_results", with_test = TRUE)
golem::add_module(name = "analysis_subgroup", with_test = TRUE)
golem::add_css_file('analysis-subgroup', dir = 'inst/app/www/css')

## learning modules
# NOTE: when creating new learning modules, its best to use the estimands or post treatment as a template
golem::add_css_file('learn', dir = 'inst/app/www/css')

# estimands
golem::add_module(name = "learn_estimands", with_test = FALSE)
golem::add_fct('learn_estimands', with_test = TRUE)
golem::add_css_file("learn-estimands", dir = 'inst/app/www/learn/estimands')
golem::add_js_file('1-namespace', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('2-helpers', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('3-scrollPlot', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('4-buildTable', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('5-buildPlot', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('6-init', dir = 'inst/app/www/learn/estimands/js')

# post treatment variables
golem::add_module(name = "learn_post_treatment", with_test = FALSE)
golem::add_fct('learn_post_treatment', with_test = FALSE)
golem::add_css_file("learn-post-treatment", dir = 'inst/app/www/learn/post-treatment')

# potential outcomes
golem::add_module(name = "learn_potential_outcomes", with_test = FALSE)
golem::add_css_file("learn-potential-outcomes", dir = 'inst/app/www/learn/potential-outcomes')

# scrollytell example
golem::add_module(name = 'learn_scrolly_example', with_test = FALSE)

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("ui", with_test = FALSE)
golem::add_fct('plot', with_test = FALSE)
golem::add_fct('validate', with_test = FALSE)
golem::add_fct('clean', with_test = TRUE)
golem::add_fct('detect', with_test = TRUE)
golem::add_fct('create', with_test = TRUE)
golem::add_fct('popup', with_test = FALSE)
golem::add_fct('convert', with_test = TRUE)
golem::add_utils('utils', with_test = TRUE)
golem::add_fct('model', with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("ui", dir = 'inst/app/www/js')
# golem::add_js_handler("handlers")
golem::add_css_file("thinkCausal", dir = 'inst/app/www/css')
golem::add_css_file('info-icon', dir = 'inst/app/www/css')
golem::add_css_file('slider', dir = 'inst/app/www/css')
golem::add_css_file('corner-ribbon', dir = 'inst/app/www/css')
golem::add_css_file('sortable', dir = 'inst/app/www/css')
golem::add_css_file('reactable', dir = 'inst/app/www/css')
# golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")
