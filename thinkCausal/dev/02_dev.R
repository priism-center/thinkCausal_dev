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
usethis::use_package('ggplot2')
usethis::use_package('dplyr')
usethis::use_package('readr')
usethis::use_package('stringr')
usethis::use_package('shinyjs')

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "home", with_test = FALSE) # Name of the module
golem::add_css_file('home')
golem::add_module(name = "learn", with_test = FALSE)
golem::add_module(name = "reproduce", with_test = FALSE)
golem::add_module(name = "settings_options", with_test = FALSE)
golem::add_module(name = "settings_about", with_test = FALSE)
golem::add_module(name = "settings_reference", with_test = FALSE)
golem::add_module(name = "help", with_test = FALSE)

# analysis modules
golem::add_module(name = "analysis_design", with_test = TRUE)
golem::add_module(name = "analysis_upload", with_test = TRUE)
golem::add_css_file('analysis-upload')
golem::add_module(name = "analysis_verify", with_test = TRUE)
golem::add_css_file('analysis-verify')
golem::add_module(name = "analysis_visualize", with_test = TRUE)
golem::add_module(name = "analysis_balance", with_test = TRUE)
golem::add_module(name = "analysis_overlap", with_test = TRUE)
golem::add_module(name = "analysis_model", with_test = TRUE)
golem::add_module(name = "analysis_diagnostics", with_test = TRUE)
golem::add_module(name = "analysis_results", with_test = TRUE)
golem::add_module(name = "analysis_subgroup", with_test = TRUE)
golem::add_css_file('analysis-subgroup')

## learning modules
golem::add_css_file('learn')

# estimands
golem::add_module(name = "learn_estimands", with_test = FALSE)
golem::add_fct('learn_estimands', with_test = TRUE)
golem::add_css_file("learn-estimands", dir = 'inst/app/www/learn/estimands')
golem::add_js_file('init', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('buildPlot', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('buildTable', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('helpers', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('namespace', dir = 'inst/app/www/learn/estimands/js')
golem::add_js_file('scrollPlot', dir = 'inst/app/www/learn/estimands/js')

# post treatment variables
golem::add_module(name = "learn_post_treatment", with_test = FALSE)
golem::add_css_file("learn-post-treatment", dir = 'inst/app/www/learn/post-treatment')

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
golem::add_utils('utils', with_test = FALSE)
golem::add_fct('model', with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("ui")
# golem::add_js_handler("handlers")
golem::add_css_file("thinkCausal")
golem::add_css_file('info-icon')
golem::add_css_file('slider')
golem::add_css_file('corner-ribbon')
golem::add_css_file('sortable')
golem::add_css_file('reactable')
# golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")
