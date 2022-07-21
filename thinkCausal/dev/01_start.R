# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "thinkCausal",
  pkg_title = "Point-and-click bartCause analysis and causal inference education",
  pkg_description = "A federally funded project devoted to building scaffolded causal inference software implementing Bayesian Additive Regression Trees.",
  author_first_name = "Joseph",
  author_last_name = "Marlo",
  author_email = "jpm770@nyu.edu",
  repo_url = 'https://priism-center.github.io/thinkCausal/'
)

## Set {golem} options ----
golem::set_golem_options()


## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
golem::use_favicon(path = 'favicon.ico')
