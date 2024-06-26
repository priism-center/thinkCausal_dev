library(bartCause)

# prep data
.data <- readr::read_csv(app_sys('extdata', 'lalonde.csv'))
.data <- dplyr::select(.data, 'treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr')
.data <- clean_auto_convert_logicals(.data)

# fit model
model_results <- fit_bart(
  .data = .data,
  ran_eff = NULL,
  .weights = NULL,
  .estimand = "ate",
  rct = T
)

model_validation <- validate_model_fit_(model_results)
test_that("bartCause::bartc() API still works", {
  expect_s3_class(model_results, 'bartcFit')
})
test_that('validate_model_fit_() works', {
  expect_null(model_validation)
})
