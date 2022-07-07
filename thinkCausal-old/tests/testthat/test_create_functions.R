
x <- data.frame(
  zero_one = c(0, 1, 0, 1, 1),
  TF = c("T", "T", "F", "T",  "F"),
  truefalse = c('true', 'false', 'false', 'false', 'true')
)
js_table <- create_datatable(x)
test_that("create_datatable() output is correct", {
  expect_s3_class(js_table, 'datatables')
})


# -------------------------------------------------------------------------

progress_bar <- create_progress_bar(50)
test_that("create_progress_bar() output is correct", {
  expect_s3_class(progress_bar, 'shiny.tag')
})


# -------------------------------------------------------------------------



# -------------------------------------------------------------------------


# reproducible_script <- create_script(
#  uploaded_file_name = 'test.csv',
#  uploaded_file_type = 'csv',
#  uploaded_file_header = 'TRUE',
#  uploaded_file_delim = ',',
#  selected_columns = c("Z", "Y", "X1", 'X2', "X3"),
#  column_names = c("treatment", "response", "covariate1", "covariate2", "covariate3"),
#  estimand = 'att',
#  common_support = 'none'
# )
# test_that("create_script() output is correct", {
#   expect_type(reproducible_script, 'character')
#   expect_equal(length(reproducible_script), 1)
# })

# TODO
# create_data_summary_grid
# create_drag_drop
# create_info_icon
# create_interactive_table
# create_interpretation
# create_progress_bar
# create_script