x <- data.frame(
   zero_one = c(0, 1, 0, 1, 1),
   TF = c("T", "T", "F", "T",  "F"),
   truefalse = c('true', 'false', 'false', 'false', 'true')
 )
converted_x <- clean_auto_convert_logicals(x)
test_that("clean_auto_convert_logicals() output is correct", {
  expect_equal(as.list(table(as.matrix(converted_x))),
               list('FALSE' = 7, 'TRUE' = 8))
  expect_s3_class(converted_x, 'data.frame')
})

y <- data.frame(
  treatment = c(TRUE, TRUE, FALSE, TRUE, FALSE),
  rsp = c(0.808, 0.296,-1.579,-1.272, 0.627),
  ID = c(0.808, 0.296,-1.579,-1.272, 0.627),
  dummyID = 1:5,
  dummy1 = c(34, 35, 10, 5, 38)
)
detected_y <- clean_detect_ZYX_columns(y)
test_that("clean_detect_ZYX_columns() output is correct", {
  expect_equal(detected_y, list(Z = 'treatment', Y = 'rsp', X = c('dummyID', 'dummy1'), ID = 'ID'))
  expect_type(detected_y, 'list')
})

col_names <- c("yes", "TRUE", "nope%", "98", 'Ábcdêãçoàúü', 'yep_-,.yep', 'hello goodbye')
cleaned_names <- clean_names(col_names)
test_that("clean_names() output is correct", {
  expect_equal(cleaned_names, c("yes", "TRUE",  "nope_percent", "n98", "Abcdeacoauu", "yep_.yep", "hello_goodbye"))
  expect_type(cleaned_names, 'character')
})

X <- data.frame(X1 = 1:5, X2 = rnorm(5), X3 = LETTERS[1:5], X4 = as.factor(LETTERS[1:5]))
column_types <- clean_detect_column_types(X)
test_that("clean_detect_column_types() output is correct", {
  expect_equal(names(column_types), c('categorical', 'continuous'))
  expect_equal(column_types, list(categorical = c("X3", "X4"), continuous = c("X2", "X1")))
  expect_type(column_types, 'list')
})

X <- data.frame(
 treatment = sample(as.logical(0:1), 5, TRUE),
 response = rnorm(5),
 X1 = 1:5,
 X2 = rnorm(5),
 X3 = LETTERS[1:5],
 X4 = as.factor(LETTERS[1:5])
)
column_types <- clean_detect_column_types(X)
plot_vars <- clean_detect_plot_vars(column_types, 'treatment', 'response')
test_that("clean_detect_plot_vars() output is correct", {
  expect_equal(names(plot_vars), c('plot_type', 'X', 'Y', 'fill', 'shape', 'size', 'grouping', 'facet'))
  expect_type(plot_vars, 'list')
})


X <- tibble(
  test = 1:5,
  to_dummy = c('level1', 'level1', 'level2', 'level2', 'level3'),
  to_dummy2 = c('char1', 'char3', 'char3', 'char2', 'char1')
)
X <- fastDummies::dummy_cols(X)
is_dummy <- clean_detect_dummy_cols(X)
test_that("clean_detect_dummy_cols() output is correct", {
  expect_true(is_dummy$contains_dummy)
  expect_type(is_dummy, 'list')
})
