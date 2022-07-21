
x1 <- data.frame(
  zero_one = sample(0:1, 10, replace = TRUE),
  integers = sample(0:10, 10, replace = TRUE),
  runif = runif(10),
  TF = sample(c("T", "F"), 10, replace = TRUE),
  char = sample(LETTERS, 10)
)
converted_x1 <- suppressWarnings(clean_auto_convert_integers(x1))
test_that("clean_auto_convert_integers() output is correct", {
  expect_s3_class(converted_x1$zero_one, "factor")
  expect_s3_class(converted_x1$integers, "factor")
  expect_type(converted_x1$runif, "double")
  expect_type(converted_x1$TF, "character")
  expect_type(converted_x1$char, "character")
  expect_s3_class(converted_x1, 'data.frame')
})


# -------------------------------------------------------------------------

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


# -------------------------------------------------------------------------

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


# -------------------------------------------------------------------------

col_names <- c("yes", "TRUE", "nope%", "98", 'Ábcdêãçoàúü', 'yep_-,.yep', 'hello goodbye')
cleaned_names <- clean_names(col_names)
test_that("clean_names() output is correct", {
  expect_equal(cleaned_names, c("yes", "TRUE",  "nope_percent", "n98", "Abcdeacoauu", "yep_.yep", "hello_goodbye"))
  expect_type(cleaned_names, 'character')
})


# -------------------------------------------------------------------------

X <- data.frame(X1 = 1:5, X2 = rnorm(5), X3 = LETTERS[1:5], X4 = as.factor(LETTERS[1:5]))
column_types <- clean_detect_column_types(X)
test_that("clean_detect_column_types() output is correct", {
  expect_equal(names(column_types), c('categorical', 'continuous'))
  expect_equal(column_types, list(categorical = c("X3", "X4"), continuous = c("X2", "X1")))
  expect_type(column_types, 'list')
})


# -------------------------------------------------------------------------

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


# -------------------------------------------------------------------------

# x6 <- tibble(
#   test = 1:5,
#   to_dummy = c('level1', 'level1', 'level2', 'level2', 'level3'),
#   to_dummy2 = c('char1', 'char3', 'char3', 'char2', 'char1')
# )
# converted_x6 <- fastDummies::dummy_cols(x6)
# dummies_detected <- clean_detect_dummy_cols(converted_x6)
# test_that("clean_detect_dummy_cols() output is correct", {
#   expect_type(dummies_detected, 'list')
#   expect_true(dummies_detected$contains_dummy)
#   expect_equal(dummies_detected$dummy_columns[[1]],
#                c("to_dummy_level1", "to_dummy_level2", "to_dummy_level3"))
#   expect_equal(dummies_detected$dummy_columns[[2]],
#                c("to_dummy2_char1", "to_dummy2_char2", "to_dummy2_char3"))
# })


# -------------------------------------------------------------------------

# x7 <- data.frame(
#   momwhite = c(1,0,0,0,0,0,1),
#   momblack = c(0,1,1,1,1,0,0),
#   momhisp = c(0,0,0,0,0,0,0),
#   y.obs = rnorm(7, 10, 3)
# )

# cleaned_df_internal <- clean_dummies_to_categorical_internal(i = 1, df = x7,
#                                                              group_names = c('momwhite', 'momblack', 'momhisp'),
#                                                              rename_group = 'race', problematic_group_names = c())
# test_that("clean_dummies_to_categorical_internal() output is correct", {
#   expect_type(cleaned_df_internal, 'list')
#   expect_true(is.null(cleaned_df_internal[[1]]))
#   expect_equal(names(cleaned_df_internal[[2]]), c('y.obs','race'))
#   expect_equal(cleaned_df_internal[[3]][[1]],
#                c("race", "momwhite", "momblack", "momhisp"))
# })
#
# race <- c("momwhite", "momblack", "momhisp")
# cleaned_df <- clean_dummies_to_categorical(x7, race)
# test_that("clean_dummies_to_categorical() output is correct", {
#   expect_equal(names(cleaned_df), c('y.obs','categorical_var'))
#   expect_s3_class(cleaned_df, 'data.frame')
# })

