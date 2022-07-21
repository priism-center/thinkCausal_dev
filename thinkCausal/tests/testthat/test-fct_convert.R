
x1 <- data.frame(
  zero_one = sample(0:1, 10, replace = TRUE),
  integers = sample(0:10, 10, replace = TRUE),
  runif = runif(10),
  TF = sample(c("T", "F"), 10, replace = TRUE),
  char = sample(LETTERS, 10)
)
converted_x1 <- suppressWarnings(
  convert_data_types(
    x1,
    c('Binary', 'Categorical', 'Continuous', 'Binary', 'Continuous')
  )
)
test_that("convert_data_types() output is correct", {
  expect_s3_class(converted_x1, 'data.frame')
  expect_equal(as.vector(sapply(converted_x1, class)),
               c("logical", 'character', 'numeric', 'logical', 'numeric'))
})


# -------------------------------------------------------------------------


my_character = c('one', 'two', 'three')
my_logical = c(TRUE, FALSE, FALSE)
my_numeric = c(1.24, 7, -22)
X <- data.frame(my_character = my_character, my_logical = my_logical, my_numeric = my_numeric)
simple_data_types <- convert_data_type_to_simple(X)
test_that("convert_data_type_to_simple() output is correct", {
  expect_equal(simple_data_types,
               c("Categorical", 'Binary', 'Continuous'))
  expect_type(simple_data_types, 'character')
})

# clean_auto_convert_logicals(X)
# test_that("clean_auto_convert_logicals() output is correct", {
#   expect_equal(detected_y, list(Z = 'treatment', Y = 'rsp', X = c('dummy1', 'dummy2')))
#   expect_type(detected_y, 'list')
# })
