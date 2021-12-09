
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
