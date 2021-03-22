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

y <- c("treatment", "rsp", "dummy1", "dummy2")
detected_y <- clean_detect_ZYX_columns(y)
test_that("clean_detect_ZYX_columns() output is correct", {
  expect_equal(detected_y, list(Z = 'treatment', Y = 'rsp', X = c('dummy1', 'dummy2')))
  expect_type(detected_y, 'list')
})

col_names <- c("yes", "TRUE", "nope%", "98", 'Ábcdêãçoàúü', 'yep_-,.yep', 'hello goodbye')
cleaned_names <- clean_names(.names)
test_that("clean_names() output is correct", {
  expect_equal(cleaned_names, c("yes", "TRUE",  "nope_percent", "n98", "Abcdeacoauu", "yep_.yep", "hello_goodbye"))
  expect_type(cleaned_names, 'character')
})
