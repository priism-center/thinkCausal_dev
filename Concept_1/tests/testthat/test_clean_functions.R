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
