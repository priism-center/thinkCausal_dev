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
