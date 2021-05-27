
sampsize <- 1000
tau <- 10
data <- dgp(sampsize, tau)

test_that("dgp() output is correct", {
  expect_s3_class(data, 'data.frame')
  expect_equal(length(data), 14)
  expect_lt( table(data$Z)["0"]/table(data$Z)["1"], 3/2)
  expect_gt( table(data$Z)["0"]/table(data$Z)["1"], 2/3)
})
