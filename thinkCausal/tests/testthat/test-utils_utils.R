
converted_logicals <- coerce_to_logical(c("T", "F", "t", "f", "TRUE", "FALSE", "1", "0", 1, 0))
test_that("converted_logicals() output is correct", {
  expect_equal(converted_logicals,
               c(T, F, T, F, T, F, T, F, T, F))
})


# -------------------------------------------------------------------------

# is_categorical


# -------------------------------------------------------------------------

# is_cat_or_logical



# -------------------------------------------------------------------------

# code_ <- extract_code('fit_bart')
# test_that("extract_code() output is correct", {
#   expect_type(code_, "character")
#   expect_true(any(grepl("bartCause::bartc", code_)))
#   expect_true(length(code_) > 10)
# })
