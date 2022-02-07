
question_1 <- "This is question 1"
question_2 <- "This is question 2"
question_texts <- list(question_1, question_2)
correct_answers <- list(c('1a'), list(c('2b', '2c')))
store <- list(
  state = 'quiz-question-1',
  states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
  question_texts = question_texts,
  question_prompts = NULL,
  correct_answers = correct_answers,
  responses = list(c('1a'), list(c('2c', '2b')), NA)
)

store2 <- list(
  correct_answers = c(correct_answers, c('3b')),
  responses = list(c('1a'), list(c('2c', '2b')), c('3a'), NA)
)


test_that("quiz functions output are correct", {
  expect_equal(quiz_get_state(store, 'current-question'), "This is question 1")
  expect_equal(quiz_get_state(store), "quiz-question-1")
  
  expect_equal(quiz_set_state(store, variable = 'current-state', value = 'quiz-question-2', state = NULL)$state,
               'quiz-question-2')
  
  expect_true(quiz_is_answer_correct(list(c('1b', '1a')), list(c('1a', '1b'))))
  expect_true(quiz_is_current_correct(store))
  expect_true(quiz_is_all_correct(store))
  expect_false(quiz_is_all_correct(store2))
})
