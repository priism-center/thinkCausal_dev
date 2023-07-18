# # testServer(
# #   mod_quiz_server,
# #   # Add here your module params
# #   args = list()
# #   , {
# #     ns <- session$ns
# #     expect_true(
# #       inherits(ns, "function")
# #     )
# #     expect_true(
# #       grepl(id, ns(""))
# #     )
# #     expect_true(
# #       grepl("test", ns("test"))
# #     )
# #     # Here are some examples of tests you can
# #     # run on your module
# #     # - Testing the setting of inputs
# #     # session$setInputs(x = 1)
# #     # expect_true(input$x == 1)
# #     # - If ever your input updates a reactiveValues
# #     # - Note that this reactiveValues must be passed
# #     # - to the testServer function via args = list()
# #     # expect_true(r$x == 1)
# #     # - Testing output
# #     # expect_true(inherits(output$tbl$html, "html"))
# # })
# #
# # test_that("module ui works", {
# #   ui <- mod_quiz_ui(id = "test")
# #   golem::expect_shinytaglist(ui)
# #   # Check that formals have not been removed
# #   fmls <- formals(mod_quiz_ui)
# #   for (i in c("id")){
# #     expect_true(i %in% names(fmls))
# #   }
# # })
# #
#
#
# question_1 <- "This is question 1"
# question_2 <- "This is question 2"
# question_texts <- list(question_1, question_2)
# correct_answers <- list(c('1a'), list(c('2b', '2c')))
# store <- list(
#   state = 'quiz-question-1',
#   states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
#   question_texts = question_texts,
#   question_prompts = NULL,
#   correct_answers = correct_answers,
#   responses = list(c('1a'), list(c('2c', '2b')), NA),
#   skipped = FALSE
# )
#
# store2 <- list(
#   state = 'quiz-question-1',
#   states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
#   correct_answers = c(correct_answers, c('3b')),
#   responses = list(c('1a'), list(c('2c', '2b')), c('3a'), NA)
# )
#
# store2 <- quiz_set_state(store2, 'quiz-skipped', TRUE)
#
#
# test_that("quiz functions output are correct", {
#   expect_equal(quiz_get_state(store, 'current-question'), "This is question 1")
#   expect_equal(quiz_get_state(store), "quiz-question-1")
#
#   expect_equal(quiz_set_state(store, variable = 'current-state', value = 'quiz-question-2', state = NULL)$state,
#                'quiz-question-2')
#
#   expect_true(quiz_is_answer_correct(list(c('1b', '1a')), list(c('1a', '1b'))))
#   expect_true(quiz_is_current_correct(store))
#   expect_true(quiz_is_all_correct(store))
#   expect_true(quiz_is_all_correct(store2))
#
#   expect_false(quiz_get_state(store, 'quiz-skipped'))
#   expect_true(quiz_get_state(store2, 'quiz-skipped'))
# })
