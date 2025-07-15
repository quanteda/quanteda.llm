test_that("ai_text makes a simple structured request", {
  chat <- chat_test("Always return 0.8")
  result <- ai_text(
    .data = c(doc1 = "What is 1 + 1?"),
    chat_fn = chat_test,
    type_object = "classification"
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("id", "score"))
  expect_equal(result$score, 0.8)
})

test_that("ai_text supports few-shot examples", {
  chat <- chat_test("Return fixed score with few-shot examples")

  few_shot <- data.frame(
    text = c("Good text", "Bad text"),
    score = c(1, 0)
  )

  result <- ai_text(
    .data = c(doc1 = "Example text"),
    chat_fn = chat_test,
    type_object = "classification",
    few_shot_examples = few_shot
  )

  expect_equal(result$score, 0.8)
})

test_that("ai_text handles multiple documents", {
  chat <- chat_test("Always return 0.8")

  result <- ai_text(
    .data = c(doc1 = "Text A", doc2 = "Text B"),
    chat_fn = chat_test,
    type_object = "classification"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$score, c(0.8, 0.8))
})

# Error handling --------------------------------------------------------

test_that("ai_text skips already processed documents", {
  chat <- chat_test("Always return 0.8")
  env <- new.env()
  env[["doc1"]] <- data.frame(score = 1)

  result <- ai_text(
    .data = c(doc1 = "Old", doc2 = "New"),
    chat_fn = chat_test,
    type_object = "classification",
    result_env = env,
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$score, c(1, 0.8))
})

test_that("ai_text errors with non-character input", {
  expect_error(
    ai_text(.data = 123, chat_fn = chat_test(), type_object = "classification"),
    "Unsupported data type"
  )
})

# Interface consistency --------------------------------------------------

test_that("mock chat provider returns correct structure", {
  chat <- chat_test()
  expect_equal(chat$get_model(), "mock-model")
  resp <- chat$chat_structured("Test", type = "classification")
  expect_equal(resp, list(score = 0.8))
})
