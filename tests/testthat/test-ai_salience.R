make_mock_chat_fn <- function(return_scores = c(0.95, 0.8)) {
  i <- 0
  function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        i <<- i + 1
        list(list(topic = "TopicA", score = return_scores[min(i, length(return_scores))]))
      }
    ), class = "MockChat")
  }
}

test_that("ai_salience returns expected output for one document", {
  chat_fn <- make_mock_chat_fn(c(0.95))
  result <- ai_salience("Test document", topics = c("TopicA"), chat_fn = chat_fn, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true("salience_TopicA" %in% colnames(result))
  expect_true(is.numeric(result$salience_TopicA))
  expect_equal(result$salience_TopicA, 0.95)
})

test_that("ai_salience works with multiple documents", {
  chat_fn <- make_mock_chat_fn(c("0.95", "0.8"))
  docs <- c("Document 1", "Document 2")
  result <- ai_salience(docs, topics = c("TopicA"), chat_fn = chat_fn, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "salience_TopicA") %in% colnames(result)))
  expect_equal(nrow(result), length(docs))
  expect_type(result$salience_TopicA, "double")
  expect_equal(result$salience_TopicA, c(0.95, 0.8))
})
