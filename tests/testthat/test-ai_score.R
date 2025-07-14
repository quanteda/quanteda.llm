make_mock_chat_fn <- function(return_scores = c("4", "2")) {
  i <- 0
  function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        i <<- i + 1
        list(score = return_scores[min(i, length(return_scores))], evidence = "Example evidence")
      }
    ), class = "MockChat")
  }
}

test_that("ai_score returns expected output for one document", {
  chat_fn <- make_mock_chat_fn(c("4"))
  result <- ai_score("Test document", prompt = "Rate from 1 to 5", chat_fn = chat_fn, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("score", "evidence") %in% colnames(result)))
  expect_type(result$score, "character")
  expect_equal(result$score, "4")
})

test_that("ai_score works with multiple documents", {
  chat_fn <- make_mock_chat_fn(c("2", "2"))
  docs <- c("Document 1", "Document 2")
  result <- ai_score(docs, prompt = "Rate from 1 to 5", chat_fn = chat_fn, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("score", "evidence") %in% colnames(result)))
  expect_equal(nrow(result), length(docs))
  expect_type(result$score, "character")
  expect_equal(result$score, c("2", "2"))
})
