test_that("ai_summary returns simple structured summary", {
  chat_fn <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        list(summary = paste("Summary of:", text))
      }
    ), class = "MockChat")
  }
  
  result <- ai_summary("This is a test document.", chat_fn = chat_fn, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(result$summary, "Summary of: This is a test document.")
})

test_that("ai_summary handles multiple documents", {
  chat_fn <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        list(summary = paste("Summary of:", text))
      }
    ), class = "MockChat")
  }
  
  docs <- c(doc1 = "Document one.", doc2 = "Document two.")
  result <- ai_summary(docs, chat_fn = chat_fn, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$summary[1], "Summary of: Document one.")
  expect_equal(result$summary[2], "Summary of: Document two.")
})

test_that("ai_summary skips already processed documents", {
  chat_fn <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        list(summary = paste("Summary of:", text))
      }
    ), class = "MockChat")
  }
  
  docs <- c(doc1 = "Already processed.", doc2 = "To be processed.")
  result_env <- new.env()
  result_env$doc1 <- data.frame(summary = "Cached summary", stringsAsFactors = FALSE)
  
  result <- ai_summary(docs, chat_fn = chat_fn, result_env = result_env, verbose = FALSE)
  expect_equal(result$summary[1], "Cached summary")
  expect_equal(result$summary[2], "Summary of: To be processed.")
})
