test_that("ai_summary handles multiple documents", {
  llm_output <- readRDS("../test_data/ai_summary_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_1-3.rds")
  expect_s3_class(llm_output, "data.frame")
  expect_identical(llm_output$id, names(txt))
})

test_that("ai_summary skips already processed documents", {
  skip()
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
