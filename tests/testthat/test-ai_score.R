test_that("ai_score returns expected output", {
  llm_output <- readRDS("../test_data/ai_score_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_57_59.rds")

  expect_s3_class(llm_output, "data.frame")
  expect_identical(
    c("id", "score", "evidence"),
    colnames(llm_output)
  )
  expect_type(llm_output$score, "character")
})

test_that("all score values are between 0 and 3", {
  llm_output <- readRDS("../test_data/ai_score_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_57_59.rds")
  expect_true(all(llm_output$score >= 0 & llm_output$score <= 3))
})
