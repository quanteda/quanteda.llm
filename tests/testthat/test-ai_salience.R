test_that("ai_salience returns expected output for one document", {
  llm_output <- readRDS("../test_data/ai_salience_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_1-3.rds")

  expect_s3_class(llm_output, "data.frame")
  expect_identical(
      c("id", "salience_economy", "salience_environment", "salience_healthcare"),
      colnames(llm_output)
  )
  expect_true(is.numeric(llm_output$salience_economy))
  expect_identical(
    llm_output$id,
    names(txt)
  )
})

test_that("all salience values are between 0 and 1", {
  llm_output <- readRDS("../test_data/ai_salience_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_1-3.rds")

  # Get all salience columns
  salience_cols <- grep("^salience_", names(llm_output), value = TRUE)

  # Check each column
  for (col in salience_cols) {
    expect_true(all(llm_output[[col]] >= 0 & llm_output[[col]] <= 1),
                info = paste("Values in", col, "should be between 0 and 1"))
  }
})
