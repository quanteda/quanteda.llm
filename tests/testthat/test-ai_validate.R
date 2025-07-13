library(testthat)
library(quanteda.llm)  # Replace with your package name

test_that("ai_validate throws error for non-character input", {
  expect_error(
    ai_validate(123, llm_output = list("output"), launch_app = FALSE),
    "`text` must be a character vector\\."
  )
})

test_that("ai_validate returns expected structure with single text input", {
  result_env <- new.env()
  result <- ai_validate(
    text = "Example text.",
    llm_output = list("Example output."),
    result_env = result_env,
    verbose = FALSE,
    launch_app = FALSE  # Skip launching Shiny app during tests
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result, c("comments", "examples", "status"))
  expect_equal(result$comments, "N/A")
  expect_equal(result$examples, "")
  expect_equal(result$status, "Unmarked")
})

test_that("ai_validate handles multiple texts input correctly", {
  texts <- c("First text.", "Second text.")
  llm_out <- list("Output 1", "Output 2")
  result_env <- new.env()
  
  result <- ai_validate(
    text = texts,
    llm_output = llm_out,
    result_env = result_env,
    verbose = FALSE,
    launch_app = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(texts))
  expect_named(result, c("comments", "examples", "status"))
  expect_equal(result$comments, rep("N/A", length(texts)))
  expect_equal(result$examples, rep("", length(texts)))
  expect_equal(result$status, rep("Unmarked", length(texts)))
})
