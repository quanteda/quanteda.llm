test_that("ai_validate throws error for non-character input", {
  expect_error(
    ai_validate(123, llm_output = list("output"), launch_app = FALSE),
    "`text` must be a character vector\\."
  )
})

test_that("ai_validate returns expected structure with single text input", {
  llm_output <- readRDS("../test_data/ai_summary_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_1-3.rds")
  txt <- txt[1]
  llm_output <- llm_output[1, ]
  result_env <- new.env()
  result <- ai_validate(
    text = txt,
    llm_output = llm_output,
    result_env = result_env,
    verbose = FALSE,
    launch_app = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result, c("comments", "examples", "status"))
  expect_equal(result$comments, rep("N/A", 1))
  expect_equal(result$examples, rep("", 1))
  expect_equal(result$status, rep("Unmarked", 1))
})

test_that("ai_validate handles multiple texts input correctly - summary", {
  llm_output <- readRDS("../test_data/ai_summary_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_1-3.rds")
  result_env <- new.env()

  result <- ai_validate(
    text = txt,
    llm_output = llm_output,
    result_env = result_env,
    verbose = FALSE,
    launch_app = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(txt))
  expect_named(result, c("comments", "examples", "status"))
  expect_equal(result$comments, rep("N/A", length(txt)))
  expect_equal(result$examples, rep("", length(txt)))
  expect_equal(result$status, rep("Unmarked", length(txt)))
})

test_that("ai_validate handles multiple texts input correctly - salience", {
  llm_output <- readRDS("../test_data/ai_salience_example.rds")
  txt <- readRDS("../test_data/data_corpus_inaugural_1-3.rds")
  result_env <- new.env()

  result <- ai_validate(
    text = txt,
    llm_output = llm_output,
    result_env = result_env,
    verbose = FALSE,
    launch_app = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(txt))
  expect_named(result, c("comments", "examples", "status"))
  expect_equal(result$comments, rep("N/A", length(txt)))
  expect_equal(result$examples, rep("", length(txt)))
  expect_equal(result$status, rep("Unmarked", length(txt)))
})

# Test for ai_validate without launching app
test_that("ai_validate handles missing llm_output id column", {
  expect_error(
    ai_validate(
      text = c("Text 1", "Text 2"),
      llm_output = data.frame(summary = c("Sum 1", "Sum 2")),
      launch_app = FALSE
    ),
    "llm_output must have an 'id' column"
  )
})
