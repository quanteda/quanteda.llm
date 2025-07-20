test_that("ai_text makes a simple structured request", {
  chat <- chat_test("Always return 0.8")
  result <- ai_text(
    .data = c(doc1 = "What is 1 + 1?"),
    chat_fn = chat_test,
    type_object = type_object("classification")
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
    type_object = type_object("classification"),
    few_shot_examples = few_shot
  )

  expect_equal(result$score, 0.8)
})

test_that("ai_text handles multiple documents", {
  chat <- chat_test("Always return 0.8")

  result <- ai_text(
    .data = c(doc1 = "Text A", doc2 = "Text B"),
    chat_fn = chat_test,
    type_object = type_object("classification"),
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$score, c(0.8, 0.8))
})

# Error handling --------------------------------------------------------

test_that("ai_text skips already processed documents", {
  skip()
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

test_that("mock chat provider returns correct structure", {
  chat <- chat_test()
  expect_equal(chat$get_model(), "mock-model")
  resp <- chat$chat_structured("Test", type = "classification")
  expect_equal(resp, list(score = 0.8))
})

test_that("ai_text handles document failures gracefully", {
  # Create a counter that persists across chat creations
  .test_env <- new.env()
  .test_env$call_count <- 0

  # Mock chat function that fails on 3rd document
  chat_fn_failing <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        .test_env$call_count <- .test_env$call_count + 1
        if (.test_env$call_count == 3) stop("API error")
        list(score = .test_env$call_count * 0.1)
      }
    ), class = "MockChat")
  }

  docs <- c(doc1 = "Text 1", doc2 = "Text 2", doc3 = "Text 3", doc4 = "Text 4")

  # Suppress warnings about skipped documents
  suppressWarnings({
    result <- ai_text(
      .data = docs,
      chat_fn = chat_fn_failing,
      type_object = type_object("test", score = type_number("Score")),
      verbose = FALSE
    )
  })

  # Check the results
  expect_equal(nrow(result), 4)  # All docs present
  expect_equal(result$score[1], 0.1)  # First doc processed
  expect_equal(result$score[2], 0.2)  # Second doc processed
  expect_true(is.na(result$score[3]))  # Failed doc has NA
  expect_equal(result$score[4], 0.4)  # Fourth doc processed (count = 4)

  # Check failed documents attribute
  expect_equal(attr(result, "failed_documents"), "doc3")
})


# Test for environment persistence
test_that("ai_text preserves external result_env", {
  env <- new.env()
  env[["doc1"]] <- data.frame(score = 1)

  result <- ai_text(
    .data = c(doc1 = "Old", doc2 = "New"),
    chat_fn = chat_test,
    type_object = type_object("test", score = type_number("Score")),
    result_env = env,
    verbose = FALSE
  )

  expect_equal(env[["doc1"]]$score, 1)  # Original preserved
  expect_true(exists("doc2", envir = env))  # New added
})

test_that("ai_text handles empty input gracefully", {
  expect_error(
    ai_text(
      .data = character(0),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "No documents to process"
  )
})

test_that("ai_text validates input data type", {
  # Test numeric input
  expect_error(
    ai_text(
      .data = c(1, 2, 3),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "Unsupported data type for ai_text"
  )

  # Test data.frame input
  expect_error(
    ai_text(
      .data = data.frame(text = c("a", "b")),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "Unsupported data type for ai_text"
  )

  # Test list input
  expect_error(
    ai_text(
      .data = list("a", "b"),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "Unsupported data type for ai_text"
  )

  # Test factor input
  expect_error(
    ai_text(
      .data = factor(c("a", "b")),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "Unsupported data type for ai_text"
  )
})

test_that("ai_text validates empty input", {
  # Test character(0)
  expect_error(
    ai_text(
      .data = character(0),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "No documents to process"
  )

  # Test character vector with all empty strings (should NOT error)
  expect_no_error(
    ai_text(
      .data = c("", ""),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score")),
      verbose = FALSE
    )
  )
})

test_that("ai_text validates type_object parameter", {
  # Test with NULL type_object
  expect_error(
    ai_text(
      .data = c("test text"),
      chat_fn = chat_test,
      type_object = NULL
    ),
    "type_object must be created with ellmer::type_object()",
    fixed = TRUE
  )

  # Test with wrong object type
  expect_error(
    ai_text(
      .data = c("test text"),
      chat_fn = chat_test,
      type_object = list(score = "numeric")
    ),
    "type_object must be created with ellmer::type_object()",
    fixed = TRUE
  )

  # Test with character string
  expect_error(
    ai_text(
      .data = c("test text"),
      chat_fn = chat_test,
      type_object = "classification"
    ),
    "type_object must be created with ellmer::type_object()",
    fixed = TRUE
  )
})

test_that("ai_text handles multiple validation errors correctly", {
  # Test empty non-character input (should fail on first check)
  expect_error(
    ai_text(
      .data = numeric(0),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    "Unsupported data type for ai_text"
  )

  # Test invalid type_object with valid data (should fail on type_object check)
  expect_error(
    ai_text(
      .data = c("valid text"),
      chat_fn = chat_test,
      type_object = "not a type object"
    ),
    "type_object must be created with ellmer::type_object()",
    fixed = TRUE
  )
})

test_that("ai_text accepts valid inputs", {
  # Test with minimal valid input
  result <- ai_text(
    .data = c("test document"),
    chat_fn = chat_test,
    type_object = type_object("test", score = type_number("Score")),
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Test with named character vector
  result <- ai_text(
    .data = c(doc1 = "first", doc2 = "second"),
    chat_fn = chat_test,
    type_object = type_object("test", score = type_number("Score")),
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$id, c("doc1", "doc2"))
})

test_that("error messages are clean without tryCatch wrapping", {
  # Capture the actual error message
  err <- tryCatch(
    ai_text(
      .data = character(0),
      chat_fn = chat_test,
      type_object = type_object("test", score = type_number("Score"))
    ),
    error = function(e) e$message
  )

  # Should be exactly the error message, no extra text
  expect_equal(err, "No documents to process")

  # Should NOT contain processing-related messages
  expect_false(grepl("Processing interrupted", err))
  expect_false(grepl("Returning", err))
})

test_that("ai_text provides enhanced error messages for JSON parsing failures", {
  # Mock that returns invalid JSON (triggers parse error)
  chat_fn_json_error <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        # Simulate a JSON parsing error
        stop("parse error: premature EOF\n                     {\"score\": \n                     (right here) ------^")
      }
    ), class = "MockChat")
  }

  # Create a long document to test length reporting
  long_doc <- paste(rep("This is a long document. ", 1000), collapse = "")
  docs <- c(doc1 = long_doc)

  # Capture messages
  messages <- capture_messages({
    suppressWarnings({
      result <- ai_text(
        .data = docs,
        chat_fn = chat_fn_json_error,
        type_object = structure(list(), class = "ellmer::Type"),
        verbose = TRUE
      )
    })
  })

  # Check that enhanced error message appears
  expect_match(messages, "JSON parsing failed", all = FALSE)
  expect_match(messages, "Document length:.*characters", all = FALSE)
  expect_match(messages, "documents exceed model token limits", all = FALSE)
  expect_match(messages, "Consider.*truncating", all = FALSE)
})

test_that("ai_text shows regular error messages for non-JSON errors", {
  # Mock that returns a different type of error
  chat_fn_api_error <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        stop("API rate limit exceeded")
      }
    ), class = "MockChat")
  }

  docs <- c(doc1 = "Test document")

  messages <- capture_messages({
    suppressWarnings({
      result <- ai_text(
        .data = docs,
        chat_fn = chat_fn_api_error,
        type_object = structure(list(), class = "ellmer::Type"),
        verbose = TRUE
      )
    })
  })

  # Should show rate limit specific message
  expect_match(messages, "Rate limit exceeded for", all = FALSE)
  expect_match(messages, "API quota or rate limit reached", all = FALSE)

  # Should NOT show generic error message for rate limit errors
  expect_no_match(messages, "Failed to process document", all = FALSE)
  # Should NOT show JSON-specific messages
  expect_no_match(messages, "JSON parsing failed", all = FALSE)
  expect_no_match(messages, "token limits", all = FALSE)
})

test_that("ai_text shows generic error messages for unrecognized errors", {
  # Mock that returns an unrecognized error type
  chat_fn_unknown_error <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        stop("Unknown error occurred")
      }
    ), class = "MockChat")
  }

  docs <- c(doc1 = "Test document")

  messages <- capture_messages({
    suppressWarnings({
      result <- ai_text(
        .data = docs,
        chat_fn = chat_fn_unknown_error,
        type_object = structure(list(), class = "ellmer::Type"),
        verbose = TRUE
      )
    })
  })

  # Should show generic error message
  expect_match(messages, "Failed to process document.*Unknown error occurred", all = FALSE)
  # Should NOT show specific error messages
  expect_no_match(messages, "JSON parsing failed", all = FALSE)
  expect_no_match(messages, "Rate limit exceeded", all = FALSE)
  expect_no_match(messages, "Request timeout", all = FALSE)
})


test_that("ai_text reports document length accurately in error messages", {
  chat_fn_json_error <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        stop("parse error: premature EOF")
      }
    ), class = "MockChat")
  }

  # Test with specific length document
  test_text <- paste(rep("a", 12345), collapse = "")
  docs <- c(test_doc = test_text)

  messages <- capture_messages({
    suppressWarnings({
      result <- ai_text(
        .data = docs,
        chat_fn = chat_fn_json_error,
        type_object = structure(list(), class = "ellmer::Type"),
        verbose = TRUE
      )
    })
  })

  # Check that it reports the correct length
  expect_match(messages, "Document length:.*12345.*characters", all = FALSE)
})

test_that("enhanced error messages respect verbose setting", {
  chat_fn_json_error <- function(...) {
    structure(list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        stop("parse error: premature EOF")
      }
    ), class = "MockChat")
  }

  docs <- c(doc1 = "Test")

  # With verbose = FALSE, should not see detailed messages
  messages <- capture_messages({
    warnings <- capture_warnings({
      result <- ai_text(
        .data = docs,
        chat_fn = chat_fn_json_error,
        type_object = structure(list(), class = "ellmer::Type"),
        verbose = FALSE
      )
    })
  })

  # Should not see cli messages when verbose = FALSE
  expect_length(messages, 0)

  # But should still get the warning
  expect_match(warnings, "Skipping document doc1 due to error", all = FALSE)
})
