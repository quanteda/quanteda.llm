#' Structured AI analysis of texts
#'
#' This function applies AI-assisted analysis to each document in a character
#' vector or a docvar in a [corpus][quanteda::corpus()], using a structured
#' [type_object()][ellmer::type_object()] to define the expected response format. It supports
#' few-shot learning examples for better performance.
#'
#' @param .data A character vector of texts
#' @param chat_fn A function like [chat_openai()][ellmer::chat_openai()]. See
#' <https://ellmer.tidyverse.org/articles/structured-data.html> for details.
#' @param ... additional arguments passed to `chat_fn`
#' @param few_shot_examples Optional few-shot learning examples (data frame with
#'   `text`, `score`)
#' @param type_object an \pkg{ellmer} [type_object()][ellmer::type_object()]
#' @param verbose logical; whether to print progress
#' @param result_env An environment to store results and allow resuming
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents; each single element defined by
#'   [type_object()][ellmer::type_object()] is added as a character vector
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' results <- quanteda::data_corpus_inaugural[1:3] %>%
#'   ai_text(chat_fn = chat_openai, model = "gpt-4o",
#'           api_args = list(temperature = 0, seed = 42),
#'           type_object =
#'             type_object("Summary of the document",
#'                         summary = type_string("Summarize the document in a few sentences.")),
#'   )
#' }
#' @importFrom glue glue
#' @export
ai_text <- function(.data, chat_fn, type_object, few_shot_examples = NULL,
                    verbose = TRUE, result_env = NULL, ...) {
  
  # --- Ensure result_env is valid before anything else uses it ---
  if (is.null(result_env)) result_env <- new.env()

  if (!is.character(.data))
    stop("Unsupported data type for ai_text")

  args <- rlang::list2(...)

  if (is.null(names(.data))) names(.data) <- as.character(seq_along(.data))

  # Set up system prompt
  if (!"system_prompt" %in% names(args)) {
    if (!is.null(few_shot_examples)) {
      if (!is.data.frame(few_shot_examples) || !all(c("text", "score") %in% colnames(few_shot_examples))) {
        stop("`few_shot_examples` must be a data frame with columns 'text' and 'score'.")
      }

      examples_text <- paste(
        "Here are some examples for scoring:",
        paste(
          apply(few_shot_examples, 1, function(row) {
            glue::glue("Document: '{row[['text']]}' Score: {row[['score']]}")
          }),
          collapse = "\n\n"
        ),
        sep = "\n\n"
      )

      args <- c(args, list(system_prompt = paste(global_system_prompt, examples_text, sep = "\n\n")))
    } else {
      args <- c(args, list(system_prompt = global_system_prompt))
    }
  }

  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()

  if (verbose) cat("\nCalling ", deparse(substitute(chat_fn)), " (", model, "):\n",
                   sep = "")

  for (doc_id in names(.data)) {
    if (exists(doc_id, envir = result_env)) next  # skip if already processed

    i <- which(names(.data) == doc_id)
    if (verbose) cat("... processing: ", "[", i, "/", length(.data), "]\n", sep = "")

    if (i > 1) suppressMessages(chat <- do.call(chat_fn, args))

    tryCatch({
      data <- chat$chat_structured(.data[i], type = type_object)
      flat <- unlist(data, recursive = TRUE, use.names = TRUE)
      result_env[[doc_id]] <- as.data.frame(as.list(flat), stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(glue::glue("Skipping document {doc_id} due to error: {e$message}"))
    })
  }

  df_results <- dplyr::bind_rows(as.list(result_env), .id = "id")
  rownames(df_results) <- NULL
  
  # Warn if any response is empty
  empty_docs <- vapply(result_env, function(x) {
    all(vapply(x, function(col) all(nzchar(col) == FALSE), logical(1)))
  }, logical(1))
  
  if (any(empty_docs)) {
    warning(
      "One or more documents returned empty responses. ",
      "This may be due to:\n",
      "- A missing or incorrectly set API key\n",
      "- The input exceeding the model's sequence length limit\n",
      "Please check both your API setup and whether your input text is too long."
    )
  }
  
  if (verbose) cat("Finished.\n")
  
  return(df_results)
}