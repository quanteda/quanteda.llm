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
#' library(quanteda)
#' results <- quanteda::data_corpus_inaugural[1:3] %>%
#'   ai_text(chat_fn = chat_openai, model = "gpt-4o",
#'           api_args = list(temperature = 0, seed = 42),
#'           type_object =
#'             type_object("Summary of the document",
#'                         summary = type_string("Summarize the document in a few sentences.")),
#'   )
#' }
#' @importFrom glue glue
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @export
ai_text <- function(.data, chat_fn, type_object, few_shot_examples = NULL,
                    verbose = TRUE, result_env = NULL, ...) {
  
  # --- Ensure result_env is valid before anything else uses it ---
  if (is.null(result_env)) result_env <- new.env()
  
  if (!is.character(.data))
    stop("Unsupported data type for ai_text")
  
  args <- rlang::list2(...)
  
  if (is.null(names(.data)))
    names(.data) <- paste0("text", as.character(seq_along(.data)))
  original_order <- names(.data)
  
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
  
  # Count already processed documents
  already_processed <- sum(names(.data) %in% names(result_env))
  total_docs <- length(.data)
  to_process <- total_docs - already_processed
  
  current_doc <- NA_character_  # Needed for progress bar format
  
  if (verbose && to_process > 0) {
    cli::cli_inform("Using {.fn {deparse(substitute(chat_fn))}} with model {.val {model}}")
    
    cli::cli_progress_bar(
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta} | {.file {current_doc}}",
      total = to_process,
      clear = FALSE,
      .envir = environment()
    )
    
    cli::cli_progress_update(set = 0, .envir = environment())
  }
  
  for (i in seq_along(.data)) {
    doc_id <- names(.data)[i]
    current_doc <- doc_id  # Will show up in progress bar
    
    if (exists(doc_id, envir = result_env)) {
      if (verbose) {
        cli::cli_progress_update(set = i, .envir = environment())
      }
      next
    }
    
    if (verbose) {
      cli::cli_progress_update(set = i - 1 + 0.5, .envir = environment())
    }
    
    if (i > 1) suppressMessages(chat <- do.call(chat_fn, args))
    
    tryCatch({
      data <- chat$chat_structured(.data[i], type = type_object)
      flat <- unlist(data, recursive = TRUE, use.names = TRUE)
      result_env[[doc_id]] <- as.data.frame(as.list(flat), stringsAsFactors = FALSE)
      
      if (verbose) {
        cli::cli_progress_update(set = i, .envir = environment())
      }
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger("Failed to process document {.val {doc_id}}: {.emph {e$message}}")
        cli::cli_progress_update(set = i, .envir = environment())
      }
      warning(glue::glue("Skipping document {doc_id} due to error: {e$message}"))
    })
  }
  
  # Reconstruct results in original order
  df_list <- list()
  for (doc_id in original_order) {
    if (exists(doc_id, envir = result_env)) {
      df_list[[doc_id]] <- cbind(id = doc_id, result_env[[doc_id]],
                                 stringsAsFactors = FALSE)
    }
  }
  
  df_results <- do.call(rbind, df_list)
  rownames(df_results) <- NULL
  
  # Check for empty responses
  empty_docs <- vapply(result_env, function(x) {
    all(vapply(x, function(col) all(nzchar(col) == FALSE), logical(1)))
  }, logical(1))
  
  if (any(empty_docs)) {
    empty_count <- sum(empty_docs)
    empty_names <- names(which(empty_docs))
    
    cli::cli_alert_warning(c(
      "{.val {empty_count}} document{?s} returned empty response{?s}",
      "!" = "This may be due to:",
      "*" = "Missing or incorrectly set API key",
      "*" = "Input text exceeding model's sequence length limit",
      "i" = "Empty documents: {.val {empty_names}}"
    ))
  }
  
  if (verbose) {
    cli::cli_alert_success("Processed {.val {total_docs}} document{?s} successfully")
  }
  
  return(df_results)
}
