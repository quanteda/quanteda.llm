#' Structured AI analysis of a character vector / docvar in a [quanteda::corpus]
#'
#' This function applies ai-assisted analysis to each document in a character vector or
#' a docvar in a [quanteda::corpus], using a structured type_object() to define the expected
#' response format. It supports few-shot learning examples for better performance.
#' 
#' @param .data A character vector of texts (with names!)
#' @param chat_fn A function like `chat_openai()` from {ellmer}
#' @param type_object A `type_object()` describing the expected structured output
#' @param few_shot_examples Optional few-shot learning examples (data frame with `text`, `score`)
#' @param verbose Logical; whether to print progress
#' @param checkpoint_file A filename to save results after each call (enables resume), file is saved as RDS to working directory
#' @param ... additional arguments passed to chat_fn
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names; each element defined by the type_object is added as a character vector
#'   
#' @examples
#' \dontrun{
#' library(quanteda)
#' # Example with few-shot learning
#' few_shot_examples <- data.frame(
#'  text = c("Document 1 text", "Document 2 text"),
#'  score = c(5, 3)
#'  )
#' ai_text(data_corpus_inaugural[1:2], chat_fn = chat_openai,
#' type_object = type_object(
#' "Rate for tone", # this could be a much longer prompt 
#' score = type_integer("From 1 to 5"),
#' evidence = type_string("Justify score")
#' ), few_shot_examples = few_shot_examples, checkpoint_file = "ai_text_results.RDS")
#' @export
ai_text <- function(.data, chat_fn, ..., type_object, few_shot_examples = NULL, 
                    verbose = TRUE, checkpoint_file = "ai_text_results.RDS") {
  if (is.character(.data)) {
    return(ai_text_character(.data, chat_fn, ..., type_object = type_object, 
                             few_shot_examples = few_shot_examples, 
                             verbose = verbose, 
                             checkpoint_file = checkpoint_file))
  } else {
    stop("Unsupported data type for ai_text")
  }
}

#' @export
#' @importFrom glue glue
ai_text_character <- function(.data, chat_fn, ..., type_object, few_shot_examples = NULL, 
                              verbose = TRUE, checkpoint_file = "ai_text_results.RDS") {
  args <- list(...)
  
  if (is.null(names(.data))) names(.data) <- as.character(seq_along(.data))
  
  # Construct system prompt
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
  
  # Add model default if needed
  if (!"model" %in% names(args) && identical(chat_fn, chat_openai)) {
    args <- c(args, list(model = "gpt-4o"))
  }
  
  # Resume logic
  if (file.exists(checkpoint_file)) {
    if (verbose) message("Loading existing results from checkpoint file: ", checkpoint_file)
    results <- readRDS(checkpoint_file)
  } else {
    results <- list()
  }
  
  remaining_names <- setdiff(names(.data), names(results))
  
  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()
  
  if (verbose) cat("Calling", deparse(substitute(chat_fn)), "(", model, "):\n")
  
  for (doc_id in remaining_names) {
    i <- which(names(.data) == doc_id)
    
    if (verbose) cat("... processing:", "[", i, "/", length(.data), "]", doc_id, "\n")
    if (i > 1) suppressMessages(chat <- do.call(chat_fn, args))
    
    tryCatch({
      data <- chat$chat_structured(.data[i], type = type_object)
      flat <- unlist(data, recursive = TRUE, use.names = TRUE)
      results[[doc_id]] <- as.data.frame(as.list(flat), stringsAsFactors = FALSE)
      saveRDS(results, checkpoint_file)
    }, error = function(e) {
      warning(glue::glue("Skipping document {doc_id} due to error: {e$message}"))
    })
  }
  
  df_results <- dplyr::bind_rows(results, .id = "id")
  rownames(df_results) <- NULL
  
  if (verbose) message("Finished.")
  return(df_results)
}
