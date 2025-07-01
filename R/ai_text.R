#' Structured AI analysis of a character vector / docvar in a [quanteda::corpus]
#'
#' This function applies ai-assisted analysis to each document in a character vector or
#' a docvar in a [quanteda::corpus], using a structured type_object() to define the expected
#' response format. It supports few-shot learning examples for better performance.
#' 
#' @param .data A character vector of texts 
#' @param chat_fn A function like `chat_openai()` from {ellmer}
#' @param type_object A `type_object()` describing the expected structured output, see here for details:
#' https://ellmer.tidyverse.org/articles/structured-data.html
#' @param few_shot_examples Optional few-shot learning examples (data frame with `text`, `score`)
#' @param verbose Logical; whether to print progress
#' @param result_env An environment to store results and allow resuming
#' @param ... additional arguments passed to chat_fn
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents; each single element defined by `type_object()` is added as a character vector
#'   
#' @examples
#' \dontrun{
#' library(quanteda)
#' library(quanteda.llm)
#' #pak::pak("quanteda/quanteda.tidy")
#' library(quanteda.tidy)
#' corpus <- quanteda::data_corpus_inaugural %>%
#'   mutate(llm_sum = ai_text(text, chat_fn = chat_openai, model = "gpt-4o",
#'                              api_args = list(temperature = 0, seed = 42),
#'                              type_object = type_object("Summary of the document",
#'                              summary = type_string("Summarize the document in a few sentences."),
#'                              checkpoint_file = "llm_sum.RDS")))
#' }
#' @export
ai_text <- function(.data, chat_fn, ..., type_object, few_shot_examples = NULL, 
                    verbose = TRUE, result_env = new.env()) {
  if (is.character(.data)) {
    return(ai_text_character(.data, chat_fn, ..., type_object = type_object, 
                             few_shot_examples = few_shot_examples, 
                             verbose = verbose, 
                             result_env = result_env))
  } else {
    stop("Unsupported data type for ai_text")
  }
}
#' @export
#' @importFrom glue glue
ai_text_character <- function(.data, chat_fn, ..., type_object, few_shot_examples = NULL, 
                              verbose = TRUE, result_env = new.env()) {
  args <- list(...)
  
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
  
  if (!"model" %in% names(args) && identical(chat_fn, chat_openai)) {
    args <- c(args, list(model = "gpt-4o"))
  }
  
  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()
  
  if (verbose) cat("Calling", deparse(substitute(chat_fn)), "(", model, "):\n")
  
  for (doc_id in names(.data)) {
    if (exists(doc_id, envir = result_env)) next  # skip if already processed
    
    i <- which(names(.data) == doc_id)
    if (verbose) cat("... processing:", "[", i, "/", length(.data), "]")
    
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
  if (verbose) message("Finished.")
  
  return(df_results)
}
