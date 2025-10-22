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
ai_text <- function(.data, chat_fn, type_object = NULL, few_shot_examples = NULL,
                    verbose = TRUE, result_env = NULL, module = NULL, ...) {
  
  if (!is.character(.data))
    stop("Unsupported data type for ai_text")
  
  if (length(.data) == 0)
    stop("No documents to process")
  
  # If a module is provided, extract its components
  if (!is.null(module)) {
    if (!inherits(module, "llm_module")) {
      stop("The provided module must be created using make_module().")
    }
    system_prompt <- module$system_prompt
    type_object <- module$type_object
  } else {
    # Ensure type_object is provided if no module is used
    if (is.null(type_object)) {
      stop("You must provide a type_object or use a module.")
    }
    system_prompt <- NULL
  }
  
  if (!inherits(type_object, c("ellmer::TypeObject", "ellmer::TypeArray", "ellmer::Type"))) {
    stop("type_object must be created with ellmer::type_object() or related functions")
  }
  
  # Capture additional arguments
  args <- rlang::list2(...)
  
  # Create or use existing environment
  if (is.null(result_env)) {
    result_env <- new.env()
  }
  
  # Set up system prompt
  if (is.null(system_prompt)) {
    if (!"system_prompt" %in% names(args)) {
      if (!is.null(few_shot_examples)) {
        if (!is.data.frame(few_shot_examples) || !all(c("text", "score") %in% colnames(few_shot_examples))) {
          stop("few_shot_examples must be a data frame with columns 'text' and 'score'.")
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
  } else {
    args <- c(args, list(system_prompt = system_prompt))
  }
  
  # Initialize chat object
  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()
  
  # Progress bar
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = length(.data),
      clear = FALSE
    )
  }
  
  # Process all documents in one batch
  tryCatch({
    results <- parallel_chat_structured(
      chat,
      prompts = as.list(.data),  # Pass all documents as a list
      type = type_object,
      convert = TRUE,
      include_tokens = FALSE,
      include_cost = FALSE,
      max_active = 10,
      rpm = 500
    )
    
    # Ensure the number of rows in results matches the number of IDs
    if (nrow(results) != length(.data)) {
      stop("Mismatch between the number of results and the number of input documents.")
    }
    
    # Add the document IDs as a new column
    results$id <- names(.data)
    
    # Reorder columns to place `id` first
    result_df <- results[, c("id", setdiff(names(results), "id"))]
    
    # Update progress bar
    if (verbose) cli::cli_progress_done(pb)
    
    # Final message summarizing the output
    if (verbose) {
      cli::cli_alert_success("Processed {.val {nrow(result_df)}} documents successfully.")
    }
    
    return(result_df)
  }, error = function(e) {
    if (verbose) cli::cli_alert_danger("Error during batch processing: {.emph {e$message}}")
    stop(e)
  })
}