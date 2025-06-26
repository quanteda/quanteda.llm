#' Classifies documents in a corpus according to a content analysis scheme
#'
#' Use the llm model to analyse a set of documents. The text-based response is
#' added as an additional column to the input data.
#' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be summarized
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param model a llm model object
#' @param topics the topics of the defined content analysis scheme - be as specific as possible
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param ... additional arguments passed to `chat_fn`
#'
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
#' @name ai_relevance
#' @import ellmer quanteda
#' @export
#' @examples
#' \dontrun{
#' library(quanteda)
#' topics = c("Politics", "Sports", "Technology", "Entertainment", "Business", "Other")
#' topic_relevance1 <- ai_relevance(data_corpus_inaugural[1:2], chat_fn = chat_ollama, model = "llama3.2", topics = topics)
#' topic_relevance2 <- ai_relevance(data_char_ukimmig2010, chat_fn = chat_openai,
#'                api_args = list(temperature = 0, seed = 42), topics = topics)
#' }
ai_relevance <- function(.data, chat_fn, ..., topics, verbose = TRUE) {
  UseMethod("ai_relevance")
}

#' @export
ai_relevance.character <- function(.data, chat_fn, ..., topics, verbose = TRUE) {
  
  args <- list(...)
  if (!"system_prompt" %in% names(args)) {
    args <- c(args, list(system_prompt = global_system_prompt))
  }
  if (!"model" %in% names(args) & identical(chat_fn, chat_openai)) {
    args <- c(args, list(model = "gpt-4o"))
  }
  
  # Define the type_array structure
  type_array <- type_array(
    "Array of classification results. The scores should sum to 1.",
    type_object(
      name = type_enum(
        "The category name",
        values = c(paste(topics)
        )
      ),
      score = type_number(
        "The score for the category, between 0 and 1, summing to 1 across all categories."
      )
    )
  )
  
  # Get document names
  names <- names(.data)
  
  # Initialize the chat object
  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()
  
  if (verbose) {
    cat("Calling", deparse(substitute(chat_fn)), "(", model, "):\n")
  }
  
  # Initialize the result vector
  result <- character(length(.data))
  
  # Process each document
  for (i in seq_along(.data)) {
    if (verbose) {
      cat("... processing:", "[", i, "/", length(.data), "]", names[i], "\n")
    }
    
    # Reinitialize the chat object for each document if necessary
    if (i > 1) {
      suppressMessages(chat <- do.call(chat_fn, args))
    }
    
    # Get the structured chat result
    chat_result <- chat$chat_structured(.data[i], type = type_array)
    
    
    # Check if chat_result is a data frame with `name` and `score` columns
    if (is.data.frame(chat_result) && all(c("name", "score") %in% colnames(chat_result))) {
      # Format the result as "Category1: Score1, Category2: Score2, ..."
      formatted_result <- paste(
        apply(chat_result, 1, function(row) paste(row["name"], ":", round(as.numeric(row["score"]), 2))),
        collapse = ", "
      )
    } else {
      # Handle unexpected output format
      warning(paste("Unexpected output format for document", names[i]))
      formatted_result <- "Invalid result"
    }
    
    result[i] <- formatted_result
  }
  
  if (verbose) {
    message("Finished.")
  }
  
  # Ensure the result is a character vector
  names(result) <- names
  
  # Return the result
  result
}