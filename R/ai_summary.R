#' Summarize documents in a corpus or character vector
#' 
#' This function is a wrapper around `ai_text` to summarize documents using a chat function.
#' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be summarized
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param model a llm model object
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param topics character; a vector of topic names for which salience scores
#' @param ... additional arguments passed to `chat_fn`
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
#' @import ellmer
#' @export
#' @examples
#' \dontrun{
#' library(quanteda)
#' summaries <- ai_summary(data_corpus_inaugural, chat_fn = chat_openai, model = "gpt-4o",
#'                 api_args = list(temperature = 0, seed = 42))
#' }
#' @export
ai_summary <- function(.data, chat_fn, ..., verbose = TRUE) {
  # Predefine the type_object for summarizing documents
  predefined_type_object <- type_object(
    "Summary of the document",
    summary = type_string("Summarize the document in a few sentences.")
  )
  
  # Call ai_text with pre-set type_object and flexible arguments
  ai_text(
    .data = .data,
    chat_fn = chat_fn,
    type_object = predefined_type_object,
    verbose = verbose,
    ...
  )
}