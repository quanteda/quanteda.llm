#' Compute scores for documents in a corpus or character vector based on a scale defined by a prompt
#' 
#' This function is a wrapper around `ai_text` to compute scores for documents using a chat function.
##' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be summarized
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param model a llm model object
#' @param prompt character; a prompt that defines the scoring criteria 
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param result_env An environment to store results and allow resuming
#' @param ... additional arguments passed to `chat_fn`
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
#' @import ellmer
#' @export
#' @examples
#' \dontrun{
#' library(quanteda)
#' # Define a prompt for scoring documents
#' prompt <- "Score the following document on a scale of how much it aligns
#' with the political left. The political left is defined as groups which
#' advocate for social equality, government intervention in the economy,
#' and progressive policies. Use the following metrics:
#' SCORING METRIC:
#' 3 : extremely left
#' 2 : very left
#' 1 : slightly left
#' 0 : not at all left"
#' # Compute scores for documents in the inaugural corpus
#' scores <- ai_score(data_corpus_inaugural, prompt, chat_fn = chat_openai, model = "gpt-4o",
#'                 api_args = list(temperature = 0, seed = 42))
#' }

#' @export
ai_score <- function(.data, prompt, chat_fn, ..., verbose = TRUE) {
  
  # Predefine the type_object for salience classification
  predefined_type_object <- type_object(
    "Scoring of documents",
    score = type_integer(paste(prompt)),
    evidence = type_string("Evidence supporting the score, summarized in a few sentences."))
  
  # Call ai_text with pre-set type_object and flexible arguments
  ai_text(
    .data = .data,
    chat_fn = chat_fn,
    type_object = predefined_type_object,
    verbose = verbose,
    ...
  )
}