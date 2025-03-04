#' Labels documents in a corpus according to a content analysis scheme
#'
#' Use the llm model to analyse a set of documents. The text-based response is
#' added as an additional column to the input data.
#' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be summarized
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param model a llm model object
#' @param label the defined content analysis scheme for the labels - be as specific as possible
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param ... additional arguments passed to `chat_fn`
#'
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
#' @name ai_label
#' @import ellmer quanteda
#' @export
#' @examples
#' \dontrun{
#' library(quanteda)
#' label = "Label the following document based on how much it aligns with 
#'   the political left, center, or right. The political left is defined as groups 
#'   which advocate for social equality, government intervention in the economy, 
#'   and progressive policies. The political center typically supports a balance 
#'   between progressive and conservative views, favoring moderate policies       
#'   and compromise. The political right generally advocates for individualism, 
#'   free-market capitalism, and traditional values."
#' label1 <- ai_label(data_corpus_inaugural[1:2], chat_fn = chat_ollama, model = "llama3.2", label = label)
#' label2 <- ai_label(data_char_ukimmig2010, chat_fn = chat_openai,
#'                api_args = list(temperature = 0, seed = 42), label = label)
#' }
ai_label <- function(.data, chat_fn, ..., label, verbose = TRUE) {
  if (is.character(.data)) {
    return(ai_label_character(.data, chat_fn, ..., label = label, verbose = verbose))
  } else {
    stop("Unsupported data type for ai_score")
  }
}
#' @export
#' @importFrom glue glue
ai_label_character <- function(.data, chat_fn, ..., label, verbose = TRUE) {
  args <- list(...)
  if (!"system_prompt" %in% names(args)) {
    args <- c(args, list(system_prompt = global_system_prompt))
  }
  if (!"model" %in% names(args) & identical(chat_fn, chat_ollama)) {
    args <- c(args, list(model = "llama3.2"))
  }
  
  type_label <- type_object(
    label = type_string("Provide a detailed response with justification.")
  )
  names <- names(.data)
  
  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()
  
  if (verbose)
    message(glue("Calling {deparse(substitute(chat_fn))} ({model}):"))
  
  result <- numeric(length(.data))
  for (i in seq_along(.data)) {
    if (verbose) {
      message(glue("... processing: [{i}/{length(.data)}] {names[i]}"))
    }
    
    if (i > 1)
      suppressMessages(chat <- do.call(chat_fn, args))
    result[i] <- chat$extract_data(.data[i], type = type_label)
  }
  
  if (verbose)
    message(glue("Finished."))
  
  # might be a list but should not be longer than one element each
  stopifnot( max(lengths(result)) == 1)
  
  result <- unlist(result)
  names(result) <- names
  result
}

