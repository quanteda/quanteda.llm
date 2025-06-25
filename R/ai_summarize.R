#' Summarize documents in a corpus
#'
#' Use the llm model to summarize a set of documents.
#' The response is added as an additional docvar to the input data
#' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be summarized
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param model a llm model object
#' @param summary_length integer; the length in words to instruct the chat function
#'   to return in the response
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param ... additional arguments passed to `chat_fn`
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
#' @import ellmer
#' @export
#' @examples
#' \dontrun{
#' library(quanteda)
#' summ1 <- ai_summarize(data_char_ukimmig2010, chat_fn = chat_ollama, model = "llama3.2")
#' summ2 <- ai_summarize(data_corpus_inaugural[1:2], chat_fn = chat_openai,
#'                 api_args = list(temperature = 0, seed = 42))
#' }
ai_summarize <- function(.data, chat_fn, ..., summary_length = 200L, verbose = TRUE) {
  UseMethod("ai_summarize")
}

#' @export
#' @importFrom glue glue
ai_summarize.character <- function(.data, chat_fn, ..., summary_length = 200L, verbose = TRUE) {

  args <- list(...)
  if (!"system_prompt" %in% names(args)) {
    args <- c(args, list(system_prompt = global_system_prompt))
  }
  if (!"model" %in% names(args) & identical(chat_fn, chat_ollama)) {
    args <- c(args, list(model = "llama3.2"))
  }

  type_summary <- type_object(
    "Summary of the document.",
    summary = type_string(paste("Summary of the text. Summarize the text in approximately", summary_length, "words."))
  )
  names <- names(.data)

  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()

  if (verbose)
    cat("Calling", deparse(substitute(chat_fn)), "(", model, "):\n")

  result <- character(length(.data))
  for (i in seq_along(.data)) {
    if (verbose) {
      cat("... processing:", "[", i, "/", length(.data), "]", names[i], "\n")
    }

    if (i > 1)
      suppressMessages(chat <- do.call(chat_fn, args))
    result[i] <- chat$chat_structured(.data[i], type = type_summary)
  }

  if (verbose)
    message(glue("Finished."))

  # might be a list but should not be longer than one element each
  stopifnot( max(lengths(result)) == 1)

  result <- unlist(result)
  names(result) <- names
  result
}
