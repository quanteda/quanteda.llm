#' Qualitatively assesses documents using a llm model
#'
#' Use the llm model to analyse a set of documents. The text-based response is
#' added as an additional column to the input data.
#' @param .data a data frame containing the documents to be summarized
#' @param texts the name of the docvar/column in the corpus/data frame
#'   containing the documents to be summarized
#' @param model a llm model object
#' @param prompt the instruction to the llm model for the assessment - be as
#'   specific as possible
#' @return the input quanteda corpus or data frame with the document assessments
#'   stored as an additional docvar / column
#' @import ellmer quanteda dplyr
#' @export
#' @examples
#' \dontrun{
#' prompt = "Is this text leaning towards the political left?
#'           The political left defined as groups which advocate for social equality,
#'           government intervention in the economy, and progressive policies."
#' result <- ai_qual(corpus_split, "splits", "llama3.2", prompt)
#' result
#' }
ai_qual <- function(.data, texts, model, prompt) {
  is_corpus <- is.corpus(.data)

  if (is_corpus) {
    docvars <- docvars(.data)
    .data <- convert(.data, to = "data.frame") %>%
      bind_cols(docvars)
    texts <- "text"
  }

  stopifnot(is.data.frame(.data), texts %in% colnames(.data))

  chat <- chat_ollama(model = model)
  type_assess <- type_object(
    assess = type_string("Provide a detailed response with justification.")
  )

  # Initialize the new docvar
  .data$assess <- NA

  # Loop through each document in the data frame
  for (i in seq_len(nrow(.data))) {
    # Extract the document text
    doc_text <- .data[[texts]][i]

    # Extract the assessment
    data <- chat$extract_data(prompt, doc_text, type = type_assess)

    # Store the assessment in the new docvar
    .data$assess[i] <- data$assess
  }

  # Convert back to corpus if the input was a corpus
  if (is_corpus) {
    text_col <- which(names(.data) == "text")
    result <- corpus(.data$text, docvars = .data[, -text_col, drop = FALSE])
    return(result)
  }

  # Return the modified data frame as output
  return(.data)
}
