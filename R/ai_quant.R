#' Score documents using a llm model
#' Use the llm model to score a set of documents.
#' The numerical scores for each text are added as an additional docvar/column to the input data
#' @param .data a \pkg{quanteda} corpus or data frame containing the documents to be summarized
#' @param texts the name of the docvar/column in the corpus/data frame containing the documents to be summarized
#' @param model a llm model object
#' @param concept the concept based on which the llm should score how much the documents align to it - be as specific as possible
#'
#' @return the input \pkg{quanteda} corpus or data frame with the document scores stored as an additional docvar/column
#' @name ai_quant
#' @import ellmer quanteda
#' @export
#' @examples
#' \dontrun{
#' concept = "The political left defined as groups which advocate for social equality,
#'            government intervention in the economy, and progressive policies."
#' sai_quanties <- ai_quant(speeches_split, "splits", "llama3.2", concept)
#' }
# Define the ai_quant function
ai_quant <- function(.data, texts, model, concept) {
  is_corpus <- inherits(.data, "corpus")

  if (is_corpus) {
    docvars <- docvars(.data)
    .data <- convert(.data, to = "data.frame") %>%
      bind_cols(docvars)
    texts <- "text"
  }

  stopifnot(is.data.frame(.data), texts %in% colnames(.data))

  chat <- chat_ollama(model = model)
  type_score <- type_object(
    score = type_number(paste("Classify how much the following text aligns with the concept:", concept, "Use the following metrics: 1 for full alignment, 0 for no alignment. The score should be between 0.0 and 1.0."))
  )

  # Initialize the new docvar
  .data$score <- NA

  # Loop through each document in the data frame
  for (i in 1:nrow(.data)) {
    # Extract the document text
    doc_text <- .data[[texts]][i]

    # Extract the score
    data <- chat$extract_data(doc_text, type = type_score)

    # Ensure the score is numeric and within the range [0.0, 1.0]
    score <- as.numeric(data$score)
    if (is.na(score) || score < 0.0 || score > 1.0) {
      score <- NA  # Handle out-of-range or invalid scores
    }

    # Store the score in the new docvar
    .data$score[i] <- score
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
