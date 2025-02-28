#' Summarize documents using a llm model
#' Use the llm model to summarize a set of documents.
#' The response is added as an additional docvar to the input data
#' @param .data a data frame containing the documents to be summarized
#' @param texts the name of the column in the data frame containing the documents to be summarized
#' @param model a llm model object
#' @param length the number of words to include in the summary
#' @param strict a logical indicating whether the summary should be exactly `length` words, default is `FALSE`
#'
#' @return the input data frame with the document summaries stored as an additional column
#' @name ai_sum
#' @import ellmer
#' @import quanteda
#' @export
#' @examples
#' # Don't run
#' #ai_summary <- ai_sum(speeches_split,
#' #"splits",
#' #"llama3.2",
#' #100)
#'
ai_sum <- function(.data, texts, model, length, strict = FALSE) {
  is_corpus <- inherits(.data, "corpus")

  if (is_corpus) {
    docvars <- docvars(.data)
    .data <- convert(.data, to = "data.frame")
    .data <- bind_cols(.data, docvars)
    texts <- "text"
  }

  # Check if .data is a data frame
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame")
  }

  # Check if the texts column exists in the data frame
  if (!texts %in% colnames(.data)) {
    stop(paste("Column", texts, "not found in the data frame"))
  }

  # Initialize the chat model
  chat <- chat_ollama(model = model)

  # Define the type summary
  type_summary <- type_object(
    "Summary of the document.",
    summary = type_string(paste("Summary of the text. Provide exactly", length, "words in the summary."))
  )

  # Initialize the new summary column
  .data$summary <- NA

  # Loop through each document in the data frame
  for (i in 1:nrow(.data)) {
    # Extract the document text
    doc_text <- .data[[texts]][i]

    # Extract the summary
    data <- chat$extract_data(doc_text, type = type_summary)

    # Post-process the summary to enforce the length restriction if strict is TRUE
    if (strict) {
      summary_words <- strsplit(data$summary, "\\s+")[[1]]
      if (length(summary_words) > length) {
        summary_words <- summary_words[1:length]
      }
      summary <- paste(summary_words, collapse = " ")
    } else {
      summary <- data$summary
    }

    # Store the summary in the new summary column
    .data$summary[i] <- summary
  }

  # Convert back to corpus if the input was a corpus
  if (is_corpus) {
    result <- corpus(.data$text, docvars = .data %>% select(-text))
    return(result)
  }

  # Return the modified data frame as output
  return(.data)
}
