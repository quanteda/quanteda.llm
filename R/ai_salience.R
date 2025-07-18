#' Compute salience scores for topics
#'
#' This function is a wrapper around [ai_text()] to compute salience scores for
#' topics in a corpus using a chat function.
##' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be summarized
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param topics character; a vector of topic names for which salience scores
#'   will be computed; the scores sum to 1 across all topics
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
#' salience <- ai_salience(data_corpus_inaugural[1:3],
#'                         chat_fn = chat_openai, model = "gpt-4o",
#'                         api_args = list(temperature = 0, seed = 42),
#'                         topics = c("economy", "environment", "healthcare"))
#' }
#' @export
ai_salience <- function(.data, topics, chat_fn, ..., verbose = TRUE) {

  # Predefine the type_object for salience classification
  predefined_type_object <- type_array(
    "Array of classification results. The scores should sum to 1.",
    type_object(
      topic = type_enum("The category name", values = paste(topics)),
      score = type_number("The salience score for the category, ranging from 0.0 to 1.0")
    )
  )

  # Call ai_text with pre-set type_object and flexible arguments
  result <- ai_text(
    .data = .data,
    chat_fn = chat_fn,
    type_object = predefined_type_object,
    verbose = verbose,
    ...
  )

  # Remove topic columns
  topic_cols <- grep("^topic", names(result), value = TRUE)
  result <- result[, !names(result) %in% topic_cols]

  # Rename score columns to salience_[topic]
  score_cols <- grep("^score", names(result), value = TRUE)
  new_names <- paste0("salience_", topics)

  # Rename the columns (assuming they're in the same order as topics)
  for (i in seq_along(score_cols)) {
    names(result)[names(result) == score_cols[i]] <- new_names[i]
  }

  # make salience values numeric
  for (col in paste0("salience_", topics)) {
    result[[col]] <- as.numeric(result[[col]])
  }

  return(result)
}
