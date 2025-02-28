#' Split documents into maximum length segments which the llm model accepts as input data
#' Use the splitting to prepare the data for further llm processing
#' The split segments are stored as a new docvar in the input corpus or row in the input data frame
#'and a new index is created to keep track of the original document
#' @param .data a quanteda corpus or data frame containing the documents to be split
#' @param texts the name of the column in the data frame containing the documents to be summarized
#' @param length_seq the number of characters to include in each segment based on llm model requirements
#' @param model the name of the llm model to use for splitting the documents,
#' this is optional and can be used instead of length_seq to get the maximum length of the segments accepted by specific llm models
#' # for a list of supported models see `llm_info`
#'
#' @return a data frame with the split segments stored as additional rows and a new index to keep track of the original document
#' @name text_split
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import quanteda
#' @export
#' @examples
#' # Don't run
#' \dontrun{
#' speeches_split <- text_split(speeches, "text", length_seq = 512)
#' }
#'
text_split <- function(.data, texts, length_seq = NULL, model = NULL) {
  if (!is.null(model)) {
    if (model %in% names(llm_info)) {
      length_seq <- llm_info[[model]]
    } else {
      stop("Specified model is not in the list of known models.")
    }
  }

  if (is.null(length_seq)) {
    stop("Either length_seq or model must be specified.")
  }

  is_corpus <- inherits(.data, "corpus")

  if (is_corpus) {
    docvars <- docvars(.data)
    .data <- convert(.data, to = "data.frame")
    .data <- bind_cols(.data, docvars)
    texts <- "text"
  }

  result <- .data %>%
    mutate(row_id = row_number()) %>%
    rowwise() %>%
    mutate(splits = list({
      text <- as.character(.data[[texts]])
      split_indices <- seq(1, nchar(text), by = length_seq)
      map_chr(split_indices, ~ substr(text, .x, min(nchar(text), .x + length_seq - 1)))
    })) %>%
    unnest(splits) %>%
    group_by(row_id) %>%
    mutate(text_index = row_number()) %>%
    ungroup() %>%
    select(-row_id)

  if (is_corpus) {
    result <- result %>%
      mutate(doc_id = row_number()) %>%
      group_by(doc_id) %>%
      mutate(text = splits) %>%
      ungroup() %>%
      select(-splits)

    drop_col <- which(names(.data) %in% c("text", "doc_id"))
    result <- corpus(.data$text, docvars = .data[, -drop_col, drop = FALSE])
  }

  return(result)
}
