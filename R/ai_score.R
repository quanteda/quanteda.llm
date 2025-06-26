#' Scores documents in a corpus according to a defined scale
#' 
#' Use the llm model to score a set of documents.
#' The numerical scores for each text are added as an additional docvar to the input data
#' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be scored
#' @param chat_fn function; a chat function from \pkg{ellmer}
#' @param model a llm model object
#' @param scale the defined scale question to generate a numeric answer - be as specific as possible
#' @param few_shot_examples a data frame with two columns: `text` and `score`, providing examples of how to score documents. This is optional but can help the model understand the scoring criteria better.
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param ... additional arguments passed to `chat_fn`
#'
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
#' @name ai_score
#' @import ellmer quanteda
#' @export
#' @examples
#' \dontrun{
#' library(quanteda)
#' scale = "Score the following document on a scale of how much it aligns
#'            with the political left. The political left is defined as groups which 
#'            advocate for social equality, government intervention in the economy, 
#'            and progressive policies. Use the following metrics: 
#'            SCORING METRIC:
#'            1 : extremely left
#'            0 : not at all left"
#' score1 <- ai_score(data_corpus_inaugural[1:2], chat_fn = chat_ollama, model = "llama3.2", scale = scale)
#' score2 <- ai_score(data_char_ukimmig2010, chat_fn = chat_openai,
#'                api_args = list(temperature = 0, seed = 42), scale = scale)
#' }
#' @export
ai_score <- function(.data, chat_fn, ..., scale, few_shot_examples = NULL, verbose = TRUE) {
  if (is.character(.data)) {
    return(ai_score_character(.data, chat_fn, ..., scale = scale, few_shot_examples = few_shot_examples, verbose = verbose))
  } else {
    stop("Unsupported data type for ai_score")
  }
}

#' @export
#' @importFrom glue glue
ai_score_character <- function(.data, chat_fn, ..., scale, few_shot_examples = NULL, verbose = TRUE) {
  args <- list(...)
  
  # Add a system prompt if not provided
  if (!"system_prompt" %in% names(args)) {
    # Include few-shot examples in the system prompt if provided
    if (!is.null(few_shot_examples)) {
      if (!is.data.frame(few_shot_examples) || !all(c("text", "score") %in% colnames(few_shot_examples))) {
        stop("`few_shot_examples` must be a data frame with columns 'text' and 'score'.")
      }
      
      # Format the few-shot examples
      examples_text <- paste(
        "Here are some examples for scoring:",
        paste(
          apply(few_shot_examples, 1, function(row) {
            glue("Document: '{row['text']}' Score: {row['score']}")
          }),
          collapse = "\n\n"
        ),
        sep = "\n\n"
      )
      
      args <- c(args, list(system_prompt = paste(global_system_prompt, examples_text, sep = "\n\n")))
    } else {
      args <- c(args, list(system_prompt = global_system_prompt))
    }
  }
  
  # Add a default model if not provided and using chat_ollama
  if (!"model" %in% names(args) & identical(chat_fn, chat_openai)) {
    args <- c(args, list(model = "gpt-4o"))
  }
  
  type_score <- type_object(
    "Score of the document. DO NOT provide a score which is not in the specified range. 
    Provide only full numbers, no decimals or fractions", 
    score = type_number(paste(scale))
  )
  names <- names(.data)
  
  chat <- suppressMessages(do.call(chat_fn, args))
  model <- chat$get_model()
  
  if (verbose)
    cat("Calling", deparse(substitute(chat_fn)), "(", model, "):\n")
 
  
  result <- numeric(length(.data))
  for (i in seq_along(.data)) {
    if (verbose) {
      cat("... processing:", "[", i, "/", length(.data), "]", names[i], "\n")
    }
    
    if (i > 1)
      suppressMessages(chat <- do.call(chat_fn, args))
    data <- chat$chat_structured(.data[i], type = type_score)
    result[i] <- as.numeric(data$score)
  }
  
  if (verbose)
    message(glue("Finished."))
  
  names(result) <- names
  result
}