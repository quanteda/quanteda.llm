#' Structured AI analysis of texts
#'
#' This function applies AI-assisted analysis to each document in a character
#' vector or a docvar in a [corpus][quanteda::corpus()], using a structured
#' [type_object()][ellmer::type_object()] to define the expected response format. It supports
#' few-shot learning examples for better performance.
#'
#' @param .data A character vector of texts
#' @param chat_fn A function like [chat_openai()][ellmer::chat_openai()]. See
#' <https://ellmer.tidyverse.org/articles/structured-data.html> for details.
#' @param ... additional arguments passed to `chat_fn`
#' @param few_shot_examples Optional few-shot learning examples (data frame with
#'   `text`, `score`)
#' @param type_object an \pkg{ellmer} [type_object()][ellmer::type_object()]
#' @param verbose logical; whether to print progress
#' @param result_env An environment to store results and allow resuming
#' @return character; the response from the LLM with a length equal to the
#'   number of input documents; each single element defined by
#'   [type_object()][ellmer::type_object()] is added as a character vector
#'
#' @examples
#' \dontrun{
#' library(quanteda)
#' results <- quanteda::data_corpus_inaugural[1:3] %>%
#'   ai_text(chat_fn = chat_openai, model = "gpt-4o",
#'           api_args = list(temperature = 0, seed = 42),
#'           type_object =
#'             type_object("Summary of the document",
#'                         summary = type_string("Summarize the document in a few sentences.")),
#'   )
#' }
#' @importFrom glue glue
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @export
ai_text <- function(.data, chat_fn, type_object, few_shot_examples = NULL,
                    verbose = TRUE, result_env = NULL, ...) {

  # Create or use existing environment
  if (is.null(result_env)) {
    result_env <- new.env()
    internal_env <- TRUE  # Flag to know we created it
  } else {
    internal_env <- FALSE
  }

  pb_id <- NULL

  tryCatch({

    if (!is.character(.data))
      stop("Unsupported data type for ai_text")

    args <- rlang::list2(...)

    if (is.null(names(.data)))
      names(.data) <- paste0("text", as.character(seq_along(.data)))
    original_order <- names(.data)

    df_results <- data.frame(
      id = original_order,
      stringsAsFactors = FALSE
    )

    # Set up system prompt
    if (!"system_prompt" %in% names(args)) {
      if (!is.null(few_shot_examples)) {
        if (!is.data.frame(few_shot_examples) || !all(c("text", "score") %in% colnames(few_shot_examples))) {
          stop("`few_shot_examples` must be a data frame with columns 'text' and 'score'.")
        }

        examples_text <- paste(
          "Here are some examples for scoring:",
          paste(
            apply(few_shot_examples, 1, function(row) {
              glue::glue("Document: '{row[['text']]}' Score: {row[['score']]}")
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

    chat <- suppressMessages(do.call(chat_fn, args))
    model <- chat$get_model()

    # Count already processed documents
    already_processed <- sum(names(.data) %in% names(result_env))
    total_docs <- length(.data)
    to_process <- total_docs - already_processed

    current_doc <- NA_character_  # Needed for progress bar format

    if (verbose && to_process > 0) {
      cli::cli_inform("Using {.fn {deparse(substitute(chat_fn))}} with model {.val {model}}")

      # Store the progress bar ID
      pb_id <- cli::cli_progress_bar(
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta} | {.file {current_doc}}",
        total = to_process,
        clear = FALSE,
        .envir = environment()
      )
    }

    # Track actual progress separately
    processed_count <- 0

    for (i in seq_along(.data)) {
      doc_id <- names(.data)[i]
      current_doc <- doc_id

      if (exists(doc_id, envir = result_env)) {
        processed_count <- processed_count + 1
        if (verbose && !is.null(pb_id)) {
          cli::cli_progress_update(id = pb_id, set = processed_count)
        }
        next
      }

      if (verbose && !is.null(pb_id)) {
        cli::cli_progress_update(id = pb_id, set = processed_count + 0.5)
      }

      if (i > 1) suppressMessages(chat <- do.call(chat_fn, args))

      tryCatch({
        data <- chat$chat_structured(.data[i], type = type_object)
        flat <- unlist(data, recursive = TRUE, use.names = TRUE)
        result_env[[doc_id]] <- as.data.frame(as.list(flat), stringsAsFactors = FALSE)

        processed_count <- processed_count + 1

        if (verbose && !is.null(pb_id)) {
          cli::cli_progress_update(id = pb_id, set = processed_count)
        }
      }, error = function(e) {
        processed_count <- processed_count + 1  # Still increment

        if (verbose) {
          cli::cli_alert_danger("Failed to process document {.val {doc_id}}: {.emph {e$message}}")
          if (!is.null(pb_id)) {
            # Safe progress update with error handling
            tryCatch(
              cli::cli_progress_update(id = pb_id, set = processed_count),
              error = function(e2) {} # Ignore progress bar errors
            )
          }
        }
        warning(glue::glue("Skipping document {doc_id} due to error: {e$message}"))
      })
    }

    if (length(result_env) > 0) {
      # Build processed results
      processed_list <- lapply(names(result_env), function(doc_id) {
        cbind(id = doc_id, result_env[[doc_id]], stringsAsFactors = FALSE)
      })
      processed_df <- do.call(rbind, processed_list)

      # Left join to preserve all documents
      df_results <- merge(
        df_results,
        processed_df,
        by = "id",
        all.x = TRUE,  # Keep all original documents
        sort = FALSE   # Preserve order
      )

      # Ensure original order is maintained
      df_results <- df_results[match(original_order, df_results$id), ]
    }

    # Add attribute to indicate which documents failed
    failed_docs <- setdiff(original_order, names(result_env))
    if (length(failed_docs) > 0) {
      attr(df_results, "failed_documents") <- failed_docs
    }

    # Your empty response checking code stays here...

    if (verbose) {
      successful_count <- sum(df_results$id %in% names(result_env))
      cli::cli_alert_success(
        "Returned {.val {nrow(df_results)}} documents ({.val {successful_count}} successful, {.val {length(failed_docs)}} with NAs)"
      )
    }

    return(df_results)

  }, error = function(e) {
    # Clean up progress bar before handling error
    if (verbose && !is.null(pb_id)) {
      tryCatch(
        cli::cli_progress_done(id = pb_id),
        error = function(e2) {} # Ignore if already closed
      )
    }

    if (verbose) {
      cli::cli_alert_danger("Processing interrupted: {.emph {e$message}}")
      actual_processed <- length(ls(result_env))
      cli::cli_alert_info("Returning {.val {actual_processed}} successfully processed documents")
    }

    # Reconstruct partial results
    df_list <- list()
    for (doc_id in names(result_env)) {
      df_list[[doc_id]] <- cbind(id = doc_id, result_env[[doc_id]],
                                 stringsAsFactors = FALSE)
    }

    if (length(result_env) > 0) {
      # Pre-allocate with available names
      df_results <- data.frame(
        id = names(.data),  # Use .data names since original_order might not exist
        stringsAsFactors = FALSE
      )

      processed_list <- lapply(names(result_env), function(doc_id) {
        cbind(id = doc_id, result_env[[doc_id]], stringsAsFactors = FALSE)
      })
      processed_df <- do.call(rbind, processed_list)

      df_results <- merge(
        df_results,
        processed_df,
        by = "id",
        all.x = TRUE,
        sort = FALSE
      )

      attr(df_results, "partial_results") <- TRUE
      attr(df_results, "processed_ids") <- names(result_env)
      attr(df_results, "failed_documents") <- setdiff(names(.data), names(result_env))

      return(df_results)
    } else {
      stop(e)
    }
  })
}
