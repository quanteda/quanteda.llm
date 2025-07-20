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

  if (!is.character(.data))
    stop("Unsupported data type for ai_text")

  if (length(.data) == 0)
    stop("No documents to process")

  if (!inherits(type_object, c("ellmer::TypeObject", "ellmer::TypeArray", "ellmer::Type"))) {
    stop("type_object must be created with ellmer::type_object() or related functions")
  }

  # capture additional arguments
  args <- rlang::list2(...)

  # Create or use existing environment
  if (is.null(result_env)) {
    result_env <- new.env()
    internal_env <- TRUE
  } else {
    internal_env <- FALSE
  }

  pb_id <- NULL

  tryCatch({
    if (is.null(names(.data)))
      names(.data) <- paste0("text", as.character(seq_along(.data)))
    original_order <- names(.data)

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

    current_doc <- NA_character_

    if (verbose && to_process > 0) {
      cli::cli_inform("Using {.fn {deparse(substitute(chat_fn))}} with model {.val {model}}")

      pb_id <- cli::cli_progress_bar(
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta} | {.file {current_doc}}",
        total = to_process,
        clear = FALSE,
        .envir = environment()
      )

      cli::cli_progress_update(id = pb_id, set = 0, force = TRUE)
    }

    processed_count <- 0

    # MAIN PROCESSING LOOP
    for (i in seq_along(.data)) {
      doc_id <- names(.data)[i]
      current_doc <- doc_id

      if (verbose && !is.null(pb_id)) {
        cli::cli_progress_update(
          id = pb_id,
          set = processed_count,
          force = TRUE,
          .envir = environment()
        )
      }

      if (exists(doc_id, envir = result_env)) {
        processed_count <- processed_count + 1
        next
      }

      if (i > 1) suppressMessages(chat <- do.call(chat_fn, args))

      tryCatch({
        data <- chat$chat_structured(.data[i], type = type_object)
        flat <- unlist(data, recursive = TRUE, use.names = TRUE)
        result_env[[doc_id]] <- as.data.frame(as.list(flat), stringsAsFactors = FALSE)

        # Pre-allocate AFTER first successful result
        if (!exists("df_results_full", envir = environment())) {
          template <- result_env[[doc_id]]

          df_results_full <- data.frame(
            id = original_order,
            matrix(NA, nrow = length(.data), ncol = ncol(template)),
            stringsAsFactors = FALSE
          )
          names(df_results_full) <- c("id", names(template))

          for (col in names(template)) {
            class(df_results_full[[col]]) <- class(template[[col]])
          }
        }

        # Fill the pre-allocated data frame
        if (exists("df_results_full", envir = environment())) {
          row_idx <- which(original_order == doc_id)
          for (col in names(flat)) {
            df_results_full[row_idx, col] <- flat[[col]]
          }
        }

        processed_count <- processed_count + 1

        if (verbose && !is.null(pb_id)) {
          cli::cli_progress_update(id = pb_id, set = processed_count)
        }
      }, error = function(e) {
        processed_count <- processed_count + 1

        if (verbose) {
          cli::cli_alert_danger("Failed to process document {.val {doc_id}}: {.emph {e$message}}")
          if (!is.null(pb_id)) {
            tryCatch(
              cli::cli_progress_update(id = pb_id, set = processed_count),
              error = function(e2) {}
            )
          }
        }
        warning(glue::glue("Skipping document {doc_id} due to error: {e$message}"))
      })
    }

    # AFTER THE LOOP - Construct final results
    if (exists("df_results_full", envir = environment())) {
      # Use the pre-allocated results
      df_results <- df_results_full
    } else if (length(result_env) > 0) {
      # Fallback method if pre-allocation didn't happen
      df_results <- data.frame(
        id = original_order,
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

      df_results <- df_results[match(original_order, df_results$id), ]
    } else {
      # No results at all
      df_results <- data.frame(
        id = original_order,
        stringsAsFactors = FALSE
      )
    }

    # Add attributes
    failed_docs <- setdiff(original_order, names(result_env))
    if (length(failed_docs) > 0) {
      attr(df_results, "failed_documents") <- failed_docs
    }

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

    if (grepl("parse error|premature EOF", e$message)) {
      cli::cli_alert_warning(c(
        "JSON parsing failed for {.val {doc_id}}",
        "!" = "Document length: {.val {nchar(.data[i])}} characters",
        "i" = "This often happens when documents exceed model token limits",
        ">" = "Consider: truncating text, using a model with larger context, or adjusting max_tokens"
      ))
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
