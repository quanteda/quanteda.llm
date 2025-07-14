#' Starts an interactive app to manually validate the output of an LLM
#' stored in a character vector
#'
#' This function launches a Shiny app that allows users to manually validate
#' the output of a LLM analysis, such as ai_score
#' The comments from manual validation for each text are added as an additional
#' docvar to the input data with all texts not yet validated as `NA`.
#' @param text a character or [quanteda::corpus] object containing the
#'   documents to be manually validated
#' @param llm_output a character string; the name of the LLM output column which
#'   contains the summaries, labels, or scores to be validated
#' @param llm_evidence a character vector; the name of an additional LLM output
#' such as evidence or justifications provided by the LLM
#' @param verbose logical; output a progress indicator if `TRUE`
#' @param launch_app Logical, whether to launch the interactive Shiny app. Defaults to TRUE.
#' @inheritParams ai_text
#' @return character; the response from the manual validation with a length
#'   equal to the number of input documents, with the elements named with the
#'   input element names
#' @name ai_validate
#' @import shiny
#' @examples
#' \dontrun{
#' library(quanteda)
#' summ1 <- ai_summary(data_char_ukimmig2010, chat_fn = chat_ollama, model = "llama3.2")
#' summ2 <- ai_summary(data_corpus_inaugural[1:2], chat_fn = chat_openai,
#'                 api_args = list(temperature = 0, seed = 42))
#' validate1 <- ai_validate(data_char_ukimmig2010, llm_output = summ1, verbose = TRUE)
#' validate2 <- ai_validate(data_corpus_inaugural[1:2], llm_output = summ2, verbose = TRUE)
#' }
#' @import shiny
#' @export
ai_validate <- function(text, llm_output, llm_evidence = NULL,
                        result_env = new.env(), ..., verbose = TRUE, launch_app = TRUE) {

  if (!is.character(text)) {
    stop("`text` must be a character vector.")
  }

  # if (!is.data.frame(llm_output)) {
  #   stop("`llm_output` must be a data frame")
  # }

  # Get the names from the text vector
  text_names <- names(text)
  if (is.null(text_names)) {
    text_names <- as.character(seq_along(text))
  }

  # Match the order of llm_output to text
  if ("id" %in% names(llm_output)) {
    # Reorder llm_output to match text order
    llm_output <- llm_output[match(text_names, llm_output$id), ]

    # Check for missing matches
    if (any(is.na(llm_output$id))) {
      stop("Some text names don't match with llm_output ids")
    }
  } else {
    stop("llm_output must have an 'id' column")
  }

  if (!exists("comments", envir = result_env)) {
    result_env$comments <- rep("N/A", length(text))
    result_env$examples <- rep("", length(text))
    result_env$status <- rep("Unmarked", length(text))
  }

  if (verbose) {
    message("Launching Shiny app for manual validation...")
  }

  if (launch_app) {
    app <- shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::tags$head(
          shiny::tags$style(HTML("
      /* Global style for the pre element with text_display id */
      #text_display {
        white-space: pre-wrap !important;
        word-wrap: break-word !important;
        overflow-wrap: anywhere !important;
        width: 100% !important;
        overflow: hidden !important;
        font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
        font-size: 14px;
        line-height: 1.5;
        margin: 0;
        padding: 0;
      }

      .text-box {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        max-height: 500px;
        overflow-y: auto;
        overflow-x: hidden !important;
        border: 1px solid #dee2e6;
        width: 100%;
        box-sizing: border-box;
      }

      /* Ensure main panel doesn't create overflow */
      .main-panel {
        padding: 15px;
        max-width: 100%;
        overflow-x: hidden;
      }
    "))
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::div(class = "sidebar",
                       shiny::h4(shiny::textOutput("navigation_info")),
                       shiny::tags$div(
                         class = "document-info",
                         shiny::tags$strong("Document: "),
                         shiny::textOutput("document_name", inline = TRUE)
                       ),
                       shiny::hr(),
                       shiny::h4("LLM Output:"),
                       shiny::div(class = "text-box", shiny::textOutput("text_display")),
                       shiny::conditionalPanel(
                         condition = "output.has_evidence",
                         shiny::div(class = "evidence-text",
                                    shiny::h5("Evidence:"),
                                    shiny::textOutput("llm_evidence")
                         )
                       ),
                       shiny::hr(),
                       shiny::fluidRow(
                         shiny::column(6, shiny::actionButton("correct_btn", "Valid",
                                                              class = "btn btn-correct", width = "100%")),
                         shiny::column(6, shiny::actionButton("wrong_btn", "Invalid",
                                                              class = "btn btn-wrong", width = "100%"))
                       ),
                       shiny::br(),
                       shiny::textOutput("status_display"),
                       shiny::hr(),
                       shiny::fluidRow(
                         shiny::column(6, shiny::actionButton("prev_text", "Previous",
                                                              class = "btn btn-secondary", width = "100%")),
                         shiny::column(6, shiny::actionButton("next_text", "Next",
                                                              class = "btn btn-secondary", width = "100%"))
                       ),
                       shiny::br(),
                       shiny::textAreaInput("comments", "Comments:", "N/A", width = "100%", height = "100px"),
                       shiny::actionButton("save_highlight", "Save Highlight", class = "btn btn-info"),
                       shiny::h5("Highlighted examples:"),
                       shiny::verbatimTextOutput("highlighted_text_display"),
                       shiny::p("Examples highlighted in the text are automatically saved.",
                                style = "font-size: 0.9em; color: #6c757d;"),
                       shiny::textOutput("navigation_status")
            )
          ),
          shiny::mainPanel(
            shiny::div(class = "main-panel",
                       shiny::h3("Text to Validate"),
                       shiny::div(class = "text-box", shiny::verbatimTextOutput("text_display"))
            )
          )
        ),
        shiny::div(class = "footer",
                   shiny::HTML("AI Validator made with ❤️,
                         <a href='https://shiny.posit.co/' target='_blank'>Shiny</a>,
                         and <a href='https://github.com/quanteda/quanteda.llm' target='_blank'>quanteda.llm</a> -
                         remember, it is important to carefully check the output of LLMs.")
        )
      ),
      server = function(input, output, session) {
        current_index <- shiny::reactiveVal(1)
        navigation_message <- shiny::reactiveVal("")

        # Navigation info display
        output$navigation_info <- shiny::renderText({
          paste("Document", current_index(), "of", length(text))
        })

        # document name display
        output$document_name <- shiny::renderText({
          text_names[current_index()]
        })

        # For conditional panel
        output$has_evidence <- shiny::reactive({
          !is.null(llm_evidence)
        })
        shiny::outputOptions(output, "has_evidence", suspendWhenHidden = FALSE)

        # Navigation status messages
        output$navigation_status <- shiny::renderText({
          navigation_message()
        })

        output$text_display <- shiny::renderText({
          text[current_index()]
        })

        # Display LLM output
        shiny::observe({
          # Get the current row from llm_output
          current_row <- llm_output[current_index(), ]

          # Display the summary (or whatever column you want to validate)
          output$llm_output_1 <- shiny::renderText({
            if (!is.null(current_row$summary)) {
              current_row$summary
            } else {
              # If there's no 'summary' column, display all non-id columns
              cols_to_show <- setdiff(names(current_row), "id")
              if (length(cols_to_show) > 0) {
                paste(current_row[cols_to_show], collapse = "\n")
              } else {
                "N/A"
              }
            }
          })
        })

        # Display evidence if provided
        if (!is.null(llm_evidence)) {
          shiny::observe({
            output$llm_evidence <- shiny::renderText({
              if (is.data.frame(llm_evidence)) {
                evidence_row <- llm_evidence[current_index(), ]
                if ("evidence" %in% names(evidence_row)) {
                  evidence_row$evidence
                } else {
                  cols_to_show <- setdiff(names(evidence_row), "id")
                  if (length(cols_to_show) > 0) {
                    evidence_row[[cols_to_show[1]]]
                  } else {
                    "N/A"
                  }
                }
              } else if (is.character(llm_evidence)) {
                llm_evidence[current_index()]
              } else {
                "N/A"
              }
            })
          })
        }

        output$status_display <- shiny::renderText({
          paste("Status:", result_env$status[current_index()])
        })

        shiny::observeEvent(input$correct_btn, {
          result_env$status[current_index()] <- "Valid"
        })

        shiny::observeEvent(input$wrong_btn, {
          result_env$status[current_index()] <- "Invalid"
        })

        shiny::observe({
          shiny::updateTextAreaInput(session, "comments",
                                     value = result_env$comments[current_index()])
          output$highlighted_text_display <- shiny::renderText({
            result_env$examples[current_index()]
          })
        })

        shiny::observeEvent(input$next_text, {
          current_comments <- input$comments

          if (current_index() < length(text)) {
            # Save comments if changed
            if (current_comments != result_env$comments[current_index()]) {
              result_env$comments[current_index()] <<- current_comments
              shiny::showNotification("Comments saved", type = "message", duration = 2)
            }

            current_index(current_index() + 1)
          } else {
            # Always save on the last document if trying to go next
            if (current_comments != result_env$comments[current_index()]) {
              result_env$comments[current_index()] <<- current_comments
            }
            shiny::showNotification(
              "You are at the last document",
              type = "warning",
              duration = 3
            )
          }
        })

        shiny::observeEvent(input$prev_text, {
          result_env$comments[current_index()] <<- input$comments

          if (current_index() > 1) {
            current_index(current_index() - 1)
            shiny::showNotification("Comments saved", type = "message", duration = 2)
          } else {
            shiny::showNotification(
              "You are at the first document",
              type = "warning",
              duration = 3
            )
          }
        })

        shiny::observeEvent(input$save_highlight, {
          session$sendCustomMessage('getSelectedText', 'highlighted_text')
        })

        shiny::observeEvent(input$highlighted_text, {
          if (nzchar(input$highlighted_text)) {
            result_env$examples[current_index()] <- input$highlighted_text
          }
        })
      }
    )

    shiny::runApp(app)
  }

  if (verbose) {
    validated_count <- sum(result_env$comments != "N/A")
    remaining_count <- sum(result_env$comments == "N/A")

    message(sprintf(
      "Finished validation. %d texts validated, %d texts remaining.",
      validated_count,
      remaining_count
    ))
  }

  return(data.frame(
    comments = result_env$comments,
    examples = result_env$examples,
    status = result_env$status,
    stringsAsFactors = FALSE
  ))
}
