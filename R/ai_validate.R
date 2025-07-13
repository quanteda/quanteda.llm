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
#' summ1 <- ai_summarize(data_char_ukimmig2010, chat_fn = chat_ollama, model = "llama3.2")
#' summ2 <- ai_summarize(data_corpus_inaugural[1:2], chat_fn = chat_openai,
#'                 api_args = list(temperature = 0, seed = 42))
#' validate1 <- ai_validate(data_char_ukimmig2010, llm_output = summ1, verbose = TRUE)
#' validate2 <- ai_validate(data_corpus_inaugural[1:2], llm_output = summ2, verbose = TRUE)
#' }
#' @import shiny
#' @export
ai_validate <- function(text, llm_output, llm_evidence = NULL,
                        result_env = new.env(), ..., verbose = TRUE, launch_app = TRUE) {
  
  if (!is.character(text)) stop("`text` must be a character vector.")
  
  wrap_text <- function(text, width = 80) {
    sapply(text, function(x) paste(strwrap(x, width = width), collapse = "\n"))
  }
  
  text <- wrap_text(text, width = 80)
  
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
          shiny::tags$style(HTML("/* styles omitted for brevity */")),
          shiny::tags$script(HTML("/* JS omitted for brevity */"))
        ),
        div(class = "title-panel", shiny::h1("Manual Validation")),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            div(class = "sidebar",
                shiny::h4("LLM Output:"),
                shiny::verbatimTextOutput("llm_output_1"),
                shiny::conditionalPanel(
                  condition = "output.llm_evidence !== null",
                  div(class = "evidence-text", shiny::textOutput("llm_evidence"))
                ),
                shiny::actionButton("correct_btn", "Valid", class = "btn btn-correct"),
                shiny::actionButton("wrong_btn", "Invalid", class = "btn btn-wrong"),
                shiny::textOutput("status_display"),
                shiny::actionButton("prev_text", "Previous Text", class = "btn"),
                shiny::actionButton("next_text", "Next Text", class = "btn"),
                shiny::textAreaInput("comments", "Comments:", "N/A", width = "100%", height = "100px"),
                shiny::actionButton("save_highlight", "Save Highlight", class = "btn"),
                shiny::h5("Highlighted examples:"),
                shiny::verbatimTextOutput("highlighted_text_display"),
                shiny::h6("Examples highlighted in the text are automatically saved.")
            )
          ),
          shiny::mainPanel(
            div(class = "main-panel",
                shiny::h3("Text to Validate"),
                div(class = "text-box", shiny::verbatimTextOutput("text_display"))
            )
          )
        ),
        div(class = "footer",
            shiny::HTML("AI Validator made with \u2665,
                         <a href='https://shiny.posit.co/' target='_blank'>Shiny</a>,
                         and <a href='https://github.com/features/copilot' target='_blank'>quanteda.llm</a> -
                         remember, it is important to carefully check the output of LLMs.")
        )
      ),
      server = function(input, output, session) {
        current_index <- shiny::reactiveVal(1)
        
        output$text_display <- shiny::renderText({ text[current_index()] })
        
        observe({
          llm_current <- llm_output[[current_index()]]
          output$llm_output_1 <- shiny::renderText({
            if (length(llm_current) >= 1) llm_current[1] else "N/A"
          })
        })
        
        if (!is.null(llm_evidence)) {
          output$llm_evidence <- shiny::renderText({ llm_evidence[[current_index()]] })
        }
        
        output$status_display <- shiny::renderText({
          paste("Status:", result_env$status[current_index()])
        })
        
        shiny::observeEvent(input$correct_btn, {
          result_env$status[current_index()] <- "Valid"
          output$status_display <- shiny::renderText({
            paste("Status:", result_env$status[current_index()])
          })
        })
        
        shiny::observeEvent(input$wrong_btn, {
          result_env$status[current_index()] <- "Invalid"
          output$status_display <- shiny::renderText({
            paste("Status:", result_env$status[current_index()])
          })
        })
        
        shiny::observe({
          shiny::updateTextAreaInput(session, "comments", value = result_env$comments[current_index()])
          output$highlighted_text_display <- shiny::renderText({
            result_env$examples[current_index()]
          })
        })
        
        shiny::observeEvent(input$next_text, {
          result_env$comments[current_index()] <<- input$comments
          if (current_index() < length(text)) {
            current_index(current_index() + 1)
            output$status <- shiny::renderText("Comments are saved by clicking previous/next text.")
          } else {
            output$status <- shiny::renderText("You are at the last text.")
          }
        })
        
        shiny::observeEvent(input$prev_text, {
          result_env$comments[current_index()] <<- input$comments
          if (current_index() > 1) {
            current_index(current_index() - 1)
            output$status <- shiny::renderText("Comments are saved by clicking previous/next.")
          } else {
            output$status <- shiny::renderText("You are at the first text.")
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
    status = result_env$status
  ))
}
