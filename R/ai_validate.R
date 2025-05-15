#' Starts an interactive app to manually validate the
#' documents in a corpus which were summarized, labelled, or scored by an LLM.
#' 
#' The comments from manual validation for each text are added as an additional docvar to the input data
#' with all texts not yet validated as "N/A".
#' @param .data a character or [quanteda::corpus] object containing the
#'   documents to be manually validated
#' @param llm_output a character string; the name of the LLM output column which contains
#' the summaries, labels, or scores to be validated
#' @param verbose logical; output a progress indicator if `TRUE`
#' @return character; the response from the manual validation with a length equal to the
#'   number of input documents, with the elements named with the input element
#'   names
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
#' @export
ai_validate <- function(text, llm_output, ..., verbose = TRUE) {
  library(shiny)
  
  # Ensure inputs are character vectors
  if (!is.character(text)) {
    stop("`text` must be a character vector.")
  }
  
  # Convert `llm_output` to a character vector for display
  llm_output <- as.character(llm_output)
  
  # Function to wrap text to a fixed width
  wrap_text <- function(text, width = 80) {
    sapply(text, function(x) paste(strwrap(x, width = width), collapse = "\n"))
  }
  
  # Wrap text and llm_output to a fixed width
  text <- wrap_text(text, width = 80)
  llm_output <- wrap_text(llm_output, width = 80)
  
  # Initialize the validated results as a regular character vector
  validated_results <- rep("N/A", length(text))  # Default to "N/A"
  
  if (verbose) {
    message("Launching Shiny app for manual validation...")
  }
  
  # Define the Shiny app
  app <- shinyApp(
    ui = fluidPage(
      # Add custom CSS for text formatting
      tags$head(
        tags$style(HTML("
          body {
            background-color: #ffccd5;
            color: #4d004d;
          }
          .title-panel {
            background-color: #ff99aa;
            padding: 10px;
            border-radius: 5px;
            color: #4d004d;
            text-align: center;
          }
          .sidebar {
            background-color: #ffb3c6;
            padding: 15px;
            border-radius: 5px;
          }
          .main-panel {
            background-color: #ffe6eb;
            padding: 15px;
            border-radius: 5px;
          }
          .btn {
            background-color: #ff6680;
            color: white;
            border: none;
          }
          .btn:hover {
            background-color: #ff3366;
          }
          textarea {
            background-color: #ffccd5;
            color: #4d004d;
            border: 1px solid #ff99aa;
          }
          .footer {
            text-align: center;
            margin-top: 20px;
            font-size: 12px;
            color: #4d004d;
          }
          .footer a {
            color: #4d004d;
            text-decoration: underline;
          }
          .footer a:hover {
            color: #ff3366;
          }
          .text-box {
            background-color: transparent; /* No background color */
            color: #4d004d;
            padding: 10px;
            height: auto; /* Adjust height dynamically */
            width: 800px; /* Fixed width */
            white-space: pre-wrap; /* Preserve line breaks */
            word-wrap: break-word; /* Break long words */
            overflow-wrap: break-word; /* Ensure long words break */
            font-family: 'Courier New', Courier, monospace;
            line-height: 1; /* Single-spacing */
            border: none; /* No border or frame */
          }
        "))
      ),
      div(class = "title-panel", h1("Manual Validation")),
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar",
              h4("LLM Output:"),
              textOutput("llm_output_sidebar"),  # Dynamically display the current LLM output
              actionButton("next_text", "Next Text", class = "btn"),  # Button to move to the next text
              textAreaInput("comments", "Comments:", "", width = "100%", height = "100px"),  # Comments field
              textOutput("status")
          )
        ),
        mainPanel(
          div(class = "main-panel",
              h3("Text to Validate"),
              div(class = "text-box", verbatimTextOutput("text_display"))  # Styled text box for displaying the text
          )
        )
      ),
      div(class = "footer",
          HTML("AI Validator made with ♥, 
               <a href='https://shiny.posit.co/' target='_blank'>Shiny</a>, 
               and <a href='https://github.com/features/copilot' target='_blank'>quanteda.llm</a> - 
               remember, it is important to carefully check the output of LLMs.")
      )
    ),
    server = function(input, output, session) {
      current_index <- reactiveVal(1)
      
      # Display the current text
      output$text_display <- renderText({
        text[current_index()]
      })
      
      # Dynamically display the current LLM output in the sidebar
      output$llm_output_sidebar <- renderText({
        llm_output[current_index()]
      })
      
      # Automatically save the comments and move to the next text
      observeEvent(input$next_text, {
        # Save the current comments
        validated_results[current_index()] <<- input$comments
        
        # Move to the next text
        if (current_index() < length(text)) {
          current_index(current_index() + 1)
          updateTextAreaInput(session, "comments", value = validated_results[current_index()])
          output$status <- renderText("Comments saved and moved to the next text.")
        } else {
          output$status <- renderText("No more texts to validate.")
        }
      })
    }
  )
  
  # Run the Shiny app
  runApp(app)
  
  if (verbose) {
    message("Finished validation.")
  }
  
  # Return the validated results
  return(validated_results)
}