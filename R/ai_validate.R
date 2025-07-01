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
ai_validate <- function(text, llm_output, llm_evidence = NULL, result_env = new.env(), ..., verbose = TRUE) {
  library(shiny)
  library(jsonlite)
  
  # Ensure inputs are character vectors
  if (!is.character(text)) {
    stop("`text` must be a character vector.")
  }
  
  # Function to wrap text to a fixed width
  wrap_text <- function(text, width = 80) {
    sapply(text, function(x) paste(strwrap(x, width = width), collapse = "\n"))
  }
  
  # Wrap text to a fixed width
  text <- wrap_text(text, width = 80)
  
  # Initialize the validated results and manual extractions
  if (!exists("comments", envir = result_env)) {
    # Start fresh if no results exist in the environment
    result_env$comments <- rep("N/A", length(text))  # Start with "N/A"
    result_env$examples <- rep("", length(text))     # Start with empty strings
    result_env$status <- rep("Unmarked", length(text))  # Start with "Unmarked"
  }
  
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
            background-color: #95b8d1;
            color: #4d004d;
          }
          .title-panel {
            background-color: #95b8d1;
            padding: 10px;
            border-radius: 5px;
            color: #4d004d;
            text-align: center;
          }
          .sidebar {
            background-color: #809bce;
            padding: 15px;
            border-radius: 5px;
          }
          .main-panel {
            background-color: #95b8d1;
            padding: 15px;
            border-radius: 5px;
          }
          .btn {
            background-color: #95b8d1;
          }
          .btn:hover {
            opacity: 0.9;
          }
          .btn-correct {
            background-color: #d6eadf; 
          }
          .btn-correct:hover {
            background-color: #218838; /* Darker green */
          }
          .btn-wrong {
            background-color: #eac4d5; 
          }
          .btn-wrong:hover {
            background-color: #c82333; /* Darker red */
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
            width: auto; /* Adjust width dynamically */
            white-space: pre-wrap; /* Preserve line breaks */
            word-wrap: break-word; /* Break long words */
            overflow-wrap: break-word; /* Ensure long words break */
            font-family: 'Courier New', Courier, monospace;
            line-height: 1; /* Single-spacing */
            border: none; /* No border or frame */
          }
          .evidence-text {
            background-color: transparent; /* No background color */
            color: #4d004d;
            font-family: Arial, sans-serif;
            font-size: 14px;
            line-height: 1.5;
            padding: 0;
            margin: 0;
          }
        "))
      ),
      div(class = "title-panel", h1("Manual Validation")),
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar",
              h4("LLM Output:"),
              verbatimTextOutput("llm_output_1"),  # Fixed ID for first component
              conditionalPanel(
                condition = "output.llm_evidence !== null",
                div(class = "evidence-text", textOutput("llm_evidence"))  # Display evidence without a box
              ),
              actionButton("correct_btn", "Valid", class = "btn btn-correct"),  # Green button for correct
              actionButton("wrong_btn", "Invalid", class = "btn btn-wrong"),        # Red button for wrong
              textOutput("status_display"),                                      # Display the current status
              actionButton("prev_text", "Previous Text", class = "btn"),         # Button to move to the previous text
              actionButton("next_text", "Next Text", class = "btn"),             # Button to move to the next text
              textAreaInput("comments", "Comments:", "N/A", width = "100%", height = "100px"),  # Comments field starts with "N/A"
              textOutput("status"),
              h5("Highlighted examples:"),
              verbatimTextOutput("highlighted_text_display"),  # Display highlighted text
              h6("Examples highlighted in the text are automatically saved.")
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
      
      observe({
        llm_current <- llm_output[[current_index()]]
        
        output$llm_output_1 <- renderText({
          if (length(llm_current) >= 1) llm_current[1] else "N/A"
        })
        
      })
      
      # Render the evidence if `llm_evidence` is provided
      if (!is.null(llm_evidence)) {
        output$llm_evidence <- renderText({
          llm_evidence[[current_index()]]
        })
      }
      
      # Display the current status
      output$status_display <- renderText({
        paste("Status:", result_env$status[current_index()])
      })
      
      # Mark the current item as "Correct"
      observeEvent(input$correct_btn, {
        result_env$status[current_index()] <- "Valid"
        output$status_display <- renderText({
          paste("Status:", result_env$status[current_index()])
        })
      })
      
      # Mark the current item as "Wrong"
      observeEvent(input$wrong_btn, {
        result_env$status[current_index()] <- "Invalid"
        output$status_display <- renderText({
          paste("Status:", result_env$status[current_index()])
        })
      })
      
      # Update the comments field and highlighted examples when navigating
      observe({
        # Update the comments field
        updateTextAreaInput(session, "comments", value = result_env$comments[current_index()])
        
        # Update the displayed highlighted examples
        output$highlighted_text_display <- renderText({
          result_env$examples[current_index()]
        })
      })
      
      # Save comments and move to the next text
      observeEvent(input$next_text, {
        # Save the current comments
        result_env$comments[current_index()] <<- input$comments
        
        # Move to the next text
        if (current_index() < length(text)) {
          current_index(current_index() + 1)
          output$status <- renderText("Comments are saved by clicking previous/next text.")
        } else {
          output$status <- renderText("You are at the last text.")
        }
      })
      
      # Save comments and move to the previous text
      observeEvent(input$prev_text, {
        # Save the current comments
        result_env$comments[current_index()] <<- input$comments
        
        # Move to the previous text
        if (current_index() > 1) {
          current_index(current_index() - 1)
          output$status <- renderText("Comments are saved by clicking previous/next.")
        } else {
          output$status <- renderText("You are at the first text.")
        }
      })
    }
  )
  
  # Run the Shiny app
  runApp(app)
  
  if (verbose) {
    # Calculate the summary
    validated_count <- sum(result_env$comments != "N/A")
    remaining_count <- sum(result_env$comments == "N/A")
    
    message(sprintf(
      "Finished validation. %d texts validated, %d texts remaining.",
      validated_count,
      remaining_count
    ))
  }
  
  # Return a data frame without the text column
  return(data.frame(
    comments = result_env$comments,  # Single column for comments
    examples = result_env$examples, # Single column for highlighted examples
    status = result_env$status      # Single column for status
  ))
}