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
ai_validate <- function(text, llm_output, save_file = "validation_progress.rds", ..., verbose = TRUE) {
  library(shiny)
  library(jsonlite)
  
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
  
  # Initialize the validated results and manual extractions
  if (file.exists(save_file)) {
    # Load progress if the save file exists
    progress <- readRDS(save_file)
    validated_results <- progress$comments
    examples <- progress$examples
    if (verbose) message("Loaded progress from file.")
  } else {
    # Start fresh if no save file exists
    validated_results <- rep("N/A", length(text))  # Start with "N/A"
    examples <- rep("", length(text))              # Start with empty strings
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
            width: auto; /* Adjust height dynamically */
            white-space: pre-wrap; /* Preserve line breaks */
            word-wrap: break-word; /* Break long words */
            overflow-wrap: break-word; /* Ensure long words break */
            font-family: 'Courier New', Courier, monospace;
            line-height: 1; /* Single-spacing */
            border: none; /* No border or frame */
          }
        ")),
        # Add JavaScript to capture highlighted text only in the main text area
        tags$script(HTML("
          document.addEventListener('mouseup', function(event) {
            var selectedText = '';
            // Check if the selection is within the main text area
            if (event.target.closest('.text-box')) {
              selectedText = window.getSelection().toString();
            }
            if (selectedText.length > 0) {
              Shiny.setInputValue('highlighted_text', selectedText, {priority: 'event'});
            }
          });
        "))
      ),
      div(class = "title-panel", h1("Manual Validation")),
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar",
              h4("LLM Output:"),
              textOutput("llm_output_sidebar"),  # Dynamically display the current LLM output
              actionButton("prev_text", "Previous Text", class = "btn"),  # Button to move to the previous text
              actionButton("next_text", "Next Text", class = "btn"),  # Button to move to the next text
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
      
      # Dynamically display the current LLM output in the sidebar
      output$llm_output_sidebar <- renderText({
        llm_output[current_index()]
      })
      
      # Update the comments field and highlighted examples when navigating
      observe({
        # Update the comments field
        updateTextAreaInput(session, "comments", value = validated_results[current_index()])
        
        # Update the displayed highlighted examples
        output$highlighted_text_display <- renderText({
          examples[current_index()]
        })
      })
      
      # Save highlighted text (append to existing highlights)
      observeEvent(input$highlighted_text, {
        # Append the new highlight to the existing highlights
        examples[current_index()] <<- paste(
          examples[current_index()],
          input$highlighted_text,
          sep = " | "
        )
        # Update the displayed highlighted text
        output$highlighted_text_display <- renderText({
          examples[current_index()]
        })
      })
      
      # Save comments and move to the next text
      observeEvent(input$next_text, {
        # Save the current comments
        validated_results[current_index()] <<- input$comments
        
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
        validated_results[current_index()] <<- input$comments
        
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
    validated_count <- sum(validated_results != "N/A")
    remaining_count <- sum(validated_results == "N/A")
    
    # Save progress to file
    saveRDS(list(comments = validated_results, examples = examples), save_file)
    message(sprintf(
      "Finished validation. %d texts validated, %d texts remaining. Progress saved to '%s'.",
      validated_count,
      remaining_count,
      save_file
    ))
  }
  
  # Return a data frame without the text column
  return(data.frame(
    comments = validated_results,  # Single column for comments
    examples = examples            # Single column for highlighted examples
  ))
}