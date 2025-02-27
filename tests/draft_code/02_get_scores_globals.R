###
### Importance functions
### Ken Benoit
### Feb 2025
###

## globals
role <- "You are a political expert tasked with reading a political party manifesto and assessing the importance of difference policy issues to the political party.  You should think carefully about your answer."
instructions <- "Based on the party's manifesto, rate the importance to the party of the following issues on a scale of 1 to 7, where 1 means that the policy issue is completely unimportant to the party and a 7 means that the issue is of the utmost importance to the party.  Your answer should be in the format of \"[Issue]: <score>. <rationale>\" where [Issue] is the name of the issue, <score> is the numeric rating and <rationale> is a brief explanation as to why you gave this answer."
issues <- c(Economic = "Taxation, spending on public services, and tradeoffs between these.",
            Social = "Social and lifestyle issues, including, abortion, LBTQ+ issues; support for traditional social values.",
            Immigration = "Immigration: immigration and border control",
            EU = "The European Union and European integration.",
            Environment = "Environmental protection: tradeoffs between protecting the environment and economic growth.",
            Decentralization = "Decentralization of political power and the role of regional governments.")
names(issues) <- toupper(names(issues))
all_issues <- paste(paste0(names(issues), ": ", issues), collapse = "\n")


## function to process a single manifesto
call_llm <- function(x, issues, chat_fn, model = NULL, api_args = list()) {
  type_importance <- type_object(
    party = type_string("The name of the political party that published the manifesto."),
    language = type_string("The language in which the manifesto is written."),
    scoring = type_string(paste(instructions, all_issues, x, sep = "\n\n")),
  )
  chat <- chat_fn(model = model, system_prompt = role, api_args = api_args)
  data <- chat$extract_data(x, type = type_importance)
  return(data)
}

## function to get all scores in a character vector
get_importance_vectorised <- function(x, chat_fn, model, api_args = list()) {
  results_list <- vector("list", length(x))
  for (i in seq_along(x)) {
    name <- names(x)[i]
    message(glue("Processing: [{i}/{length(x)}] {name}"))
    
    results_list[[i]] <- tryCatch(
      {
        this_result <- call_llm(
          x = data_corpus_manifs[i], 
          issues = issues,
          chat_fn = chat_fn,
          model = model,
          api_args = api_args
        )
        tibble(
          manifesto = name,
          party = this_result$party,
          error = "",
          scoring = this_result$scoring
        )
      },
      error = function(e) {
        message(glue("Error processing {name}: {e$message}"))
        tibble(
          manifesto = name,
          party = NA_character_,
          error = e$message,
          scoring = NA_character_
        )
      }
    )
  }

  results_df <- bind_rows(results_list)
  results_df <- results_df |>
    bind_cols(extract_scores(results_df$scoring))
  
  results_df
}

## function to extract scores from the text of the LLM output
library(purrr)
library(stringr)
library(tidyr)
extract_scores <- function(text) {
  issue_names <- text %>%
    str_extract_all("([A-Z]+): \\d+") %>%  # Extract all occurrences of ISSUE: NUMBER
    unlist() %>%
    str_extract("^[A-Z]+") %>%  # Extract just the issue name
    unique() |>
    na.omit() |>
    as.character()
  
  pattern <- paste0("(", paste(issue_names, collapse = "|"), "):\\s*(\\d+)")
  matches_list <- str_match_all(text, pattern)
  
  results_list <- lapply(matches_list, function(matches) {
    if (all(is.na(matches))) {
      return(setNames(as.list(rep(NA_real_, length(issue_names))), issue_names))
    }
    
    # convert extracted names and scores into a named list
    scores_list <- setNames(as.numeric(matches[, 3]), matches[, 2])
    
    # ensure all issue names exist in output
    full_scores <- setNames(rep(NA_real_, length(issue_names)), issue_names)
    full_scores[names(scores_list)] <- scores_list
    
    return(as.list(full_scores))
  })
  
  bind_rows(results_list)
}



## ---- outtakes

# ranking = type_string("Rank each issue in order of relative importance, where 1 is the most important issue and 6 is the least important.  Your answer should be in the format of \"[Issue]: <ranking>. <rationale>\" where [Issue] is the name of the issue, <ranking> is the integer ranking and <rationale> is a brief explanation as to why you gave this answer.")
# names(data$scoring) <- paste("Importance_Score", names(issues), sep = "_")
# names(data$ranking) <- paste("ranking", names(issues), sep = "_")
