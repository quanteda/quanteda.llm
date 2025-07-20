#' @importFrom rlang .data
NULL

## globals

context_max <- list(
  "GPT-3" = 2048,
  "GPT-3.5-turbo" = 4096,
  "GPT-4" = 8192,
  "BERT" = 512,
  "RoBERTa" = 512,
  "T5" = 512,
  "XLNet" = 512,
  "ALBERT" = 512,
  "llama3.2" = 2048
)

global_system_prompt = "You are an expert tasked with reading the supplied documents carefully and objectively."
