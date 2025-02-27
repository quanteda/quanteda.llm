###
### get importance scores from Gemini 1.5 Pro
### Kenneth Benoit
### Feb 2025
###

library(quanteda)
source("02_get_scores_globals.R")

load("../Data/Production data/data_corpus_manifs.rda")

results_df <- get_importance_vectorised(data_corpus_manifs,
                                        chat_fn = chat_gemini,
                                        model = "gemini-1.5-pro-latest",
                                        api_args = list(list(temperature = 0)))

saveRDS(results_df, file = "../Data/Production data/importance_scores_Gemini_1.5_pro.rds")




## -----------------------------------------------------------------------------------------------------------------------
redo_list <- results_df %>%
  filter(error != "") %>%
  pull(manifesto)
redo_list

results_df_redo <- bind_rows(results_redo_list)

results_df <- results_df %>%
  rows_update(results_df_redo, by = "manifesto")

