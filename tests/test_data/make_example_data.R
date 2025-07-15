library(quanteda)
library(quanteda.llm)

# scoring example
scores <- ai_score(data_corpus_inaugural[57:59], prompt,
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0, seed = 42))
saveRDS(scores, "ai_score_example.rds")

# salience example
salience <- ai_salience(data_corpus_inaugural[1:3],
                        chat_fn = chat_openai, model = "gpt-4o",
                        api_args = list(temperature = 0, seed = 42),
                        topics = c("economy", "environment", "healthcare"))
saveRDS(salience, "ai_salience_example.rds")

# summary example
summaries <- ai_summary(data_corpus_inaugural[1:3],
                        chat_fn = chat_openai, model = "gpt-4o",
                        api_args = list(temperature = 0, seed = 42))
saveRDS(summaries, "ai_summary_example.rds")

saveRDS(data_corpus_inaugural[1:3], "data_corpus_inaugural_1-3.rds")
saveRDS(data_corpus_inaugural[57:59], "data_corpus_inaugural_57_59.rds")
