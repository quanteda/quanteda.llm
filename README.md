
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quanteda.llm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/quanteda.llm)](https://CRAN.R-project.org/package=quanteda.llm)
[![R-CMD-check](https://github.com/quanteda/quanteda.llm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quanteda/quanteda.llm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **quanteda.llm** package makes it easy to use LLMs with
quanteda.corpora (or data frames), to enable classification,
summarisation, scoring, and analysis of documents and text. **quanteda**
provides a host of convenient functions for managing, manipulating, and
describing corpora as well as linking their document variables and
metadata to these documents. **quanteda.llm** makes it convenient to
link these to LLMs for analysing or classifying these texts, creating
new variables from what is created by the LLMs. Using a tidy approach
and linking to the new **quanteda.tidy** package, we enable convenient
operations using common Tidyverse functions for manipulating LLM-created
obejcts and variables.

# Included functions

The package includes the following functions:

- `ai_text()`:
  - A generic function that can be used with any LLM supported by
    `ellmer`.
  - Generates structured responses such as summaries, relevance scores,
    or classifications based on pre-defined instructions and scales for
    texts in a `quanteda corpus`.
  - Users can flexibly define prompts and structure of responses via
    `type_object()` from the [`ellmer`
    package](https://ellmer.tidyverse.org/articles/structured-data.html).
  - Users can add a dataset with examples to improve LLM performance
    (few-shot prompting)
- `ai_validate()`:
  - Starts an interactive app to manually validate the LLM-generated
    outputs.

# Supported LLMs

The package supports all LLMs currently available with the `ellmer`
package, including:

- Anthropic’s Claude: `chat_anthropic()`.
- AWS Bedrock: `chat_aws_bedrock()`.
- Azure OpenAI: `chat_azure_openai()`.
- Cloudflare: `chat_cloudflare()`.
- Databricks: `chat_databricks()`.
- DeepSeek: `chat_deepseek()`.
- GitHub model marketplace: `chat_github()`.
- Google Gemini/Vertex AI: `chat_google_gemini()`,
  `chat_google_vertex()`.
- Groq: `chat_groq()`.
- Hugging Face: `chat_huggingface()`.
- Mistral: `chat_mistral()`.
- Ollama: `chat_ollama()`.
- OpenAI: `chat_openai()`.
- OpenRouter: `chat_openrouter()`.
- perplexity.ai: `chat_perplexity()`.
- Snowflake Cortex: `chat_snowflake()` and `chat_cortex_analyst()`.
- VLLM: `chat_vllm()`.

For authentication and usage of each of these LLMs, please refer to the
respective `ellmer` documentation
[here](https://ellmer.tidyverse.org/reference/index.html). **For
example,** to use the `chat_openai` models, you would need to sign up
for an API key from
[OpenAI](https://platform.openai.com/playground/prompts) which you can
save in your `.Renviron` file as `OPENAI_API_KEY`. To use the
`chat_ollama` models, first download and install
[Ollama](https://ollama.com/). Then install some models either from the
command line (e.g. with ollama pull llama3.1) or within R using the
`rollama` package. The Ollama app must be running for the models to be
used.

## Installation

You can install the development version of quanteda.llm from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("quanteda/quanteda.llm")
```

## Examples

### Using `ai_text()` for summarization of documents

``` r
library(quanteda)
library(quanteda.llm)
#pak::pak("quanteda/quanteda.tidy")
library(quanteda.tidy)
corpus <- quanteda::data_corpus_inaugural %>%
  mutate(llm_sum = ai_text(text, chat_fn = chat_openai, model = "gpt-4o",
                           api_args = list(temperature = 0, seed = 42),
                           type_object = type_object("Summary of the document",
                           summary = type_string("Summarize the document in a few sentences."),
                           checkpoint_file = "llm_sum.RDS")))
# llm_sum is created as a new docvar in the corpus
# the checkpoint_file allow resuming the process in case of interruptions 
# without re-generating all responses
```

### Using `ai_text()` for classifying documents as per relevance of topics

``` r
library(quanteda)
library(quanteda.llm)
#pak::pak("quanteda/quanteda.tidy")
library(quanteda.tidy)
corpus <- quanteda::data_corpus_inaugural %>%
  mutate(llm_relevance = ai_text(text, chat_fn = chat_openai, model = "gpt-4o",
                                 api_args = list(temperature = 0, seed = 42),
                                 type_object = type_array("Array of classification results. The scores should sum to 1.",
                                 type_object(name = type_enum("The name of the topic", values = c("economy", "environment", "healthcare")),
                                 score = type_number("The score for the topic, between 0 and 1, sum to 1 across all topics"))),
                           checkpoint_file = "llm_relevance.RDS"))
# llm_relevance.name and llm_relevance.score is created as a new docvar in the corpus
# the checkpoint_file allow resuming the process in case of interruptions 
# without re-generating all responses
```

### Using `ai_text()` for scoring documents on a scale

``` r
library(quanteda)
library(quanteda.llm)
#pak::pak("quanteda/quanteda.tidy")
library(quanteda.tidy)
corpus <- quanteda::data_corpus_inaugural %>%
  mutate(llm_scale = ai_text(text, chat_fn = chat_openai, model = "gpt-4o",
                                 api_args = list(temperature = 0, seed = 42),
                                 type_object = type_object(
         "Scoring of documents on a scale of 0 to 3",
         score = type_integer("Score the following document on a scale of how much it aligns
         with the political left. The political left is defined as groups which 
         advocate for social equality, government intervention in the economy, 
         and progressive policies. Use the following metrics: 
         SCORING METRIC:
         3 : extremely left
         2 : very left
         1 : slightly left
         0 : not at all left"),
         evidence = type_string("Evidence supporting the score.")),
                           checkpoint_file = "llm_scale.RDS"))
# llm_scale.score and llm_scale.evidence is created as a new docvar in the corpus
# the checkpoint_file allow resuming the process in case of interruptions 
# without re-generating all responses
```

### Using `ai_validate()` to manually check LLM-generated outputs

``` r
library(quanteda)
library(quanteda.llm)
#pak::pak("quanteda/quanteda.tidy")
library(quanteda.tidy)
# Start the interactive app to manually check the LLM-generated outputs
corpus <- corpus %>%
  quanteda.tidy::mutate(validated = ai_validate(text, llm_scale.score))
# validated is created as a new docvar in the corpus with all non-validated scores set to NA
```
