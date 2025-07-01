
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
  - Generates structured responses or classifications based on
    pre-defined instructions for texts in a `quanteda corpus`.
  - Users can flexibly define prompts and structure of responses via
    `type_object()` from the [`ellmer`
    package](https://ellmer.tidyverse.org/articles/structured-data.html).
  - Users can add a dataset with examples to improve LLM performance
    (few-shot prompting)
  - Supports resuming interrupted processes in a `result_env`
    environment.
- `ai_validate()`:
  - Starts an interactive app to manually validate the LLM-generated
    outputs.
  - Allows users to review and validate the LLM-generated outputs and
    justifications, marking them as valid or invalid.
  - Supports resuming the validation process in case of interruptions in
    a `result_env` environment.
- `ai_summary()`:
  - A wrapper around `ai_text()` for summarizing documents in a corpus.
  - Uses a pre-defined `type_object()` to structure the summary output.
- `ai_salience()`:
  - A wrapper around `ai_text()` for computing salience scores for
    topics in a corpus.
  - Uses a pre-defined `type_object()` to structure the salience
    classification output.
- `ai_score()`:
  - A wrapper around `ai_text()` for scoring documents based on a scale
    defined by a prompt.
  - Uses a pre-defined `type_object()` to structure the scoring output.

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

You can install the development version of **quanteda.llm** from
<https://github.com/quanteda/quanteda.llm> with:

``` r
# install.packages("pak")
pak::pak("quanteda/quanteda.llm")
pak::pak("quanteda/quanteda.tidy")
```

``` r
library(quanteda)
#> Package version: 4.3.0
#> Unicode version: 14.0
#> ICU version: 71.1
#> Parallel computing: 10 of 10 threads used.
#> See https://quanteda.io for tutorials and examples.
library(quanteda.llm)
#> Loading required package: ellmer
library(quanteda.tidy)
#> 
#> Attaching package: 'quanteda.tidy'
#> The following object is masked from 'package:stats':
#> 
#>     filter

data_corpus_inaugural <- tail(data_corpus_inaugural, 3)
```

## Examples

### Using `ai_summary()` for summarization of documents

``` r
# creates a new docvar "summary"
data_corpus_inaugural <- data_corpus_inaugural %>%
  mutate(ai_summary(text, chat_fn = chat_openai, model = "gpt-4o",
                    api_args = list(temperature = 0, seed = 42)))
#> 
#> Calling chat_fn (gpt-4o):
#> ... processing: [1/3]
#> ... processing: [2/3]
#> ... processing: [3/3]
#> Finished.

library(kableExtra)
cbind(data.frame(docnames = docnames(data_corpus_inaugural)), 
      summary = data_corpus_inaugural$summary) %>%
  kable("html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(2)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

docnames
</th>

<th style="text-align:left;">

summary
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

2017-Trump
</td>

<td style="text-align:left;">

This document is a speech that emphasizes the transition of power in the
United States, with a focus on returning control from Washington, D.C.
to the people. The speaker criticizes the political establishment for
prospering while American citizens have faced economic hardships. The
speech outlines a vision for revitalizing the nation by prioritizing
American interests, enhancing job opportunities, and rebuilding
infrastructure. Key themes include unity, patriotism, American
exceptionalism, and a focus on domestic challenges such as poverty,
crime, and unemployment. The speech also stresses rebuilding
international relationships based on mutual respect and prioritizing
America’s protection and prosperity.
</td>

</tr>

<tr>

<td style="text-align:left;">

2025-Trump
</td>

<td style="text-align:left;">

This speech, delivered at an inauguration, emphasizes the themes of
unity, resilience, and the importance of democracy. The speaker
acknowledges the challenges the country faces, including a pandemic,
racial injustice, and political divisions, but stresses the need for
unity and collaboration to overcome these obstacles. He calls for an end
to division and an embrace of common goals such as dignity, respect, and
truth. The speech includes a dedication to serve all citizens and a
commitment to uphold democratic principles, urging Americans to work
together to heal and advance the nation.
</td>

</tr>

<tr>

<td style="text-align:left;">

2021-Biden.txt
</td>

<td style="text-align:left;">

This document is a speech on the theme of democracy and unity in
America, likely delivered during a significant political occasion like
an inauguration. The speaker praises the resilience of American
democracy, highlighting recent challenges such as political extremism, a
pandemic, and social injustices while emphasizing the importance of
unity to overcome these difficulties. The speaker calls for Americans to
come together beyond political divides and address issues like racial
justice, climate change, and economic recovery. It’s a hopeful message
with references to historical precedents of unity and progress in
America, invoking a sense of duty and collective responsibility to
preserve and advance democracy.
</td>

</tr>

</tbody>

</table>

### Using `ai_salience()` for salience rating of topics

``` r
# define the topics for salience classification
topics <- c("economy", "environment", "healthcare")
data_corpus_inaugural <- data_corpus_inaugural %>%
  mutate(ai_salience(text, topics, chat_fn = chat_openai, model = "gpt-4o",
                     api_args = list(temperature = 0, seed = 42)))
#> 
#> Calling chat_fn (gpt-4o):
#> ... processing: [1/3]
#> ... processing: [2/3]
#> ... processing: [3/3]
#> Finished.

# topic and score are created as new docvars in the corpus
glimpse(data_corpus_inaugural)
#> Rows: 3
#> Columns: 14
#> $ doc_id    <chr> "2017-Trump", "2025-Trump", "2021-Biden.txt"
#> $ text      <chr> "Chief Just…", "Chief Just…", "Chief Just…"
#> $ Year      <int> 2017, 2021, 2025
#> $ President <chr> "Trump", "Biden", "Trump"
#> $ FirstName <chr> "Donald J.", "Joseph R.", "Donald J."
#> $ Party     <fct> Republican, Democratic, Republican
#> $ id        <chr> "1", "2", "3"
#> $ summary   <chr> "This document is a speech that emphasizes the transition of…
#> $ topic1    <dbl> 1, 3, 1
#> $ topic2    <dbl> 2, 2, 2
#> $ topic3    <dbl> 3, 1, 3
#> $ score1    <dbl> 0.65, 0.15, 0.05
#> $ score2    <dbl> 0.10, 0.15, 0.15
#> $ score3    <dbl> 0.25, 0.70, 0.80
```

### Using `ai_score()` for scoring documents

``` r
prompt <- "Score the following document on a scale of how much it aligns
with the political left. The political left is defined as groups which
advocate for social equality, government intervention in the economy,
and progressive policies. Use the following metrics:
SCORING METRIC:
3 : extremely left
2 : very left
1 : slightly left
0 : not at all left"

data_corpus_inaugural <- data_corpus_inaugural %>%
  mutate(ai_score(text, prompt, chat_fn = chat_openai, model = "gpt-4o",
                  api_args = list(temperature = 0, seed = 42)))
#> 
#> Calling chat_fn (gpt-4o):
#> ... processing: [1/3]
#> ... processing: [2/3]
#> ... processing: [3/3]
#> Finished.

# score and evidence are created as new docvars in the corpus
cbind(data.frame(docnames = docnames(data_corpus_inaugural)), 
      select(docvars(data_corpus_inaugural), score, evidence)) %>%
  kable("html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(3) 
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

docnames
</th>

<th style="text-align:left;">

score
</th>

<th style="text-align:left;">

evidence
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

2017-Trump
</td>

<td style="text-align:left;">

0
</td>

<td style="text-align:left;">

The document emphasizes themes such as nationalism, patriotism, and
‘America First,’ highlighting a departure from government intervention
in global affairs that prioritize American interests and protectionism.
The language used suggests disenchantment with the establishment and a
focus on returning power to the populace in rhetoric rather than
advocating for progressive policies or social equality. The stress on
self-reliance, protectionism in trade, and criticism of the political
establishment aligns more with conservative rather than leftist views.
</td>

</tr>

<tr>

<td style="text-align:left;">

2025-Trump
</td>

<td style="text-align:left;">

1
</td>

<td style="text-align:left;">

The document emphasizes themes of unity, democracy, and addressing
systemic issues like racial justice and the climate crisis. These are
values generally associated with the political left. The speech also
mentions the need for government action to rebuild the middle class and
secure healthcare for all, aligning with progressive economic policies.
However, the overall message is one of unity and seeking common ground,
not exclusively leftist personal or economic policies, resulting in a
score of ‘slightly left.’
</td>

</tr>

<tr>

<td style="text-align:left;">

2021-Biden.txt
</td>

<td style="text-align:left;">

2
</td>

<td style="text-align:left;">

The document emphasizes themes typically associated with the political
left, such as unity, racial justice, and combating political extremism,
white supremacy, and domestic terrorism. It endorses progressive ideals
like rebuilding the middle class, making healthcare secure for all, and
delivering racial justice, all of which align with left-leaning
priorities. The focus on climate crisis and systemic racism are also
indicative of left-wing concerns. However, the speech also calls for
unity and bipartisan cooperation, which moderates the left alignment
slightly, but it strongly emphasizes traditionally progressive policies.
</td>

</tr>

</tbody>

</table>

### Using `ai_validate()` to manually check LLM-generated outputs

``` r
data_corpus_inaugural <- data_corpus_inaugural %>%
  mutate(ai_validate(text, score, evidence))
# validated scores and comments as well as highlighted examples from texts are
# created as new docvars in the corpus 
```
