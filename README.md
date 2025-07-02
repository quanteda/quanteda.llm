
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quanteda.llm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/quanteda.llm)](https://CRAN.R-project.org/package=quanteda.llm)
[![R-CMD-check](https://github.com/quanteda/quanteda.llm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quanteda/quanteda.llm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/quanteda/quanteda.llm/graph/badge.svg)](https://app.codecov.io/gh/quanteda/quanteda.llm)
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
#> Package version: 4.3.1
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

The document is an inaugural address in which the speaker emphasizes a
national effort to rebuild the country and restore its promise for all
citizens. The speaker criticizes past administrations for benefiting a
small group in Washington while neglecting the broader population,
resulting in job loss and economic decline. The promise of the speech is
to transfer power back to the people, emphasizing ‘America first’
policies. Key themes include protecting US borders, revitalizing
infrastructure, enhancing national pride, and enhancing American
industry and employment. The speech concludes with a call for unity and
a commitment to making America strong, wealthy, proud, and safe again.
</td>

</tr>

<tr>

<td style="text-align:left;">

2021-Biden
</td>

<td style="text-align:left;">

The speech is a call for unity and commitment to overcoming current
challenges in the United States, including a health crisis, political
divisions, racial injustice, and climate change. The speaker emphasizes
the importance of democracy, resilience, and collective action in
addressing these issues. They acknowledge past struggles and triumphs,
reiterating the need for unity and truth to secure a better future. The
speech invokes historical moments and figures to inspire resolve and
purpose, encouraging all Americans to work together for the common good.
The address concludes with a pledge to serve all Americans and a call
for prayers and a moment of silence for those lost to the pandemic.
</td>

</tr>

<tr>

<td style="text-align:left;">

2025-Trump
</td>

<td style="text-align:left;">

This document appears to be a speech by Donald Trump, addressing various
political figures and the public, marking the beginning of his new
presidential term. In his address, he outlines his agenda which includes
prioritizing “America first,” restoring national sovereignty, and
ensuring safety by ending the weaponization of the Justice Department.
He aims to confront issues of national trust, immigration, public
safety, and the economy, including initiatives like declaring
emergencies at the southern border and energy industry reforms to boost
domestic production.

Trump announces measures to restore what he perceives as lost American
values and promotes national unity, prosperity, and greatness. He refers
to his election as a mandate to reverse prior policies, citing support
from diverse demographic groups. Trump’s agenda further emphasizes
strengthening the military, revamping trade relations via tariffs, and
addressing environmental and educational policies according to his
vision. The speech conveys a rallying call to renew American pride and
strength, framing his administration as a transformative era for the
United States.
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
#> $ doc_id    <chr> "2017-Trump", "2021-Biden", "2025-Trump"
#> $ text      <chr> "Chief Just…", "Chief Just…", "Thank you.…"
#> $ Year      <int> 2017, 2021, 2025
#> $ President <chr> "Trump", "Biden", "Trump"
#> $ FirstName <chr> "Donald J.", "Joseph R.", "Donald J."
#> $ Party     <fct> Republican, Democratic, Republican
#> $ id        <chr> "1", "2", "3"
#> $ summary   <chr> "The document is an inaugural address in which the speaker e…
#> $ topic1    <dbl> 1, 1, 1
#> $ topic2    <dbl> 2, 2, 2
#> $ topic3    <dbl> 3, 3, 3
#> $ score1    <dbl> 0.7, 0.2, 0.4
#> $ score2    <dbl> 0.10, 0.35, 0.30
#> $ score3    <dbl> 0.20, 0.45, 0.30
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

The document represents a speech that emphasizes nationalism, populism,
and protectionism, reflecting right-wing and conservative ideologies
rather than leftist ones. The speech highlights transferring power away
from the political establishment to the people, criticizes foreign
engagements, and advocates for ‘America first’ policies, focusing on
national strength and pride. These elements do not align with the
political left’s focus on global cooperation and progressive social
policies.
</td>

</tr>

<tr>

<td style="text-align:left;">

2021-Biden
</td>

<td style="text-align:left;">

2
</td>

<td style="text-align:left;">

The document, which is a speech, emphasizes themes of unity, social
justice, racial equality, and combating climate change - all causes
typically associated with the political left. It stresses the need to
address systemic racism, a ‘cry for racial justice,’ and the existential
threat posed by climate change. The call for restoring alliances and
focusing on unity and collective action aligns with leftist principles
of international cooperation and social equality. The repeated calls for
unity and healing from recent national divisions, while not explicitly
leftist, are set in contrast to addressing right-leaning extremism and
white supremacy.
</td>

</tr>

<tr>

<td style="text-align:left;">

2025-Trump
</td>

<td style="text-align:left;">

0
</td>

<td style="text-align:left;">

The document aligns with right-wing political ideology, emphasizing
national sovereignty, reduction in government intervention (e.g., ending
the Green New Deal), and traditional values. The speech prioritizes
America First policies, a strong military, and economic nationalism,
including tariffs and energy independence. These points of focus
contrast with left-wing advocacy for social equality, economic
regulation, and progressive policies. Thus, the score regarding how much
this document aligns with the political left is 0: not at all left.
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
