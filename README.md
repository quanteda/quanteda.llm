
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

- `text_split`: Splits documents in a corpus into sequences which LLMs
  can process
- `ai_sum`: Summarises documents in a corpus using an LLM
- `ai_qual`: Analyses documents in a corpus using an LLM and based on
  given instructions (simulating qualitative assessments)
- `ai_quant`: Scores documents in a corpus using an LLM and based on
  given instructions

More to follow.

# Supported LLMs

The package supports the following LLMs:

- **Ollama models**: To use the **quanteda.llm** functions first
  download and install [Ollama](https://ollama.com/). Then install some
  models either from the command line (e.g. with ollama pull llama3.2)
  or within R using the `rollama` R package. The Ollama app must be
  running for the models to be used.

- More to follow.

## Installation

You can install the development version of quanteda.llm from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("quanteda/quanteda.llm")
```

## Examples

### Using `text_split`

``` r
library(quanteda.llm)
corpus <- quanteda::data_corpus_inaugural
corpus_split <- text_split(corpus, "text", model="BERT") # split the text into sequences of 512 characters as required by BERT
corpus_split <- corpus_split %>%
  select(doc_id, splits, text_index)
corpus_split %>% head()
# A tibble: 6 × 4
#  doc_id          President  splits                                                 text_index
#  <chr>           <chr>      <chr>                                                       <int>
#1 1789-Washington Washington "Fellow-Citizens of the Senate and of the House of "            1
#2 1789-Washington Washington "Representatives:\n\nAmong the vicissitudes incident "          2
#3 1789-Washington Washington "to life no event could have filled me with greater"            3
#4 1789-Washington Washington " anxieties than that of which the notification was"            4
#5 1789-Washington Washington " transmitted by your order, and received on the 14"            5
#6 1789-Washington Washington "th day of the present month. On the one hand, I wa"            6
```

### Using `ai_sum`

``` r
library(quanteda.llm)
ai_summary <- ai_sum(corpus_split, 
                     "splits", 
                     "llama3.2", 
                     100)
ai_summary[5,5]
# A tibble: 1 × 1
#  summary                                                                                                                    
#  <chr>                                                                                                                      
#1 1 The author's official act in office is an invocation of God's blessing on the US government and its lead…
```

### Using `ai_qual`

``` r
library(quanteda.llm)
ai_qualies <- ai_qual(corpus_split, 
                      "splits", 
                      "llama3.2", 
                      prompt = "Is this text leaning towards the political left? The political left defined as groups which advocate for social equality, government intervention in the economy, and progressive policies.")
ai_qualies[3,4]
# A tibble: 1 × 1
#  assess                                                                                                                                                         
#  <chr>                                                                                                    
1 "This text does not lean towards the political left. In fact, it is a passage from the inaugural address…
```

### Using `ai_quant`

``` r
library(quanteda.llm)
ai_quanties <- ai_quant(corpus_split, 
                        "splits", 
                        "llama3.2", 
                        concept = "The political left defined as groups which advocate for social equality, government intervention in the economy, and progressive policies.")
head(ai_quant)
# A tibble: 3 × 5
#  splits                                               text_index score
#  <chr>                                                     <int> <dbl>
#1 1789-Washington Washington "Fellow-Citizens of the Senate and of the House of Represent…          1  0.74
#2 1789-Washington Washington "e asylum of my declining years  -  a retreat which was rend…          2  0.1 
#3 1789-Washington Washington "dence one who (inheriting inferior endowments from nature a…          3  0   
```
