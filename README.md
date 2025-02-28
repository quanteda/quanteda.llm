
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

speeches_split <- text_split(speeches, "text", length_seq=2000) # split the text into sequences of 512 characters
speeches_split <- speeches_split %>%
  select(doc_id, splits, text_index)
speeches_split %>% head()
# A tibble: 6 × 3
#  doc_id splits                                                                                                                                          text_index
#   <chr>  <chr>                                                                                                                                                <int>
#1 264    " Thanks, very much, Madam Deputy Speaker. Can I congratulate on your election as Deputy Speake…          1
#2 264    "we updated our Nationally Determined Contribution under the Paris Agreement to reflect th…          2
#3 264    "Passing this legislation sends a great message to the people of Australia that we are tak…          3
#4 264    "as big and as significant as the Industrial Revolution. The Industrial Revolution was based u…          4
#5 264    "when it came to the pandemic was that Australia needs to be more self-reliant. We need to be …          5
#6 264    "debate in an academic journal. Australians in recent times have experienced first drought…          6
```

### Using `ai_sum`

``` r
library(quanteda.llm)
ai_summary <- ai_sum(speeches_split, 
                     "splits", 
                     "llama3.2", 
                     100)
ai_summary[1,3]
# A tibble: 1 × 1
#  summary                                                                                                                    
#  <chr>                                                                                                                      
#1 The new speaker speaks about the importance of listening to facts and science when it comes to climate change, emphasizing…
```

### Using `ai_qual`

``` r
library(quanteda.llm)
ai_qualies <- ai_qual(speeches_split, 
                      "splits", 
                      "llama3.2", 
                      prompt = "Is this text leaning towards the political left? The political left defined as groups which advocate for social equality, government intervention in the economy, and progressive policies.")
ai_qualies[3,4]
# A tibble: 1 × 1
#  assess                                                                                                                                                         
#  <chr>                                                                                                                                                          
#1 "This text leans towards the political left as it supports progressive policies such as taking action on climate change, committing to a net-zero target by 20…
```

### Using `ai_quant`

``` r
library(quanteda.llm)
ai_quanties <- ai_quant(speeches_split, 
                        "splits", 
                        "llama3.2", 
                        concept = "The political left defined as groups which advocate for social equality, government intervention in the economy, and progressive policies.")
head(ai_quant)
# A tibble: 5 × 3
#  splits                                               text_index score
#  <chr>                                                     <int> <dbl>
#1 " Thanks, very much, Madam Deputy Speaker. Can I co"          1 0.34 
#2 "ngratulate on your election as Deputy Speaker. Can"          2 0    
#3 " I say to the previous speaker, who is not a bad b"          3 1    
#4 "loke, but, for goodness sake, to speak about facts"          4 0.955
#5 " when it comes to climate change. Wow. Yet, we sho"          5 0.5  
```
