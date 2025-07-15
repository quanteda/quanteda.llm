# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(quanteda.llm)

chat_test <- function(...) {
  structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(text, type) {
        list(score = 0.8)
      }
    ),
    class = "MockChat"
  )
}

test_check("quanteda.llm")
