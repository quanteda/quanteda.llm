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
