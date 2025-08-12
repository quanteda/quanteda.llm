#' Create a structured analysis module
#'
#' Helper to create an "llm_module" object containing a system prompt,
#' a type_object schema, and optional post-processing function.
#'
#' @param name A descriptive name for the module.
#' @param system_prompt A system prompt string to guide the LLM.
#' @param type_object An \pkg{ellmer} [type_object()][ellmer::type_object()]
#'   describing the expected structured output.
#' @param post_process Optional function to post-process results.
#'
#' @return An object of class `"llm_module"`.
#' @export
make_module <- function(name, system_prompt, type_object, post_process = NULL) {
  structure(
    list(
      name = name,
      system_prompt = system_prompt,
      type_object = type_object,
      post_process = post_process
    ),
    class = "llm_module"
  )
}