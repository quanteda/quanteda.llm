#' Analysis modules
#'
#' Factory functions to create structured AI text analysis modules.
#' Use these with [ai_text()] by passing them as the `module` argument.
#'
#' These modules dynamically insert parameters into system prompts using
#' `ellmer::interpolate()`, allowing you to customize behavior when creating
#' modules.
#'
#' @name module
#' @importFrom ellmer type_object type_number type_string type_array interpolate
NULL

#' Sentiment analysis module factory
#'
#' @param sentiment_scale_min Numeric, minimum sentiment score (default -1)
#' @param sentiment_scale_max Numeric, maximum sentiment score (default 1)
#' @return An llm_module for sentiment analysis
#' @export
sentiment_module <- function(sentiment_scale_min = -1, sentiment_scale_max = 1) {
  make_module(
    name = "Sentiment Analysis",
    system_prompt = ellmer::interpolate("
      You are an expert sentiment analyst.
      Rate the sentiment of the input text on a scale from {{sentiment_scale_min}} (very negative) 
      to {{sentiment_scale_max}} (very positive).
      Provide a numeric 'score' and a brief 'explanation' for your rating.
    "),
    type_object = ellmer::type_object(
      score = ellmer::type_number("Sentiment score between the scale bounds."),
      explanation = ellmer::type_string("Brief explanation for the sentiment score.")
    )
  )
}

#' Position taking module factory
#'
#' @param stance_labels Character vector of stance labels (default c("support", "oppose", "neutral"))
#' @return An llm_module for position taking
#' @export
position_taking_module <- function(stance_labels = c("support", "oppose", "neutral")) {
  make_module(
    name = "Position Taking",
    system_prompt = ellmer::interpolate("
      You are an expert analyst extracting claims.
      Extract the main 'claim', the 'stance' (choose from {{paste(stance_labels, collapse = ', ')}}, case-insensitive), 
      and the 'rationale' for the stance.
    "),
    type_object = ellmer::type_object(
      claim = ellmer::type_string("The main claim extracted from the text."),
      stance = ellmer::type_string("Position taken regarding the claim."),
      rationale = ellmer::type_string("Reasoning behind the stance.")
    )
  )
}

#' Ideological scaling module factory
#'
#' @param ideology_scale_min Numeric minimum scale (default 0)
#' @param ideology_scale_max Numeric maximum scale (default 10)
#' @return An llm_module for ideological scaling
#' @export
ideological_scaling_module <- function(ideology_scale_min = 0, ideology_scale_max = 10) {
  make_module(
    name = "Ideological Scaling",
    system_prompt = ellmer::interpolate("
      You are a political analyst.
      Rate the ideological position of the text on a scale from {{ideology_scale_min}} to {{ideology_scale_max}}.
      Provide a numeric 'position' and a 'justification' explaining the rating.
    "),
    type_object = ellmer::type_object(
      position = ellmer::type_number("Ideological position score within the scale bounds."),
      justification = ellmer::type_string("Explanation for the ideological position.")
    )
  )
}

#' Policy salience module factory
#'
#' @param policy_issues_example Character vector of example policy issues (default c("Economy", "Healthcare", "Education", "Environment", "Immigration"))
#' @return An llm_module for policy salience ranking
#' @export
policy_salience_module <- function(policy_issues_example = c("Economy", "Healthcare", "Education", "Environment", "Immigration")) {
  make_module(
    name = "Policy Salience",
    system_prompt = ellmer::interpolate("
      You are a policy analyst.
      Rank the policy issues discussed in the text by their salience.
      Policy issues may include: {{paste(policy_issues_example, collapse = ', ')}}.
      Return a ranked list of policy issues with fields 'issue' and 'rank'.
    "),
    type_object = ellmer::type_array(
      ellmer::type_object(
        issue = ellmer::type_string("Policy issue."),
        rank = ellmer::type_number("Rank of policy issue by salience.")
      ),
      description = "Ranked list of policy issues by salience."
    )
  )
}

#' Named entity recognition module factory
#'
#' @param named_entity_categories Character vector of categories (default c("people", "parties", "organizations"))
#' @return An llm_module for named entity recognition
#' @export
named_entity_module <- function(named_entity_categories = c("people", "parties", "organizations")) {
  make_module(
    name = "Named Entity Recognition",
    system_prompt = ellmer::interpolate("
      Extract named entities from the text, categorized into: {{paste(named_entity_categories, collapse = ', ')}}.
      Return lists for each category.
    "),
    type_object = ellmer::type_object(
      people = ellmer::type_array(ellmer::type_string(), description = "List of people."),
      parties = ellmer::type_array(ellmer::type_string(), description = "List of political parties."),
      organizations = ellmer::type_array(ellmer::type_string(), description = "List of organizations.")
    )
  )
}

#' Topic classification module factory
#'
#' @param topic_labels Character vector of allowed topics (default c("Technology", "Health", "Politics", "Education", "Environment", "Economy"))
#' @return An llm_module for topic classification
#' @export
topic_classification_module <- function(topic_labels = c("Technology", "Health", "Politics", "Education", "Environment", "Economy")) {
  make_module(
    name = "Topic Classification",
    system_prompt = ellmer::interpolate("
      Classify the text into one or more topics from: {{paste(topic_labels, collapse = ', ')}}.
      Provide the list of assigned topics as 'topics' and a brief 'explanation'.
    "),
    type_object = ellmer::type_object(
      topics = ellmer::type_array(ellmer::type_string(), description = "List of assigned topic labels."),
      explanation = ellmer::type_string("Brief explanation for the classification.")
    )
  )
}

#' Framing analysis module factory
#'
#' @param frame_labels Character vector of frame labels (default c("Economic", "Moral", "Security", "Rights", "Environment"))
#' @return An llm_module for framing analysis
#' @export
framing_analysis_module <- function(frame_labels = c("Economic", "Moral", "Security", "Rights", "Environment")) {
  make_module(
    name = "Framing Analysis",
    system_prompt = ellmer::interpolate("
      Identify text spans and assign frame labels from: {{paste(frame_labels, collapse = ', ')}}.
      Return a list of objects with 'text_span' and 'frame_label'.
    "),
    type_object = ellmer::type_array(
      ellmer::type_object(
        text_span = ellmer::type_string("Extracted text span."),
        frame_label = ellmer::type_string("Assigned frame label.")
      ),
      description = "List of frames with text spans and labels."
    )
  )
}

#' Claim detection module factory
#'
#' @return An llm_module for claim detection
#' @export
claim_detection_module <- function() {
  make_module(
    name = "Claim Detection",
    system_prompt = ellmer::interpolate("
      Extract tuples of (actor, action, issue) from the text.
    "),
    type_object = ellmer::type_array(
      ellmer::type_object(
        actor = ellmer::type_string("Actor making the claim."),
        action = ellmer::type_string("Action or statement."),
        issue = ellmer::type_string("Issue addressed.")
      ),
      description = "List of (actor, action, issue) tuples."
    )
  )
}

#' Trade-off identification module factory
#'
#' @return An llm_module for trade-off identification
#' @export
tradeoff_module <- function() {
  make_module(
    name = "Trade-off Identification",
    system_prompt = ellmer::interpolate("
      Identify pairs of competing considerations or options described as trade-offs.
    "),
    type_object = ellmer::type_array(
      ellmer::type_object(
        option_1 = ellmer::type_string("First competing option."),
        option_2 = ellmer::type_string("Second competing option.")
      ),
      description = "List of pairwise trade-offs."
    )
  )
}

#' Narrative analysis module factory
#'
#' @return An llm_module for narrative analysis
#' @export
narrative_module <- function() {
  make_module(
    name = "Narrative Analysis",
    system_prompt = ellmer::interpolate("
      Provide a narrative summary including main actors and plot arc.
    "),
    type_object = ellmer::type_object(
      summary = ellmer::type_string("Summary of the narrative."),
      actors = ellmer::type_array(ellmer::type_string(), description = "List of main actors."),
      plot_arc = ellmer::type_string("Description of plot arc.")
    )
  )
}

#' Factuality checking module factory
#'
#' @return An llm_module for factuality checking
#' @export
factuality_module <- function() {
  make_module(
    name = "Factuality Checking",
    system_prompt = ellmer::interpolate("
      Provide a confidence score between 0 (low) and 1 (high) on the factuality of the text.
      Include an explanation.
    "),
    type_object = ellmer::type_object(
      confidence = ellmer::type_number("Confidence score between 0 and 1."),
      explanation = ellmer::type_string("Explanation of factuality confidence.")
    )
  )
}

#' Argument structure extraction module factory
#'
#' @return An llm_module for argument structure extraction
#' @export
argument_structure_module <- function() {
  make_module(
    name = "Argument Structure Extraction",
    system_prompt = ellmer::interpolate("
      Extract argument components: premises, claims, and supporting evidence.
    "),
    type_object = ellmer::type_object(
      premises = ellmer::type_array(ellmer::type_string(), description = "List of premises."),
      claims = ellmer::type_array(ellmer::type_string(), description = "List of claims."),
      support = ellmer::type_array(ellmer::type_string(), description = "List of supporting evidence.")
    )
  )
}

#' Dialogic analysis module factory
#'
#' @return An llm_module for dialogic analysis
#' @export
dialogic_analysis_module <- function() {
  make_module(
    name = "Dialogic Analysis",
    system_prompt = ellmer::interpolate("
      Split the transcript into speaker turns.
      For each turn, provide 'turn_id', 'speaker', 'utterance'.
      Optionally provide 'reply_to' indicating which turn this responds to.
    "),
    type_object = ellmer::type_array(
      ellmer::type_object(
        turn_id = ellmer::type_string("Unique turn identifier."),
        speaker = ellmer::type_string("Speaker name."),
        utterance = ellmer::type_string("Text of the utterance."),
        reply_to = ellmer::type_string("Turn ID this turn replies to, or null.")
      ),
      description = "List of speaker turns and reply relationships."
    )
  )
}

#' Translation module factory
#'
#' @param target_language Character string for target language (default "English")
#' @return An llm_module for translation
#' @export
translation_module <- function(target_language = "English") {
  make_module(
    name = paste0("Translation to ", target_language),
    system_prompt = ellmer::interpolate("
      Translate the input text into {{target_language}}.
      Provide the translation as 'translation' and notes on nuances as 'notes'.
    "),
    type_object = ellmer::type_object(
      translation = ellmer::type_string("Translated text in the target language."),
      notes = ellmer::type_string("Notes on translation nuances or challenges.")
    )
  )
}
