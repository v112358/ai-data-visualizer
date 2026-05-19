# R/llm_helpers.R
# LLM integration for localized chart editing.
# The LLM receives existing code + user request and returns a modified version.
# It never sees the full dataset, only schema + sample rows.

library(ellmer)
library(here)
source(here("keys", "gemini.R"))

create_llm_object <- function() {
  chat_google_gemini(
    system_prompt =
      "You are an expert ggplot2 code editor embedded in a professional chart editor.
Your only job is to apply LOCALIZED, SURGICAL modifications to existing ggplot2 R code.

Hard rules:
- Return ONLY the complete modified R code. No prose, no explanations, no markdown, no backticks.
- The data frame is always named 'df'. Never load libraries.
- Preserve everything in the existing code that the user did not ask to change.
- Use tidyverse syntax. Reference columns with !!sym('colname').
- The final expression must be the plot itself (no assignment with <- or ->).
- For structural changes (new geom layers, group coloring, facets): append to the existing chain.
- For theme/style changes: replace only the affected argument in the relevant function call.
- For color values: use valid R color names or hex codes (e.g. '#e63946' or 'tomato').
- If the request is ambiguous, make the most visually sensible change.
- Use linewidth (not size) for line thickness in ggplot2 >= 3.4.",
    api_key = GEMINI_KEY,
    model = 'gemini-2.5-flash'
  )
}

#' Apply a localized LLM edit to existing ggplot code.
#' @param current_code character - the current ggplot code string
#' @param request character - user's natural language request
#' @param df data.frame - dataset (used for schema context only, never full data)
#' @param llm ellmer chat object
#' @return character - updated code string
apply_llm_edit <- function(current_code, request, df, llm) {
  schema <- paste(
    paste0(names(df), " (", vapply(df, function(x) class(x)[1], ""), ")"),
    collapse = ", "
  )

  n_rows   <- nrow(df)
  sample_r <- paste(capture.output(print(head(df, 3))), collapse = "\n")

  prompt <- glue::glue(
    "Current ggplot2 code:\n{current_code}\n\n",
    "Dataset: {n_rows} rows. Columns: {schema}\n",
    "Sample rows:\n{sample_r}\n\n",
    "User request: {request}\n\n",
    "Return ONLY the modified R code. No markdown, no backticks, no explanation."
  )

  response <- llm$chat(prompt)
  sanitize_code(response)
}

sanitize_code <- function(code_string) {
  code_string <- gsub("```[rR]?\n?|```", "", code_string)
  code_string <- gsub(" {2,}", " ", code_string)
  trimws(code_string)
}

validate_plot_code <- function(code) {
  disallowed <- c(
    "system\\(", "unlink\\(", "\\brm\\(", "library\\(", "require\\(",
    "install\\.packages\\(", "source\\(", "readLines\\(", "writeLines\\(",
    "\\bfile\\(", "readRDS\\(", "saveRDS\\(", "Sys\\.setenv\\(", "options\\("
  )
  for (pat in disallowed) {
    if (grepl(pat, code, ignore.case = TRUE)) {
      warning(paste("Disallowed pattern:", pat))
      return(FALSE)
    }
  }
  TRUE
}
