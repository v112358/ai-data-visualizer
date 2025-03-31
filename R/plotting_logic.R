#' Validate that the LLM-generated ggplot code is "safe" to run
#'
#' @param code A character string containing the R code (ggplot, etc.)
#' @return TRUE if safe, or FALSE (or an error) if not safe

validate_plot_code <- function(code) {
  
  # 1. Check for disallowed functions or patterns
  disallowed_patterns <- c(
    "system\\(",  # Might allow remote code execution
    "unlink\\(",  # Could delete files
    "rm\\(",      # Could remove objects from the environment
    "library\\(", "require\\(", # Shouldn't load additional packages
    "install.packages\\("
  )
  
  for (pat in disallowed_patterns) {
    if (grepl(pat, code, ignore.case = TRUE)) {
      warning(paste("Disallowed pattern found:", pat))
      return(FALSE)
    }
  }
  
  # 2. Check that it references df (or doesn't try to use random objects)
  if (!grepl("ggplot\\s*\\(\\s*df", code)) {
    warning("The code does not appear to reference 'df' in ggplot(...)")
  }
  
  # If all checks passed:
  TRUE
}


#' Sanitize code by removing extra backticks, triple backticks, etc.
#'
#' @param code A character string containing the R code
#' @return A cleaned-up character string
sanitize_plot_code <- function(code) {
  code <- gsub("```[rR]?", "", code)        # remove ```r, ```
  code <- gsub("\\\\", "", code)            # remove escaped backslashes
  code <- gsub("\\n|\\t", " ", code)        # remove newlines, tabs
  code <- gsub(" +", " ", code)             # collapse multiple spaces
  code <- trimws(code)                      # trim leading/trailing whitespace
  code
}


#' Safely generate the plot by validating and parsing
#'
#' @param code A character string containing ggplot code
#' @param .data A data.frame or tibble to assign to `df` in the environment
#'
#' @return The resulting plot object, or NULL if invalid
generate_safe_plot <- function(code, .data) {
  
  # 1) Sanitize
  code_clean <- sanitize_plot_code(code)
  
  # 2) Validate
  is_safe <- validate_plot_code(code_clean)
  if (!is_safe) {
    warning("Plot code deemed unsafe. Returning NULL.")
    return(NULL)
  }
  
  # 3) Attempt to parse
  expr <- NULL
  tryCatch({
    expr <- parse(text = code_clean)
  }, error = function(e) {
    warning("Failed to parse plot code: ", e$message)
  })
  
  if (is.null(expr)) return(NULL)
  
  # 4) Evaluate in a safe environment
  #    We create an environment with df = .data
  safe_env <- new.env(parent = emptyenv())
  safe_env$df <- .data
  
  plot_obj <- NULL
  tryCatch({
    # Evaluate expression in that environment
    plot_obj <- eval(expr, envir = safe_env)
  }, error = function(e) {
    warning("Plot code evaluation error: ", e$message)
  })
  
  plot_obj
}
