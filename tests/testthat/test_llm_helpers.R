# test_llm_helpers.R

library(testthat)
library(here)

# Source your LLM logic file. 
# Make sure "llm_logic.R" really exists in the "R" folder at your project root.
source(here("R", "llm_helpers.R"))

test_that("generate_plot_code() returns a non-empty string", {
  # 1) Create minimal sample data
  sample_df <- data.frame(
    x = 1:5,
    y = c(3.2, 4.5, 2.1, 5.0, 6.7)
  )
  
  # 2) Minimal prompt
  prompt_text <- "Create a scatter plot of x vs y using ggplot."
  
  # 3) We need an LLM object if generate_plot_code() relies on one
  #    e.g. if your llm_logic.R has create_llm_object()
  #    LLM might require a valid chat backend, so you can skip if not set up
  llm <- create_llm_object()
  
  # 4) Call your function
  code_result <- generate_plot_code(
    prompt = prompt_text,
    df     = sample_df,
    llm    = llm
  )
  
  # 5) Basic checks
  expect_type(code_result, "character")
  expect_gt(nchar(code_result), 0)  # not empty
})

test_that("refine_plot_code() modifies code based on feedback", {
  skip("Uncomment once you have a working LLM + logic for refine_plot_code()")
  
  # 1) Some existing code
  current_code <- "ggplot(df, aes(x, y)) + geom_point()"
  feedback     <- "Please change points to a histogram."
  
  # 2) Possibly create the LLM object
  llm <- create_llm_object()
  
  # 3) Refine the code
  refined <- refine_plot_code(
    current_code = current_code,
    feedback     = feedback,
    llm          = llm
  )
  
  # 4) Check results
  expect_type(refined, "character")
  expect_false(identical(refined, current_code)) # should differ
})

