# test_llm_helpers.R

library(testthat)
library(here)
library(ellmer)

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

test_that("validate_plot_code() disallows malicious code", {
  
  # 1) Banned patterns
  pattern_1 <- "ggplot(df, aes(x = var1, y = var2)) + geom_point()"
  pattern_2 <- "system('ls'); ggplot(df, aes(x, y)) + geom_point()"
  pattern_3 <- "library(tidyverse)"
  pattern_4 <- "rm(df)"
  pattern_5 <- "install.packages(tidyverse)"
  pattern_6 <- "ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) + geom_line()"
  
  result_1 <- validate_plot_code(pattern_1)
  result_2 <- validate_plot_code(pattern_2)
  result_3 <- validate_plot_code(pattern_3)
  result_4 <- validate_plot_code(pattern_4)
  result_5 <- validate_plot_code(pattern_5)
  result_6 <- validate_plot_code(pattern_6)
  
  # 4) Check results
  expect_true(result_1)
  expect_false(result_2)
  expect_false(result_3)
  expect_false(result_4)
  expect_false(result_5)
  expect_true(result_6) 
})
