# test_default_graphs.R

library(testthat)
library(here)
library(ellmer)
source(here("R", "default_graphs.R"))
library(ggplot2)
library(tidyverse)

test_that("generate_default_line_code() generates valid ggplot code", {
  
  # 1) Basic test inputs
  x <- "time"
  y <- "voltage"
  
  # 2) Create a sample df with matching columns
  df <- data.frame(
    time = 1:10,
    voltage = rnorm(10)
  )
  
  # 3) Generate code
  code <- generate_default_line_code(x, y)
  
  # 4) Check type and structure
  expect_type(code, "character")
  expect_length(code, 1)
  expect_match(code, "ggplot\\(")
  expect_match(code, "geom_line\\(")
  expect_match(code, "!!sym\\('time'\\)")
  expect_match(code, "!!sym\\('voltage'\\)")
  
  # 5) Evaluate code and test result
  val <- eval(parse(text = code), envir = list2env(list(df = df)))
  expect_s3_class(val, "ggplot")
})

test_that("generate_default_bar_code() generates valid ggplot bar plot code", {
  
  # 1) Basic test inputs
  x <- "category"
  y <- "count"
  
  # 2) Create a sample df with matching columns
  df <- data.frame(
    category = c("A", "B", "C"),
    count = c(10, 20, 15)
  )
  
  # 3) Generate code
  code <- generate_default_bar_code(x, y)
  
  # 4) Check type and structure
  expect_type(code, "character")
  expect_length(code, 1)
  
  # 5) Confirm proper column references and structure
  expect_match(code, "!!sym\\('category'\\)")
  expect_match(code, "!!sym\\('count'\\)")
  expect_match(code, "geom_col\\(")
  expect_match(code, "title = 'Bar Plot of count by category'")
  expect_match(code, "x = 'category'")
  expect_match(code, "y = 'count'")
  
  # 6) Evaluate code and confirm it's a ggplot object
  val <- eval(parse(text = code), envir = list2env(list(df = df)))
  expect_s3_class(val, "ggplot")
})

test_that("generate_default_scatter_code() generates valid ggplot scatter plot code", {
  x <- "height"
  y <- "weight"
  
  df <- data.frame(
    height = rnorm(50, 170, 10),
    weight = rnorm(50, 65, 15)
  )
  
  code <- generate_default_scatter_code(x, y)
  
  expect_type(code, "character")
  expect_length(code, 1)
  expect_match(code, "!!sym\\('height'\\)")
  expect_match(code, "!!sym\\('weight'\\)")
  expect_match(code, "geom_point\\(")
  expect_match(code, "title = 'Scatter Plot of weight vs height'")
  
  val <- eval(parse(text = code), envir = list2env(list(df = df)))
  expect_s3_class(val, "ggplot")
})


test_that("generate_default_hist_code() generates valid ggplot histogram code", {
  col <- "temperature"
  
  df <- data.frame(
    temperature = rnorm(100, 22, 3)
  )
  
  code <- generate_default_hist_code(col)
  
  expect_type(code, "character")
  expect_length(code, 1)
  expect_match(code, "!!sym\\('temperature'\\)")
  expect_match(code, "geom_histogram\\(")
  expect_match(code, "title = 'Histogram of temperature'")
  
  val <- eval(parse(text = code), envir = list2env(list(df = df)))
  expect_s3_class(val, "ggplot")
})


test_that("generate_default_agg_bar_code() generates valid aggregated bar plots", {
  cat_col <- "group"
  num_col <- "value"
  
  df <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    value = rnorm(30)
  )
  
  for (agg in c("sum", "mean", "max", "min", "count")) {
    code <- generate_default_agg_bar_code(cat_col, num_col, agg)
    
    expect_type(code, "character")
    expect_length(code, 1)
    expect_match(code, glue::glue("title = '{agg} of {num_col} by {cat_col}'"))
    expect_match(code, "geom_col\\(")
    
    val <- eval(parse(text = code), envir = list2env(list(df = df)))
    expect_s3_class(val, "ggplot")
  }
})
