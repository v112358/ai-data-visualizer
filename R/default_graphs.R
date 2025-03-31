#' Generate default line graph code
#'
#' Creates a neat, minimal line plot of y_col vs x_col from df, 
#' with a consistent theme and labels.
#'
#' @param x_col Character string for the column name on the x-axis
#' @param y_col Character string for the column name on the y-axis
#'
#' @return A character string containing ggplot code
#'
generate_default_line_code <- function(x_col, y_col) {
  glue::glue("
df %>%
  ggplot(aes(x = !!sym('{x_col}'), y = !!sym('{y_col}'))) +
    geom_line(color = 'steelblue', size = 1) +
    labs(
      title = 'Line Graph of {y_col} vs {x_col}',
      x = '{x_col}',
      y = '{y_col}'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face='bold', hjust=0.5, size=14),
      axis.text = element_text(size=11),
      axis.title = element_text(size=12)
    )
")
}


#' Generate default bar plot code
#'
#' @param x_col Character string for the categorical column on the x-axis
#' @param y_col Character string for the numeric column on the y-axis
#' @return A character string containing ggplot code
#'
generate_default_bar_code <- function(x_col, y_col) {
  glue::glue("
df %>%
  ggplot(aes(x = !!sym('{x_col}'), y = !!sym('{y_col}'))) +
    geom_col(fill = 'steelblue', color = 'white') +
    labs(
      title = 'Bar Plot of {y_col} by {x_col}',
      x = '{x_col}',
      y = '{y_col}'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face='bold', hjust=0.5, size=14),
      axis.text = element_text(size=11),
      axis.title = element_text(size=12)
    )
")
}

#' Generate default scatter plot code
#'
#' @param x_col Character string for the x-axis numeric column
#' @param y_col Character string for the y-axis numeric column
#' @return A character string containing ggplot code
#'
generate_default_scatter_code <- function(x_col, y_col) {
  glue::glue("
df %>%
  ggplot(aes(x = !!sym('{x_col}'), y = !!sym('{y_col}'))) +
    geom_point(color = 'steelblue', size = 2) +
    labs(
      title = 'Scatter Plot of {y_col} vs {x_col}',
      x = '{x_col}',
      y = '{y_col}'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face='bold', hjust=0.5, size=14),
      axis.text = element_text(size=11),
      axis.title = element_text(size=12)
    )
")
}

#' Generate default histogram code
#'
#' @param num_col Character string for the numeric column
#' @return A character string containing ggplot code
#'
generate_default_hist_code <- function(num_col) {
  glue::glue("
df %>%
  ggplot(aes(x = !!sym('{num_col}'))) +
    geom_histogram(fill = 'steelblue', color = 'white', bins = 30) +
    labs(
      title = 'Histogram of {num_col}',
      x = '{num_col}',
      y = 'Count'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face='bold', hjust=0.5, size=14),
      axis.text = element_text(size=11),
      axis.title = element_text(size=12)
    )
")
}

#' Generate a default aggregated bar plot code
#'
#' Groups `df` by a categorical column, computes an aggregate of a numeric column,
#' and then plots a bar chart of the result.
#'
#' @param cat_col String; name of the categorical column
#' @param num_col String; name of the numeric column
#' @param agg_fn String; one of "sum", "mean", "count", "max", or "min"
#'
#' @return A character string containing ggplot code for the aggregated bar plot
#'
generate_default_agg_bar_code <- function(cat_col, num_col, agg_fn = c("sum", "mean", "count", "max", "min")) {
  
  agg_fn <- match.arg(agg_fn)  # ensure it's one of the allowed functions
  
  summarise_expr <- switch(
    agg_fn,
    "count" = "dplyr::summarise(value = dplyr::n())",
    # For sum, mean, max, min, we apply the function to num_col
    paste0("dplyr::summarise(value = ", agg_fn, "(!!sym('", num_col, "')))")  
  )

  code <- glue::glue("
df %>%
  dplyr::group_by(!!sym('{cat_col}')) %>%
  {summarise_expr} %>%
  ggplot(aes(x = !!sym('{cat_col}'), y = value)) +
    geom_col(fill = 'steelblue', color = 'white') +
    labs(
      title = '{agg_fn} of {num_col} by {cat_col}',
      x = '{cat_col}',
      y = '{agg_fn} of {num_col}'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face='bold', hjust=0.5, size=14),
      axis.text = element_text(size=11),
      axis.title = element_text(size=12)
    )
")
  
  code
}

