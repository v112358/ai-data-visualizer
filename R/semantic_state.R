# R/semantic_state.R
# Semantic state and code-patching for the chart editor.
# Code is the single source of truth after initial generation.

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Custom theme ─────────────────────────────────────────────────────────────

theme_chart_studio <- function(base_size = 15) {
  theme_minimal(base_size = base_size, base_family = "Inter") +
  theme(
    plot.background     = element_rect(fill = "#FAFAFA", color = NA),
    panel.background    = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid.major    = element_line(color = "#E8E8E8", linewidth = 0.4),
    panel.grid.minor    = element_blank(),
    plot.title          = element_text(face = "bold", size = rel(1.2), hjust = 0,
                                       margin = margin(b = 6)),
    plot.subtitle       = element_text(color = "#666666", size = rel(0.9), hjust = 0,
                                       margin = margin(b = 12)),
    plot.title.position = "plot",
    axis.text           = element_text(color = "#555555"),
    axis.title          = element_text(color = "#333333", face = "bold", size = rel(0.85)),
    axis.line           = element_line(color = "#CCCCCC", linewidth = 0.4),
    axis.ticks          = element_line(color = "#CCCCCC", linewidth = 0.3),
    legend.position     = "right",
    legend.key.size     = unit(0.8, "lines"),
    legend.text         = element_text(size = rel(0.8), color = "#444444"),
    legend.title        = element_text(size = rel(0.85), face = "bold"),
    plot.margin         = margin(16, 20, 12, 16),
    plot.caption        = element_text(color = "#BBBBBB", size = 7, hjust = 1,
                                       margin = margin(t = 8)),
    strip.text          = element_text(face = "bold", color = "#333333")
  )
}

# ── State constructors ──────────────────────────────────────────────────────

default_line_state <- function(x_col, y_col,
                                color_by        = NULL,
                                line_color      = "#2D6A9F",
                                line_width      = 1.2,
                                show_points     = FALSE,
                                point_size      = 2.5,
                                smooth          = FALSE,
                                smooth_method   = "loess",
                                color_palette   = "default",
                                title           = NULL,
                                x_label         = NULL,
                                y_label         = NULL,
                                theme           = "chart_studio",
                                legend_pos      = "right",
                                show_gridlines  = TRUE,
                                title_size      = 18L,
                                axis_text_size  = 13L,
                                axis_title_size = 14L) {
  list(chart_type = "line", x_col = x_col, y_col = y_col,
       color_by = color_by, line_color = line_color,
       line_width = line_width, show_points = show_points, point_size = point_size,
       smooth = smooth, smooth_method = smooth_method, color_palette = color_palette,
       title = title %||% paste(y_col, "over", x_col),
       x_label = x_label %||% x_col, y_label = y_label %||% y_col,
       theme = theme, legend_pos = legend_pos, show_gridlines = show_gridlines,
       title_size = title_size, axis_text_size = axis_text_size, axis_title_size = axis_title_size)
}

default_scatter_state <- function(x_col, y_col,
                                   color_by        = NULL,
                                   line_color      = "#2D6A9F",
                                   point_size      = 2.5,
                                   point_alpha     = 0.65,
                                   point_shape     = 21L,
                                   smooth          = FALSE,
                                   smooth_method   = "loess",
                                   color_palette   = "default",
                                   title           = NULL,
                                   x_label         = NULL,
                                   y_label         = NULL,
                                   theme           = "chart_studio",
                                   legend_pos      = "right",
                                   show_gridlines  = TRUE,
                                   title_size      = 16L,
                                   axis_text_size  = 12L,
                                   axis_title_size = 13L) {
  list(chart_type = "scatter", x_col = x_col, y_col = y_col,
       color_by = color_by, line_color = line_color,
       point_size = point_size, point_alpha = point_alpha, point_shape = as.integer(point_shape),
       smooth = smooth, smooth_method = smooth_method, color_palette = color_palette,
       title = title %||% paste(y_col, "vs", x_col),
       x_label = x_label %||% x_col, y_label = y_label %||% y_col,
       theme = theme, legend_pos = legend_pos, show_gridlines = show_gridlines,
       title_size = title_size, axis_text_size = axis_text_size, axis_title_size = axis_title_size)
}

default_bar_state <- function(x_col, y_col,
                               color_by        = NULL,
                               line_color      = "#2D6A9F",
                               bar_width       = 0.7,
                               bar_position    = "dodge",
                               color_palette   = "default",
                               title           = NULL,
                               x_label         = NULL,
                               y_label         = NULL,
                               theme           = "chart_studio",
                               legend_pos      = "right",
                               show_gridlines  = TRUE,
                               title_size      = 16L,
                               axis_text_size  = 12L,
                               axis_title_size = 13L) {
  list(chart_type = "bar", x_col = x_col, y_col = y_col,
       color_by = color_by, line_color = line_color,
       bar_width = bar_width, bar_position = bar_position, color_palette = color_palette,
       title = title %||% paste(y_col, "by", x_col),
       x_label = x_label %||% x_col, y_label = y_label %||% y_col,
       theme = theme, legend_pos = legend_pos, show_gridlines = show_gridlines,
       title_size = title_size, axis_text_size = axis_text_size, axis_title_size = axis_title_size)
}

default_hist_state <- function(x_col,
                                line_color      = "#2D6A9F",
                                bins            = 30L,
                                title           = NULL,
                                x_label         = NULL,
                                y_label         = NULL,
                                theme           = "chart_studio",
                                legend_pos      = "right",
                                show_gridlines  = TRUE,
                                title_size      = 18L,
                                axis_text_size  = 13L,
                                axis_title_size = 14L) {
  list(chart_type = "hist", x_col = x_col, y_col = NULL,
       color_by = NULL, line_color = line_color, bins = as.integer(bins),
       color_palette = "default",
       title = title %||% paste("Distribution of", x_col),
       x_label = x_label %||% x_col, y_label = y_label %||% "Count",
       theme = theme, legend_pos = legend_pos, show_gridlines = show_gridlines,
       title_size = title_size, axis_text_size = axis_text_size, axis_title_size = axis_title_size)
}


# ── Code generation ─────────────────────────────────────────────────────────

state_to_code <- function(state) {
  x  <- state$x_col
  y  <- state$y_col
  cb <- state$color_by
  has_group <- !is.null(cb) && nzchar(cb) && cb != "__none__"

  parts <- switch(state$chart_type,
    "line"    = .line_parts(state, x, y, has_group),
    "scatter" = .scatter_parts(state, x, y, has_group),
    "bar"     = .bar_parts(state, x, y, has_group),
    "hist"    = .hist_parts(state, x)
  )

  labs_str <- sprintf(
    "labs(title = '%s', x = '%s', y = '%s', caption = 'Chart Studio')",
    state$title, state$x_label, state$y_label
  )

  theme_base <- if (state$theme == "chart_studio") {
    "theme_chart_studio()"
  } else {
    sprintf("theme_chart_studio() +\n  theme_%s()", state$theme)
  }

  theme_override <- sprintf(
    "theme(plot.title = element_text(face = 'bold', size = %d), axis.text = element_text(size = %d), axis.title = element_text(size = %d), legend.position = '%s')",
    state$title_size, state$axis_text_size, state$axis_title_size, state$legend_pos
  )

  y_expand <- if (state$chart_type %in% c("bar", "hist"))
    "scale_y_continuous(expand = expansion(mult = c(0, 0.05)))"
  else NULL

  layers <- Filter(Negate(is.null),
    c(parts$geoms, list(parts$palette, y_expand, labs_str, theme_base, theme_override)))
  code <- sprintf("df %%>%%\n  ggplot(%s) +\n  %s", parts$aes, paste(layers, collapse = " +\n  "))

  if (!isTRUE(state$show_gridlines))
    code <- paste0(code, " +\n  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())")

  code
}

# ── Private geom builders ───────────────────────────────────────────────────

.palette_scale <- function(state, has_group, is_fill) {
  if (!has_group) return(NULL)
  pal <- state$color_palette %||% "default"
  pfx <- if (is_fill) "scale_fill" else "scale_color"
  if (pal == "default") {
    vals <- c('#2D6A9F','#E8534A','#3DAA6E','#F5A623','#9B59B6','#1ABC9C')
    return(sprintf("%s_manual(values = c('%s'))", pfx, paste(vals, collapse = "','")))
  }
  switch(pal,
    "viridis" = sprintf("%s_viridis_d()", pfx),
    "plasma"  = sprintf("%s_viridis_d(option = 'plasma')", pfx),
    "okabe"   = sprintf("%s_manual(values = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7'))", pfx),
    sprintf("%s_brewer(palette = '%s')", pfx, pal)
  )
}

.line_parts <- function(state, x, y, has_group) {
  aes <- if (has_group)
    sprintf("aes(x = !!sym('%s'), y = !!sym('%s'), color = !!sym('%s'), group = !!sym('%s'))", x, y, state$color_by, state$color_by)
  else
    sprintf("aes(x = !!sym('%s'), y = !!sym('%s'))", x, y)

  geom_l <- if (has_group)
    sprintf("geom_line(linewidth = %.1f, lineend = 'round')", state$line_width)
  else
    sprintf("geom_line(color = '%s', linewidth = %.1f, lineend = 'round')", state$line_color, state$line_width)

  geom_p <- if (isTRUE(state$show_points)) {
    if (has_group) sprintf("geom_point(size = %.1f)", state$point_size)
    else           sprintf("geom_point(color = '%s', size = %.1f)", state$line_color, state$point_size)
  } else NULL

  smooth <- if (isTRUE(state$smooth))
    sprintf("geom_smooth(method = '%s', se = FALSE, linetype = 'dashed', color = 'gray50')", state$smooth_method)
  else NULL

  list(aes = aes,
       geoms = Filter(Negate(is.null), list(geom_l, geom_p, smooth)),
       palette = .palette_scale(state, has_group, is_fill = FALSE))
}

.scatter_parts <- function(state, x, y, has_group) {
  if (has_group) {
    aes    <- sprintf("aes(x = !!sym('%s'), y = !!sym('%s'), fill = !!sym('%s'))", x, y, state$color_by)
    geom_p <- sprintf("geom_point(size = %.1f, alpha = %.2f, shape = 21, stroke = 0.4)",
                      state$point_size, state$point_alpha)
    pal    <- .palette_scale(state, has_group, is_fill = TRUE)
  } else {
    aes    <- sprintf("aes(x = !!sym('%s'), y = !!sym('%s'))", x, y)
    geom_p <- sprintf("geom_point(fill = '%s', color = 'white', size = %.1f, alpha = %.2f, shape = 21, stroke = 0.4)",
                      state$line_color, state$point_size, state$point_alpha)
    pal    <- NULL
  }

  smooth <- if (isTRUE(state$smooth))
    sprintf("geom_smooth(method = '%s', se = FALSE, linetype = 'dashed', color = 'gray50')", state$smooth_method)
  else NULL

  list(aes = aes,
       geoms = Filter(Negate(is.null), list(geom_p, smooth)),
       palette = pal)
}

.bar_parts <- function(state, x, y, has_group) {
  aes <- if (has_group)
    sprintf("aes(x = !!sym('%s'), y = !!sym('%s'), fill = !!sym('%s'))", x, y, state$color_by)
  else
    sprintf("aes(x = !!sym('%s'), y = !!sym('%s'))", x, y)

  geom_b <- if (has_group)
    sprintf("geom_col(position = '%s', width = %.2f, color = NA)", state$bar_position, state$bar_width)
  else
    sprintf("geom_col(fill = '%s', width = %.2f, color = NA)", state$line_color, state$bar_width)

  list(aes = aes,
       geoms = list(geom_b),
       palette = .palette_scale(state, has_group, is_fill = TRUE))
}

.hist_parts <- function(state, x) {
  list(
    aes     = sprintf("aes(x = !!sym('%s'))", x),
    geoms   = list(sprintf("geom_histogram(fill = '%s', color = NA, alpha = 0.85, bins = %d)",
                           state$line_color, state$bins)),
    palette = NULL
  )
}


# ── Surgical patch functions ────────────────────────────────────────────────

.q    <- "['\"]"
.qcap <- "['\"]([^'\"]+)['\"]"

patch_color <- function(code, new_color) {
  code <- gsub(paste0("(geom_line\\([^)]*color\\s*=\\s*)", .q, "[^'\"]*", .q),     paste0("\\1'", new_color, "'"), code)
  # shape 21 scatter uses fill for the visible color; fall through to color only for line points
  code_fp <- gsub(paste0("(geom_point\\([^)]*fill\\s*=\\s*)", .q, "[^'\"]*", .q),  paste0("\\1'", new_color, "'"), code)
  if (!identical(code_fp, code)) {
    code <- code_fp
  } else {
    code <- gsub(paste0("(geom_point\\([^)]*color\\s*=\\s*)", .q, "[^'\"]*", .q),  paste0("\\1'", new_color, "'"), code)
  }
  code <- gsub(paste0("(geom_col\\([^)]*fill\\s*=\\s*)", .q, "[^'\"]*", .q),       paste0("\\1'", new_color, "'"), code)
  code <- gsub(paste0("(geom_histogram\\([^)]*fill\\s*=\\s*)", .q, "[^'\"]*", .q), paste0("\\1'", new_color, "'"), code)
  code
}

patch_palette <- function(code, new_palette) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  lines <- lines[!grepl("^\\s*scale_(color|fill)_", lines)]
  code  <- paste(lines, collapse = "\n")

  is_fill   <- grepl("fill\\s*=\\s*!!sym", code)
  pfx       <- if (is_fill) "scale_fill" else "scale_color"
  new_scale <- if (new_palette == "default") {
    vals <- c('#2D6A9F','#E8534A','#3DAA6E','#F5A623','#9B59B6','#1ABC9C')
    sprintf("%s_manual(values = c('%s'))", pfx, paste(vals, collapse = "','"))
  } else switch(new_palette,
    "viridis" = sprintf("%s_viridis_d()", pfx),
    "plasma"  = sprintf("%s_viridis_d(option = 'plasma')", pfx),
    "okabe"   = sprintf("%s_manual(values = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7'))", pfx),
    sprintf("%s_brewer(palette = '%s')", pfx, new_palette)
  )
  sub("( \\+\n  labs\\()", paste0(" +\n  ", new_scale, "\\1"), code)
}

patch_line_width <- function(code, new_width) {
  gsub("(linewidth\\s*=\\s*)[0-9.]+", paste0("\\1", sprintf("%.1f", new_width)), code)
}

patch_point_size <- function(code, new_size) {
  gsub("(geom_point\\([^)]*size\\s*=\\s*)[0-9.]+", paste0("\\1", sprintf("%.1f", new_size)), code)
}

patch_alpha <- function(code, new_alpha) {
  gsub("(geom_point\\([^)]*alpha\\s*=\\s*)[0-9.]+", paste0("\\1", sprintf("%.2f", new_alpha)), code)
}

patch_point_shape <- function(code, new_shape) {
  if (grepl("geom_point\\([^)]*shape\\s*=\\s*[0-9]+", code))
    gsub("(geom_point\\([^)]*shape\\s*=\\s*)[0-9]+", paste0("\\1", as.integer(new_shape)), code)
  else
    gsub("(geom_point\\([^)]*)(\\))", paste0("\\1, shape = ", as.integer(new_shape), "\\2"), code)
}

patch_bar_width <- function(code, new_width) {
  gsub("(geom_col\\([^)]*width\\s*=\\s*)[0-9.]+", paste0("\\1", sprintf("%.2f", new_width)), code)
}

patch_bar_position <- function(code, new_pos) {
  gsub(paste0("(geom_col\\([^)]*position\\s*=\\s*)", .q, "[^'\"]*", .q),
       paste0("\\1'", new_pos, "'"), code)
}

patch_bins <- function(code, new_bins) {
  gsub("(geom_histogram\\([^)]*bins\\s*=\\s*)[0-9]+", paste0("\\1", as.integer(new_bins)), code)
}

patch_theme <- function(code, new_theme) {
  if (new_theme == "chart_studio") {
    # Remove any standard theme layered after chart_studio
    gsub(" \\+\n  theme_(minimal|classic|bw|light|dark)\\(\\)", "", code)
  } else if (grepl("theme_(minimal|classic|bw|light|dark)\\(\\)", code)) {
    # Replace existing standard theme layer
    gsub("theme_(minimal|classic|bw|light|dark)\\(\\)", paste0("theme_", new_theme, "()"), code)
  } else {
    # chart_studio only — insert new theme after it
    sub("(theme_chart_studio\\(\\))", paste0("\\1 +\n  theme_", new_theme, "()"), code)
  }
}

patch_legend_pos <- function(code, new_pos) {
  gsub(paste0("(legend\\.position\\s*=\\s*)", .q, "[^'\"]*", .q),
       paste0("\\1'", new_pos, "'"), code)
}

patch_gridlines <- function(code, show) {
  # Use panel.grid.major explicitly — setting the parent panel.grid doesn't override
  # child elements that theme_chart_studio() set explicitly (ggplot2 inheritance rule).
  has_off <- grepl("panel\\.grid\\.major = element_blank", code)
  if (!show && !has_off)
    code <- paste0(trimws(code), " +\n  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())")
  if ( show &&  has_off)
    code <- gsub(" \\+\n  theme\\(panel\\.grid\\.major = element_blank\\(\\), panel\\.grid\\.minor = element_blank\\(\\)\\)", "", code)
  code
}

patch_title_size <- function(code, new_size) {
  gsub("(plot\\.title\\s*=\\s*element_text\\([^)]*size\\s*=\\s*)[0-9]+",
       paste0("\\1", as.integer(new_size)), code)
}

patch_axis_text_size <- function(code, new_size) {
  gsub("(axis\\.text\\s*=\\s*element_text\\([^)]*size\\s*=\\s*)[0-9]+",
       paste0("\\1", as.integer(new_size)), code)
}

patch_axis_title_size <- function(code, new_size) {
  gsub("(axis\\.title\\s*=\\s*element_text\\([^)]*size\\s*=\\s*)[0-9]+",
       paste0("\\1", as.integer(new_size)), code)
}

patch_labs <- function(code, title = NULL, x_label = NULL, y_label = NULL) {
  m <- regexpr("labs\\([^)]+\\)", code)
  if (m == -1) return(code)
  labs_str <- regmatches(code, m)
  escape   <- function(s) gsub("'", "\\\\'", s)
  qpat     <- paste0(.q, "[^'\"]*", .q)
  if (!is.null(title))
    labs_str <- gsub(paste0("(\\btitle\\s*=\\s*)", qpat), paste0("\\1'", escape(title), "'"), labs_str)
  if (!is.null(x_label))
    labs_str <- gsub(paste0("(,\\s*x\\s*=\\s*)", qpat), paste0("\\1'", escape(x_label), "'"), labs_str)
  if (!is.null(y_label))
    labs_str <- gsub(paste0("(,\\s*y\\s*=\\s*)", qpat), paste0("\\1'", escape(y_label), "'"), labs_str)
  regmatches(code, m) <- labs_str
  code
}

patch_show_points <- function(code, show, line_color, point_size) {
  has_points <- grepl("geom_point", code)
  if (show && !has_points) {
    layer <- sprintf("geom_point(color = '%s', size = %.1f)", line_color, point_size)
    code  <- sub("(geom_line\\([^)]*\\))(\\s*\\+)", paste0("\\1 +\n  ", layer, "\\2"), code)
    if (!grepl("geom_point", code))
      code <- sub("(geom_line\\([^)]*\\))", paste0("\\1 +\n  ", layer), code)
  } else if (!show && has_points) {
    code <- gsub("\\s*\\+\\s*\n?\\s*geom_point\\([^)]*\\)", "", code)
  }
  code
}

patch_smooth <- function(code, show, method) {
  has_smooth <- grepl("geom_smooth", code)
  if (show && !has_smooth) {
    layer  <- sprintf("geom_smooth(method = '%s', se = FALSE, linetype = 'dashed', color = 'gray50')", method)
    anchor <- if (grepl("geom_point", code)) "geom_point" else "geom_line"
    code   <- sub(paste0("(", anchor, "\\([^)]*\\))(\\s*\\+)"), paste0("\\1 +\n  ", layer, "\\2"), code)
    if (!grepl("geom_smooth", code))
      code <- sub(paste0("(", anchor, "\\([^)]*\\))"), paste0("\\1 +\n  ", layer), code)
  } else if (!show && has_smooth) {
    code <- gsub("\\s*\\+\\s*\n?\\s*geom_smooth\\([^)]*\\)", "", code)
  } else if (show && has_smooth) {
    code <- gsub(paste0("(geom_smooth\\([^)]*method\\s*=\\s*)", .q, "[^'\"]*", .q),
                 paste0("\\1'", method, "'"), code)
  }
  code
}


# ── Parse code → UI values ──────────────────────────────────────────────────

extract_ui_values <- function(code) {
  out <- list()

  # color: shape 21 scatter uses fill in geom_point; line points use color
  for (pat in c("geom_line\\([^)]*color\\s*=\\s*",
                "geom_point\\([^)]*fill\\s*=\\s*",
                "geom_point\\([^)]*color\\s*=\\s*",
                "geom_col\\([^)]*fill\\s*=\\s*",
                "geom_histogram\\([^)]*fill\\s*=\\s*")) {
    m <- regmatches(code, regexpr(paste0(pat, .qcap), code))
    if (length(m) > 0) { out$line_color <- sub(paste0(".*[=]\\s*", .qcap, ".*"), "\\1", m); break }
  }

  m <- regmatches(code, regexpr("linewidth\\s*=\\s*([0-9.]+)", code))
  if (length(m) > 0) out$line_width <- as.numeric(sub(".*=\\s*([0-9.]+).*", "\\1", m))

  out$show_points <- grepl("geom_point", code) && grepl("geom_line", code)
  out$smooth      <- grepl("geom_smooth", code)
  if (isTRUE(out$smooth)) {
    m <- regmatches(code, regexpr(paste0("geom_smooth\\([^)]*method\\s*=\\s*", .qcap), code))
    if (length(m) > 0) out$smooth_method <- sub(paste0(".*method\\s*=\\s*", .qcap, ".*"), "\\1", m)
  }

  m <- regmatches(code, regexpr("geom_point\\([^)]*alpha\\s*=\\s*([0-9.]+)", code))
  if (length(m) > 0) out$point_alpha <- as.numeric(sub(".*alpha\\s*=\\s*([0-9.]+).*", "\\1", m))

  m <- regmatches(code, regexpr("geom_point\\([^)]*shape\\s*=\\s*([0-9]+)", code))
  if (length(m) > 0) out$point_shape <- as.integer(sub(".*shape\\s*=\\s*([0-9]+).*", "\\1", m))

  m <- regmatches(code, regexpr("geom_point\\([^)]*size\\s*=\\s*([0-9.]+)", code))
  if (length(m) > 0) out$point_size <- as.numeric(sub(".*size\\s*=\\s*([0-9.]+).*", "\\1", m))

  m <- regmatches(code, regexpr("geom_col\\([^)]*width\\s*=\\s*([0-9.]+)", code))
  if (length(m) > 0) out$bar_width <- as.numeric(sub(".*width\\s*=\\s*([0-9.]+).*", "\\1", m))

  m <- regmatches(code, regexpr(paste0("geom_col\\([^)]*position\\s*=\\s*", .qcap), code))
  if (length(m) > 0) out$bar_position <- sub(paste0(".*position\\s*=\\s*", .qcap, ".*"), "\\1", m)

  m <- regmatches(code, regexpr("geom_histogram\\([^)]*bins\\s*=\\s*([0-9]+)", code))
  if (length(m) > 0) out$bins <- as.integer(sub(".*bins\\s*=\\s*([0-9]+).*", "\\1", m))

  # color palette: distinguish curated default from okabe by first hex value
  out$color_palette <- if (grepl("scale_(color|fill)_brewer", code)) {
    m <- regmatches(code, regexpr(paste0("palette\\s*=\\s*", .qcap), code))
    if (length(m) > 0) sub(paste0(".*palette\\s*=\\s*", .qcap, ".*"), "\\1", m) else "default"
  } else if (grepl("option\\s*=\\s*'plasma'", code)) {
    "plasma"
  } else if (grepl("scale_(color|fill)_viridis_d", code)) {
    "viridis"
  } else if (grepl("scale_(color|fill)_manual", code)) {
    if (grepl("#2D6A9F", code)) "default" else "okabe"
  } else {
    "default"
  }

  # theme: default chart_studio; override if a standard theme layer present
  out$theme <- "chart_studio"
  for (th in c("minimal", "classic", "bw", "light", "dark")) {
    if (grepl(paste0("theme_", th, "\\("), code)) { out$theme <- th; break }
  }

  m <- regmatches(code, regexpr(paste0("legend\\.position\\s*=\\s*", .qcap), code))
  if (length(m) > 0) out$legend_pos <- sub(paste0(".*=\\s*", .qcap, ".*"), "\\1", m)

  out$show_gridlines <- !grepl("panel\\.grid\\.major = element_blank", code)

  labs_m <- regmatches(code, regexpr("labs\\([^)]+\\)", code))
  if (length(labs_m) > 0) {
    lc <- labs_m[1]
    m  <- regmatches(lc, regexpr(paste0("\\btitle\\s*=\\s*", .qcap), lc))
    if (length(m) > 0) out$title <- sub(paste0(".*=\\s*", .qcap, ".*"), "\\1", m)
    m  <- regmatches(lc, regexpr(paste0(",\\s*x\\s*=\\s*", .qcap), lc))
    if (length(m) > 0) out$x_label <- sub(paste0(".*=\\s*", .qcap, ".*"), "\\1", m)
    m  <- regmatches(lc, regexpr(paste0(",\\s*y\\s*=\\s*", .qcap), lc))
    if (length(m) > 0) out$y_label <- sub(paste0(".*=\\s*", .qcap, ".*"), "\\1", m)
  }

  m <- regmatches(code, regexpr("plot\\.title[^,)]*size\\s*=\\s*([0-9]+)", code))
  if (length(m) > 0) out$title_size <- as.integer(sub(".*size\\s*=\\s*([0-9]+).*", "\\1", m))

  m <- regmatches(code, regexpr("axis\\.text[^,)]*size\\s*=\\s*([0-9]+)", code))
  if (length(m) > 0) out$axis_text_size <- as.integer(sub(".*size\\s*=\\s*([0-9]+).*", "\\1", m))

  m <- regmatches(code, regexpr("axis\\.title[^,)]*size\\s*=\\s*([0-9]+)", code))
  if (length(m) > 0) out$axis_title_size <- as.integer(sub(".*size\\s*=\\s*([0-9]+).*", "\\1", m))

  out
}
