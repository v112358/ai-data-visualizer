library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(stringr)
library(ellmer)
library(here)
library(tibble)

source("R/semantic_state.R")
source("R/llm_helpers.R")
source("R/logging.R")

llm <- create_llm_object()

# ── UI ─────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  title = "Chart Studio",

  tags$head(tags$style(HTML("
    * { box-sizing: border-box; }
    body { background: #f0f2f5; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 0; }
    .app-header { background: #1a1a2e; color: #fff; padding: 10px 20px; display: flex; align-items: center; gap: 12px; margin-bottom: 16px; }
    .app-header h4 { margin: 0; font-size: 16px; font-weight: 600; }
    .app-header .subtitle { font-size: 12px; color: #8888aa; margin-left: 4px; }
    .panel-card { background: #fff; border-radius: 10px; padding: 14px; box-shadow: 0 1px 6px rgba(0,0,0,0.08); }
    .chart-canvas { background: #fff; border-radius: 10px; padding: 16px 16px 10px; box-shadow: 0 1px 6px rgba(0,0,0,0.08); }
    .panel-label { font-size: 11px; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.6px; margin-bottom: 8px; margin-top: 6px; }
    .status-bar { font-size: 11px; color: #6c757d; margin-top: 6px; min-height: 16px; }
    .ctrl-row { margin-bottom: 8px; }
    .ctrl-row label { font-size: 12px !important; font-weight: 500; color: #343a40; }
    .undo-row { display: flex; gap: 6px; margin-top: 8px; }
    .ai-history { max-height: 200px; overflow-y: auto; margin-top: 8px; }
    .ai-item { background: #f8f9fa; border-left: 3px solid #3a86ff; border-radius: 0 4px 4px 0; padding: 6px 8px; margin-bottom: 6px; font-size: 11px; color: #495057; }
    hr.thin { margin: 10px 0; border-color: #f0f0f0; }
    .form-control, .selectize-input { font-size: 13px !important; }
    .btn { font-size: 12px; }
    .shiny-input-container { margin-bottom: 6px; }
    .shiny-input-container label { font-size: 12px !important; font-weight: 500; color: #343a40; }
    input[type='color'] { width: 100%; height: 36px; border-radius: 6px; border: 1px solid #dee2e6; cursor: pointer; padding: 2px 4px; }
  "))),

  tags$script(HTML("
    window.paletteSwatches = {
      'default': ['#F8766D','#00BA38','#619CFF'],
      'Set1':    ['#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00'],
      'Set2':    ['#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854'],
      'Dark2':   ['#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E'],
      'Paired':  ['#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99'],
      'viridis': ['#440154','#31688E','#35B779','#FDE725'],
      'plasma':  ['#0D0887','#7E03A8','#CC4678','#F89441','#F0F921'],
      'okabe':   ['#E69F00','#56B4E9','#009E73','#F0E442','#0072B2','#D55E00','#CC79A7']
    };
    document.addEventListener('DOMContentLoaded', function() {
      var el = document.getElementById('lineColor');
      if (el) el.addEventListener('input', function() {
        Shiny.setInputValue('lineColor', this.value);
      });
    });
    Shiny.addCustomMessageHandler('updateColorInput', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) { el.value = msg.value; Shiny.setInputValue(msg.id, msg.value); }
    });
  ")),

  div(class = "app-header",
    tags$svg(xmlns="http://www.w3.org/2000/svg", width="20", height="20", viewBox="0 0 24 24",
             fill="none", stroke="#3a86ff", `stroke-width`="2",
             tags$polyline(points="22 12 18 12 15 21 9 3 6 12 2 12")),
    h4("Chart Studio"),
    span(class = "subtitle", "AI-Assisted Graph Editor")
  ),

  fluidRow(style = "margin: 0 8px;",

    # ── Left: Data + chart type panel ────────────────────────────────────
    column(2, style = "padding: 0 6px;",
      div(class = "panel-card",

        div(class = "panel-label", "Chart Type"),
        div(class = "ctrl-row",
          selectInput("chartType", NULL,
                      choices = c("Line", "Scatter", "Bar", "Histogram"),
                      selected = "Line")
        ),

        hr(class = "thin"),
        div(class = "panel-label", "Data"),
        fileInput("file", NULL, accept = ".csv", buttonLabel = "Browse"),
        actionButton("useToyData", "Use iris sample",
                     class = "btn btn-outline-secondary btn-sm",
                     style = "width:100%; margin-bottom:10px;"),

        hr(class = "thin"),
        div(class = "panel-label", "Columns"),
        uiOutput("col_selectors")
      )
    ),

    # ── Center: Chart canvas ──────────────────────────────────────────────
    column(7, style = "padding: 0 6px;",
      div(class = "chart-canvas",
        plotOutput("plot", height = "470px"),
        div(class = "undo-row",
          actionButton("undoBtn", "↩ Undo", class = "btn btn-outline-secondary btn-sm"),
          actionButton("redoBtn", "↪ Redo", class = "btn btn-outline-secondary btn-sm")
        ),
        div(class = "status-bar", textOutput("statusMsg", inline = TRUE))
      )
    ),

    # ── Right: Edit panel ─────────────────────────────────────────────────
    column(3, style = "padding: 0 6px;",
      div(class = "panel-card", style = "padding: 10px;",
        tabsetPanel(id = "editTabs", type = "tabs",

          # ── Style tab ────────────────────────────────────────────────────
          tabPanel("Style",
            div(style = "padding-top: 10px;",

              # Color — single picker when ungrouped, palette selector when grouped
              div(class = "panel-label", "Color"),
              conditionalPanel(
                "!input.colorBy || input.colorBy === '__none__' || input.chartType === 'Histogram'",
                div(class = "ctrl-row",
                  tags$label("Fill / Line color",
                    style = "font-size:12px; font-weight:500; color:#343a40; display:block; margin-bottom:4px;"),
                  tags$input(id = "lineColor", type = "color", value = "#3a86ff")
                )
              ),
              conditionalPanel(
                "input.colorBy && input.colorBy !== '__none__' && input.chartType !== 'Histogram'",
                div(class = "ctrl-row",
                  selectizeInput("colorPalette", "Color palette",
                    choices = c(
                      "ggplot2 default"             = "default",
                      "ColorBrewer: Set1"           = "Set1",
                      "ColorBrewer: Set2"           = "Set2",
                      "ColorBrewer: Dark2"          = "Dark2",
                      "ColorBrewer: Paired"         = "Paired",
                      "Viridis"                     = "viridis",
                      "Plasma"                      = "plasma",
                      "Okabe-Ito (colorblind-safe)" = "okabe"
                    ),
                    selected = "default",
                    options = list(render = I('{
                      option: function(item, escape) {
                        var sw = (window.paletteSwatches[item.value] || []).map(function(c) {
                          return "<span style=\'display:inline-block;width:11px;height:11px;border-radius:2px;margin-right:2px;vertical-align:middle;background:" + c + "\'></span>";
                        }).join("");
                        return "<div style=\'padding:3px 6px\'>" + sw + " <span style=\'vertical-align:middle\'>" + escape(item.label) + "</span></div>";
                      },
                      item: function(item, escape) {
                        var sw = (window.paletteSwatches[item.value] || []).map(function(c) {
                          return "<span style=\'display:inline-block;width:11px;height:11px;border-radius:2px;margin-right:2px;vertical-align:middle;background:" + c + "\'></span>";
                        }).join("");
                        return "<div>" + sw + " " + escape(item.label) + "</div>";
                      }
                    }'))
                  )
                )
              ),

              hr(class = "thin"),

              # Line-specific
              conditionalPanel("input.chartType == 'Line'",
                div(class = "panel-label", "Line"),
                div(class = "ctrl-row",
                  sliderInput("lineWidth", "Width", 0.3, 4, 1.0, 0.1, ticks = FALSE)
                ),
                hr(class = "thin"),
                div(class = "panel-label", "Points"),
                div(class = "ctrl-row",
                  checkboxInput("showPoints", "Show data points", FALSE)
                ),
                conditionalPanel("input.showPoints == true",
                  div(class = "ctrl-row",
                    sliderInput("linePointSize", "Point size", 0.5, 6, 2.5, 0.5, ticks = FALSE)
                  )
                )
              ),

              # Scatter-specific
              conditionalPanel("input.chartType == 'Scatter'",
                div(class = "panel-label", "Points"),
                div(class = "ctrl-row",
                  sliderInput("scatterPointSize", "Size",    0.5, 8,   2.5, 0.5,  ticks = FALSE)
                ),
                div(class = "ctrl-row",
                  sliderInput("pointAlpha",       "Opacity", 0.05, 1.0, 0.7, 0.05, ticks = FALSE)
                ),
                div(class = "ctrl-row",
                  selectInput("pointShape", "Shape",
                              choices = c("● Filled circle" = 16, "○ Open circle" = 1,
                                          "■ Filled square" = 15, "▲ Triangle" = 17,
                                          "◆ Diamond" = 18),
                              selected = 16)
                )
              ),

              # Bar-specific
              conditionalPanel("input.chartType == 'Bar'",
                div(class = "panel-label", "Bars"),
                div(class = "ctrl-row",
                  sliderInput("barWidth", "Bar width", 0.1, 1.0, 0.7, 0.05, ticks = FALSE)
                ),
                div(class = "ctrl-row",
                  selectInput("barPosition", "Grouped position",
                              choices = c("Side by side" = "dodge",
                                          "Stacked"      = "stack",
                                          "Proportional" = "fill"),
                              selected = "dodge")
                )
              ),

              # Histogram-specific
              conditionalPanel("input.chartType == 'Histogram'",
                div(class = "panel-label", "Bins"),
                div(class = "ctrl-row",
                  sliderInput("histBins", "Number of bins", 5, 100, 30, 1, ticks = FALSE)
                )
              ),

              # Trend line — Line and Scatter only
              conditionalPanel("input.chartType == 'Line' || input.chartType == 'Scatter'",
                hr(class = "thin"),
                div(class = "panel-label", "Trend Line"),
                div(class = "ctrl-row",
                  checkboxInput("addSmooth", "Add trend line", FALSE)
                ),
                conditionalPanel("input.addSmooth == true",
                  div(class = "ctrl-row",
                    selectInput("smoothMethod", "Method",
                                choices = c("loess", "lm", "gam"), selected = "loess")
                  )
                )
              ),

              hr(class = "thin"),
              div(class = "panel-label", "Theme"),
              div(class = "ctrl-row",
                selectInput("chartTheme", NULL,
                            choices = c("Minimal" = "minimal", "Classic" = "classic",
                                        "Black & White" = "bw", "Light" = "light",
                                        "Dark" = "dark"),
                            selected = "minimal")
              ),
              div(class = "ctrl-row",
                selectInput("legendPos", "Legend",
                            choices = c("Right" = "right", "Bottom" = "bottom",
                                        "Left" = "left", "Top" = "top", "None" = "none"),
                            selected = "right")
              ),
              div(class = "ctrl-row",
                checkboxInput("showGridlines", "Show gridlines", TRUE)
              )
            )
          ),

          # ── Labels tab ───────────────────────────────────────────────────
          tabPanel("Labels",
            div(style = "padding-top: 10px;",
              div(class = "panel-label", "Text"),
              div(class = "ctrl-row", textInput("chartTitle", "Title",  value = "")),
              div(class = "ctrl-row", textInput("xLabel",     "X axis", value = "")),
              div(class = "ctrl-row", textInput("yLabel",     "Y axis", value = "")),
              hr(class = "thin"),
              div(class = "panel-label", "Sizes"),
              div(class = "ctrl-row",
                sliderInput("titleSize",    "Title",     8, 24, 14, 1, ticks = FALSE)
              ),
              div(class = "ctrl-row",
                sliderInput("axisTextSize", "Axis text", 6, 18, 11, 1, ticks = FALSE)
              )
            )
          ),

          # ── AI Edit tab ──────────────────────────────────────────────────
          tabPanel("AI Edit",
            div(style = "padding-top: 10px;",
              div(class = "panel-label", "Natural Language Edit"),
              textAreaInput("aiRequest", NULL,
                            placeholder = "e.g., use a dark theme, add a trend line, flip the axes, color bars by Species",
                            height = "90px"),
              actionButton("applyAI", "Apply", class = "btn btn-primary btn-sm",
                           style = "width: 100%;"),
              hr(class = "thin"),
              div(class = "panel-label", "Edit History"),
              div(class = "ai-history", uiOutput("aiHistory"))
            )
          )
        )
      )
    )
  )
)


# ── Server ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  user_id <- paste0("session_", substr(session$token, 1, 8))
  log_to_gsheet("session_start", "", user_id = user_id)

  current_code <- reactiveVal("")
  undo_stack   <- reactiveVal(list())
  redo_stack   <- reactiveVal(list())
  ai_history   <- reactiveVal(list())
  status_text  <- reactiveVal("")
  data_source  <- reactiveVal("upload")

  # ── Dataset ─────────────────────────────────────────────────────────────
  observeEvent(input$useToyData, { data_source("toy");    log_to_gsheet("data_source", "toy",    user_id) })
  observeEvent(input$file,       { data_source("upload"); log_to_gsheet("data_source", "upload", user_id) })

  dataset <- reactive({
    if (data_source() == "toy")
      iris %>% rownames_to_column("index") %>% mutate(index = as.numeric(index))
    else { req(input$file); readr::read_csv(input$file$datapath, show_col_types = FALSE) }
  })

  numericCols <- reactive({
    req(dataset()); names(dataset())[sapply(dataset(), is.numeric)]
  })
  categoricalCols <- reactive({
    req(dataset())
    names(dataset())[sapply(dataset(), function(x) is.character(x) || is.factor(x))]
  })

  # ── Column selectors (chart-type aware) ──────────────────────────────────
  output$col_selectors <- renderUI({
    req(numericCols())
    nc <- numericCols()
    cc <- categoricalCols()
    ct <- input$chartType %||% "Line"

    if (ct %in% c("Line", "Scatter")) {
      tagList(
        selectInput("lineX",   "X axis",            choices = nc, selected = nc[1]),
        selectInput("lineY",   "Y axis",             choices = nc, selected = nc[min(2, length(nc))]),
        selectInput("colorBy", "Color by (group)",   choices = c("None" = "__none__", cc), selected = "__none__")
      )
    } else if (ct == "Bar") {
      x_choices <- if (length(cc) > 0) c(cc, nc) else nc   # prefer categorical
      tagList(
        selectInput("lineX",   "X axis (category)",  choices = x_choices, selected = x_choices[1]),
        selectInput("lineY",   "Y axis (numeric)",   choices = nc, selected = nc[1]),
        selectInput("colorBy", "Color by (group)",   choices = c("None" = "__none__", cc), selected = "__none__")
      )
    } else {  # Histogram
      tagList(
        selectInput("lineX", "Column", choices = nc, selected = nc[1])
      )
    }
  })

  # ── Helpers ──────────────────────────────────────────────────────────────

  push_undo <- function(code) {
    if (!nzchar(code)) return()
    undo_stack(head(c(list(code), undo_stack()), 20))
    redo_stack(list())
  }

  sync_ui_from_code <- function(code) {
    v <- extract_ui_values(code)
    if (!is.null(v$line_color))     session$sendCustomMessage("updateColorInput", list(id = "lineColor", value = v$line_color))
    if (!is.null(v$line_width))     updateSliderInput(session,   "lineWidth",        value = v$line_width)
    if (!is.null(v$show_points))    updateCheckboxInput(session, "showPoints",       value = v$show_points)
    if (!is.null(v$point_size))     updateSliderInput(session,   "linePointSize",    value = v$point_size)
    if (!is.null(v$point_size))     updateSliderInput(session,   "scatterPointSize", value = v$point_size)
    if (!is.null(v$point_alpha))    updateSliderInput(session,   "pointAlpha",       value = v$point_alpha)
    if (!is.null(v$point_shape))    updateSelectInput(session,   "pointShape",       selected = as.character(v$point_shape))
    if (!is.null(v$bar_width))      updateSliderInput(session,   "barWidth",         value = v$bar_width)
    if (!is.null(v$bar_position))   updateSelectInput(session,   "barPosition",      selected = v$bar_position)
    if (!is.null(v$bins))           updateSliderInput(session,   "histBins",         value = v$bins)
    if (!is.null(v$smooth))         updateCheckboxInput(session, "addSmooth",        value = v$smooth)
    if (!is.null(v$smooth_method))  updateSelectInput(session,   "smoothMethod",     selected = v$smooth_method)
    if (!is.null(v$theme))          updateSelectInput(session,   "chartTheme",       selected = v$theme)
    if (!is.null(v$legend_pos))     updateSelectInput(session,   "legendPos",        selected = v$legend_pos)
    if (!is.null(v$show_gridlines)) updateCheckboxInput(session, "showGridlines",    value = v$show_gridlines)
    if (!is.null(v$color_palette)) updateSelectInput(session,   "colorPalette",     selected = v$color_palette)
    if (!is.null(v$title))          updateTextInput(session,     "chartTitle",       value = v$title)
    if (!is.null(v$x_label))        updateTextInput(session,     "xLabel",           value = v$x_label)
    if (!is.null(v$y_label))        updateTextInput(session,     "yLabel",           value = v$y_label)
    if (!is.null(v$title_size))     updateSliderInput(session,   "titleSize",        value = v$title_size)
    if (!is.null(v$axis_text_size)) updateSliderInput(session,   "axisTextSize",     value = v$axis_text_size)
  }

  # Gather all style inputs into a plain list (call inside reactive context)
  read_style <- function(ct = NULL) {
    ct <- ct %||% isolate(input$chartType) %||% "Line"
    list(
      chart_type     = ct,
      color_by       = { cb <- isolate(input$colorBy); if (!is.null(cb) && cb != "__none__") cb else NULL },
      line_color     = isolate(input$lineColor)        %||% "#3a86ff",
      line_width     = isolate(input$lineWidth)        %||% 1.0,
      show_points    = isTRUE(isolate(input$showPoints)),
      point_size     = if (ct == "Line")    isolate(input$linePointSize)    %||% 2.5
                       else                 isolate(input$scatterPointSize) %||% 2.5,
      point_alpha    = isolate(input$pointAlpha)       %||% 0.7,
      point_shape    = as.integer(isolate(input$pointShape) %||% 16L),
      bar_width      = isolate(input$barWidth)         %||% 0.7,
      bar_position   = isolate(input$barPosition)      %||% "dodge",
      bins           = as.integer(isolate(input$histBins) %||% 30L),
      color_palette  = isolate(input$colorPalette)       %||% "default",
      smooth         = isTRUE(isolate(input$addSmooth)),
      smooth_method  = isolate(input$smoothMethod)     %||% "loess",
      title          = { t <- isolate(input$chartTitle); if (isTruthy(t)) t else NULL },
      x_label        = { x <- isolate(input$xLabel);    if (isTruthy(x)) x else NULL },
      y_label        = { y <- isolate(input$yLabel);    if (isTruthy(y)) y else NULL },
      theme          = isolate(input$chartTheme)        %||% "minimal",
      legend_pos     = isolate(input$legendPos)         %||% "right",
      show_gridlines = isTRUE(isolate(input$showGridlines) %||% TRUE),
      title_size     = as.integer(isolate(input$titleSize)     %||% 14L),
      axis_text_size = as.integer(isolate(input$axisTextSize)  %||% 11L)
    )
  }

  build_state <- function(x_col, y_col = NULL, s) {
    switch(s$chart_type,
      "Line" = default_line_state(
        x_col=x_col, y_col=y_col, color_by=s$color_by,
        line_color=s$line_color, line_width=s$line_width,
        show_points=s$show_points, point_size=s$point_size,
        smooth=s$smooth, smooth_method=s$smooth_method,
        color_palette=s$color_palette,
        title=s$title, x_label=s$x_label, y_label=s$y_label,
        theme=s$theme, legend_pos=s$legend_pos,
        show_gridlines=s$show_gridlines,
        title_size=s$title_size, axis_text_size=s$axis_text_size),

      "Scatter" = default_scatter_state(
        x_col=x_col, y_col=y_col, color_by=s$color_by,
        line_color=s$line_color, point_size=s$point_size,
        point_alpha=s$point_alpha, point_shape=s$point_shape,
        smooth=s$smooth, smooth_method=s$smooth_method,
        color_palette=s$color_palette,
        title=s$title, x_label=s$x_label, y_label=s$y_label,
        theme=s$theme, legend_pos=s$legend_pos,
        show_gridlines=s$show_gridlines,
        title_size=s$title_size, axis_text_size=s$axis_text_size),

      "Bar" = default_bar_state(
        x_col=x_col, y_col=y_col, color_by=s$color_by,
        line_color=s$line_color, bar_width=s$bar_width, bar_position=s$bar_position,
        color_palette=s$color_palette,
        title=s$title, x_label=s$x_label, y_label=s$y_label,
        theme=s$theme, legend_pos=s$legend_pos,
        show_gridlines=s$show_gridlines,
        title_size=s$title_size, axis_text_size=s$axis_text_size),

      "Histogram" = default_hist_state(
        x_col=x_col, line_color=s$line_color, bins=s$bins,
        title=s$title, x_label=s$x_label, y_label=s$y_label,
        theme=s$theme, legend_pos=s$legend_pos,
        show_gridlines=s$show_gridlines,
        title_size=s$title_size, axis_text_size=s$axis_text_size)
    )
  }

  # Shared rebuild: generate new code and update labels
  do_rebuild <- function(x_col, y_col, style, reset_labels = FALSE) {
    if (reset_labels) { style$title <- NULL; style$x_label <- NULL; style$y_label <- NULL }
    state    <- build_state(x_col, y_col, style)
    new_code <- state_to_code(state)
    current_code(new_code)
    if (reset_labels) {
      updateTextInput(session, "chartTitle", value = state$title)
      updateTextInput(session, "xLabel",     value = state$x_label)
      updateTextInput(session, "yLabel",     value = state$y_label)
    }
    new_code
  }

  # ── Structural observers ─────────────────────────────────────────────────

  # Column / Y column change
  observeEvent(list(input$lineX, input$lineY), {
    req(dataset(), input$lineX)
    ct <- isolate(input$chartType) %||% "Line"
    y  <- if (ct != "Histogram") { req(input$lineY); input$lineY } else NULL
    push_undo(current_code())
    do_rebuild(input$lineX, y, read_style(ct), reset_labels = TRUE)
    status_text("Chart updated.")
    log_to_gsheet("column_change", paste(input$lineX, "x", input$lineY %||% ""), user_id)
  }, ignoreInit = TRUE)

  # Chart type change
  observeEvent(input$chartType, {
    req(dataset(), input$lineX)
    ct <- input$chartType
    y  <- if (ct != "Histogram") isolate(input$lineY) else NULL
    push_undo(current_code())
    do_rebuild(isolate(input$lineX), y, read_style(ct))
    status_text(paste(ct, "chart selected."))
    log_to_gsheet("chart_type_change", ct, user_id)
  }, ignoreInit = TRUE)

  # Color-by group change
  observeEvent(input$colorBy, {
    req(dataset(), input$lineX)
    ct <- isolate(input$chartType) %||% "Line"
    y  <- if (ct != "Histogram") isolate(input$lineY) else NULL
    push_undo(current_code())
    do_rebuild(isolate(input$lineX), y, read_style(ct))
    status_text("Grouping updated.")
  }, ignoreInit = TRUE)

  # ── Deterministic style patches ─────────────────────────────────────────

  observeEvent(input$lineColor, {
    req(nzchar(current_code()))
    current_code(patch_color(current_code(), input$lineColor))
  }, ignoreInit = TRUE)

  observeEvent(input$colorPalette, {
    req(nzchar(current_code()))
    current_code(patch_palette(current_code(), input$colorPalette))
  }, ignoreInit = TRUE)

  observeEvent(input$lineWidth, {
    req(nzchar(current_code()))
    current_code(patch_line_width(current_code(), input$lineWidth))
  }, ignoreInit = TRUE)

  observeEvent(input$showPoints, {
    req(nzchar(current_code()))
    current_code(patch_show_points(current_code(), input$showPoints,
                                   isolate(input$lineColor) %||% "#3a86ff",
                                   isolate(input$linePointSize) %||% 2.5))
  }, ignoreInit = TRUE)

  observeEvent(input$linePointSize, {
    req(nzchar(current_code()), isTRUE(input$showPoints))
    current_code(patch_point_size(current_code(), input$linePointSize))
  }, ignoreInit = TRUE)

  observeEvent(input$scatterPointSize, {
    req(nzchar(current_code()), input$chartType == "Scatter")
    current_code(patch_point_size(current_code(), input$scatterPointSize))
  }, ignoreInit = TRUE)

  observeEvent(input$pointAlpha, {
    req(nzchar(current_code()), input$chartType == "Scatter")
    current_code(patch_alpha(current_code(), input$pointAlpha))
  }, ignoreInit = TRUE)

  observeEvent(input$pointShape, {
    req(nzchar(current_code()), input$chartType == "Scatter")
    current_code(patch_point_shape(current_code(), as.integer(input$pointShape)))
  }, ignoreInit = TRUE)

  observeEvent(input$barWidth, {
    req(nzchar(current_code()), input$chartType == "Bar")
    current_code(patch_bar_width(current_code(), input$barWidth))
  }, ignoreInit = TRUE)

  observeEvent(input$barPosition, {
    req(nzchar(current_code()), input$chartType == "Bar")
    current_code(patch_bar_position(current_code(), input$barPosition))
  }, ignoreInit = TRUE)

  observeEvent(input$histBins, {
    req(nzchar(current_code()), input$chartType == "Histogram")
    current_code(patch_bins(current_code(), input$histBins))
  }, ignoreInit = TRUE)

  observeEvent(input$addSmooth, {
    req(nzchar(current_code()))
    current_code(patch_smooth(current_code(), input$addSmooth,
                              isolate(input$smoothMethod) %||% "loess"))
  }, ignoreInit = TRUE)

  observeEvent(input$smoothMethod, {
    req(nzchar(current_code()), isTRUE(input$addSmooth))
    current_code(patch_smooth(current_code(), show = TRUE, method = input$smoothMethod))
  }, ignoreInit = TRUE)

  observeEvent(input$chartTheme, {
    req(nzchar(current_code()))
    current_code(patch_theme(current_code(), input$chartTheme))
  }, ignoreInit = TRUE)

  observeEvent(input$legendPos, {
    req(nzchar(current_code()))
    current_code(patch_legend_pos(current_code(), input$legendPos))
  }, ignoreInit = TRUE)

  observeEvent(input$showGridlines, {
    req(nzchar(current_code()))
    current_code(patch_gridlines(current_code(), input$showGridlines))
  }, ignoreInit = TRUE)

  observeEvent(input$titleSize, {
    req(nzchar(current_code()))
    current_code(patch_title_size(current_code(), input$titleSize))
  }, ignoreInit = TRUE)

  observeEvent(input$axisTextSize, {
    req(nzchar(current_code()))
    current_code(patch_axis_text_size(current_code(), input$axisTextSize))
  }, ignoreInit = TRUE)

  title_d   <- debounce(reactive(input$chartTitle), 600)
  x_label_d <- debounce(reactive(input$xLabel),      600)
  y_label_d <- debounce(reactive(input$yLabel),       600)

  observeEvent(title_d(),   { req(nzchar(current_code())); current_code(patch_labs(current_code(), title   = title_d()))   }, ignoreInit = TRUE)
  observeEvent(x_label_d(), { req(nzchar(current_code())); current_code(patch_labs(current_code(), x_label = x_label_d())) }, ignoreInit = TRUE)
  observeEvent(y_label_d(), { req(nzchar(current_code())); current_code(patch_labs(current_code(), y_label = y_label_d())) }, ignoreInit = TRUE)

  # ── AI edit ─────────────────────────────────────────────────────────────
  observeEvent(input$applyAI, {
    req(nzchar(trimws(input$aiRequest)), nzchar(current_code()), dataset())
    request <- trimws(input$aiRequest)
    status_text("Applying AI edit…")
    push_undo(current_code())

    new_code <- tryCatch(
      apply_llm_edit(current_code(), request, dataset(), llm),
      error = function(e) { status_text(paste("AI error:", e$message)); NULL }
    )

    if (!is.null(new_code) && nzchar(new_code)) {
      current_code(new_code)
      sync_ui_from_code(new_code)
      hist <- c(list(list(request = request, ts = format(Sys.time(), "%H:%M"))), ai_history())
      ai_history(head(hist, 15))
      updateTextAreaInput(session, "aiRequest", value = "")
      status_text(paste0("AI edit applied: “", request, "”"))
      log_to_gsheet("ai_edit", request, user_id)
    }
  })

  # ── Undo / Redo ──────────────────────────────────────────────────────────
  observeEvent(input$undoBtn, {
    stack <- undo_stack()
    if (!length(stack)) { status_text("Nothing to undo."); return() }
    redo_stack(c(list(current_code()), redo_stack()))
    current_code(stack[[1]]); undo_stack(stack[-1])
    sync_ui_from_code(stack[[1]]); status_text("Undid last change.")
  })

  observeEvent(input$redoBtn, {
    stack <- redo_stack()
    if (!length(stack)) { status_text("Nothing to redo."); return() }
    push_undo(current_code())
    current_code(stack[[1]]); redo_stack(stack[-1])
    sync_ui_from_code(stack[[1]]); status_text("Redid change.")
  })

  # ── Plot rendering ────────────────────────────────────────────────────────
  output$plot <- renderPlot({
    code <- current_code()
    if (!nzchar(code)) {
      return(ggplot() +
        annotate("text", x=0.5, y=0.5, label="Load data and select columns to begin",
                 color="#adb5bd", size=5) + theme_void())
    }
    if (!validate_plot_code(code)) { status_text("Unsafe code detected."); return(NULL) }
    df <- dataset()
    result <- tryCatch({
      val <- eval(parse(text = code))
      if (inherits(val, "ggplot")) val else NULL
    }, error = function(e) { status_text(paste("Render error:", conditionMessage(e))); NULL })
    if (is.null(result)) return(NULL)
    print(result)
  })

  output$statusMsg <- renderText(status_text())

  output$aiHistory <- renderUI({
    hist <- ai_history()
    if (!length(hist)) return(div(style="font-size:11px;color:#adb5bd;", "No AI edits yet."))
    lapply(hist, function(h)
      div(class="ai-item", span(style="color:#6c757d;margin-right:6px;", h$ts), h$request))
  })
}

shinyApp(ui, server)
