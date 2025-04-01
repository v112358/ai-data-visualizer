library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(stringr)
library(ellmer)
library(here)

source("R/llm_helpers.R")
source("R/default_graphs.R")
source("R/logging.R")

llm <- create_llm_object()

ui <- fluidPage(
  titlePanel("AI Data Visualizer"),
  
  tags$head( ## head is for defining look behavior
    tags$style(HTML("
    .chart-option:hover { 
      background-color: #f0f0f0; 
      cursor: pointer;
    }
  "))
  ), ##style for chart-option (will come up in a pop-up) to specify the hover behavior and background color change
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      
      uiOutput("toy_data_button"),
      actionButton("open_chart_modal", "Choose Chart Type"), ## triggers the modal to show with all the graph options
      
      #Default chart UI inputs
      conditionalPanel( ## will only show when respective chart type is picked
        condition = "output.currentChartType == 'Line'",
        uiOutput("line_inputs")
      ),
      conditionalPanel(
        condition = "output.currentChartType == 'Scatter'",
        uiOutput("scatter_inputs")
      ),
      conditionalPanel(
        condition = "output.currentChartType == 'Bar'",
        checkboxInput("is_aggregated", "Use Aggregation?", FALSE), ## aggregation logic for barplots
        uiOutput("bar_inputs")
      ),
      conditionalPanel(
        condition = "output.currentChartType == 'Hist'",
        uiOutput("hist_inputs")
      ),
      
      #"Other" mode: LLM-based workflow
      conditionalPanel(
        condition = "output.currentChartType == 'Other'",
        textInput("prompt", "Describe your visualization", 
                  placeholder = "e.g., Show a histogram of numeric columns"),
        actionButton("generate", "Generate"),
        
        textAreaInput("feedbackInput", "Tweak your graph", placeholder = "e.g., Change colors, add labels"),
        actionButton("update", "Tweak"),
        br(),
        actionButton("undo", "Undo"),
        actionButton("redo", "Redo"),
        h4("Undo Counter:"),
        textOutput("undo_counter"),
        h4("Previous Feedback"),
        verbatimTextOutput("feedback_history")
      ),
      conditionalPanel(
        condition = "output.currentChartType != 'Other' && output.currentChartType != ''",
        actionButton("refineAI", "Refine with AI")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graph",
                 plotOutput("plot"),
                 verbatimTextOutput("code_output")
        ),
        tabPanel("CSV Preview",
                 tableOutput("csv_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  user_id <- paste0("session_", substr(session$token, 1, 8))
  log_to_gsheet("session_start", "", user_id = user_id)
  
  #REACTIVE STATE FOR CHART TYPE
  chartType <- reactiveVal(NULL)
  
  observeEvent(input$open_chart_modal, {
    showModal(
      modalDialog(
        title = "Select a Chart Type",
        easyClose = TRUE,
        footer = NULL,
        # Arrange clickable chart images in a fluidRow:
        fluidRow(
          # Each chart type as an actionButton with an image + label
          column(3,
                 actionButton("opt_line", label = div(
                   class = "chart-option",
                   tags$img(src = "line_chart.png", width = "100%", style="border:1px solid #ccc;"),
                   tags$p("Line Plot", style="text-align: center; margin-top: 5px;")
                 ), style = "border:none;background:none;padding:0; width:100%")
          ),
          column(3,
                 actionButton("opt_scatter", label = div(
                   class = "chart-option",
                   tags$img(src = "scatter_chart.png", width = "100%", style="border:1px solid #ccc;"),
                   tags$p("Scatter Plot", style="text-align: center; margin-top: 5px;")
                 ), style = "border:none;background:none;padding:0; width:100%")
          ),
          column(3,
                 actionButton("opt_bar", label = div(
                   class = "chart-option",
                   tags$img(src = "bar_chart.png", width = "100%", style="border:1px solid #ccc;"),
                   tags$p("Bar Chart", style="text-align: center; margin-top: 5px;")
                 ), style = "border:none;background:none;padding:0; width:100%")
          ),
          column(3,
                 actionButton("opt_hist", label = div(
                   class = "chart-option",
                   tags$img(src = "hist_chart.png", width = "100%", style="border:1px solid #ccc;"),
                   tags$p("Histogram", style="text-align: center; margin-top: 5px;")
                 ), style = "border:none;background:none;padding:0; width:100%")
          )
        ),
        br(),
        fluidRow(
          column(3,
                 actionButton("opt_other", label = div(
                   class = "chart-option",
                   tags$img(src = "other_chart.png", width = "100%", style="border:1px solid #ccc;"),
                   tags$p("Other (AI Mode)", style="text-align: center; margin-top: 5px;")
                 ), style = "border:none;background:none;padding:0; width:100%")
          )
        )
      )
    )
  })
  
  # Observers for each chart type option:
  observeEvent(input$opt_line, {
    removeModal()
    chartType("Line") 
    log_to_gsheet("chart_selected", "Line",user_id = user_id)
  })
  
  observeEvent(input$opt_scatter, {
    removeModal()
    chartType("Scatter")
    log_to_gsheet("chart_selected", "Scatter",user_id = user_id)
  })
  
  observeEvent(input$opt_bar, {
    removeModal()
    chartType("Bar")
    log_to_gsheet("chart_selected", "Bar",user_id = user_id)
  })
  
  observeEvent(input$opt_hist, {
    removeModal()
    chartType("Hist")
    log_to_gsheet("chart_selected", "Histogram",user_id = user_id)
  })
  
  observeEvent(input$opt_other, {
    removeModal()
    chartType("Other")
    log_to_gsheet("chart_selected", "Other",user_id = user_id)
  })
  
  # Expose chartType to UI for conditionalPanel
  output$currentChartType <- reactive({
    chartType() %||% ""
  })
  outputOptions(output, "currentChartType", suspendWhenHidden = FALSE)
  
  # READ THE CSV DATASET
  
  data_source <- reactiveVal("upload")
  
  observeEvent(input$use_toy_data, {
    data_source("toy")
    output$message_area <- renderUI({
      span("Using toy data (iris).", style = "color: blue; font-weight: bold;")
    })
    log_to_gsheet("data_source", "Toy data used",user_id = user_id)
  })
  
  observeEvent(input$file, {
    data_source("upload")
    output$message_area <- renderUI({
      span("Using uploaded file.", style = "color: green; font-weight: bold;")
    })
    log_to_gsheet("data_source", "Uploaded CSV",user_id = user_id)
  })
  
  output$toy_data_button <- renderUI({
    label <- if (data_source() == "toy") {
      HTML('Try with toy data <span style="color: white; background-color: #28a745; border-radius: 6px; padding: 2px 6px; margin-left: 6px;">Active</span>')
    } else {
      "Try with toy data"
    }
    
    actionButton("use_toy_data", label)
  })
  
  dataset <- reactive({
    if (data_source() == "toy") {
      iris %>%
        tibble::rownames_to_column(var = "index")
    } else {
      req(input$file)
      readr::read_csv(input$file$datapath)
    }
  })
  
  output$csv_preview <- renderTable({
    req(dataset())
    head(dataset(), 5)
  })
  
  #DETERMINE NUMERIC VS. CATEGORICAL COLS
  numericCols <- reactive({
    req(dataset())
    names(dataset())[sapply(dataset(), is.numeric)]
  })
  
  categoricalCols <- reactive({
    req(dataset())
    df <- dataset()
    names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  })
  
  dateCols <- reactive({
    req(dataset())
    names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  })
  
  #DYNAMIC UI FOR DEFAULT CHARTS
  output$line_inputs <- renderUI({
    req(numericCols())
    tagList(
      selectInput("lineX", "X Axis (often numeric or date)", choices = c(numericCols(),dateCols())),
      selectInput("lineY", "Y Axis (numeric)", choices = numericCols())
    )
  })
  
  output$scatter_inputs <- renderUI({
    req(numericCols())
    tagList(
      selectInput("scatterX", "X (numeric)", choices = numericCols()),
      selectInput("scatterY", "Y (numeric)", choices = numericCols())
    )
  })
  
  output$bar_inputs <- renderUI({
    if (!input$is_aggregated) {
      # Non-aggregated
      tagList(
        selectInput("barX", "X (categorical)", choices = categoricalCols()),
        selectInput("barY", "Y (numeric)", choices = numericCols())
      )
    } else {
      # Aggregated
      tagList(
        selectInput("barAggCat", "Categorical Column", choices = categoricalCols()),
        selectInput("barAggNum", "Numeric Column", choices = numericCols()),
        selectInput("barAggFn", "Aggregation Function",
                    choices = c("sum", "mean", "count", "max", "min"), 
                    selected = "sum")
      )
    }
  })
  
  output$hist_inputs <- renderUI({
    req(numericCols())
    selectInput("histCol", "Histogram Column", choices = numericCols())
  })
  observeEvent(input$refineAI, {
    req(current_code())  # We need some code from the default chart
    
    # Switch to 'Other' mode
    chartType("Other")
    
    # Initialize the LLM stack with the existing code
    undo_stack(list(current_code()))
    redo_stack(list())
    undo_counter(0)
    feedback_history("")  # reset any old feedback from prior LLM session
  })
  
  # UNDO/REDO STATE FOR LLM (ONLY WHEN CHARTTYPE == 'Other') 
  feedback_history <- reactiveVal("")
  undo_stack <- reactiveVal(list())
  redo_stack <- reactiveVal(list())
  undo_counter <- reactiveVal(0)
  current_code <- reactiveVal("")  # store the "final" code to evaluate for the plot
  
  # TIE THE DEFAULT CODE (NON-LLM) TO current_code() REACTIVELY
  observe({
    # If user is in "Other", we do nothing here (the LLM approach handles code).
    # If user picks a default chart (Line, Scatter, Bar, Hist), we generate code reactively.
    req(dataset())
    ct <- chartType()
    if (is.null(ct) || ct == "Other") {
      return(NULL)
    }
    
    # Build code from default_graphs.R
    # We'll handle each chart type. If the user changes any input, this triggers.
    new_code <- NULL
    
    if (ct == "Line") {
      req(input$lineX, input$lineY)
      new_code <- generate_default_line_code(input$lineX, input$lineY)
      
    } else if (ct == "Scatter") {
      req(input$scatterX, input$scatterY)
      new_code <- generate_default_scatter_code(input$scatterX, input$scatterY)
      
    } else if (ct == "Bar") {
      if (!input$is_aggregated) {
        req(input$barX, input$barY)
        new_code <- generate_default_bar_code(input$barX, input$barY)
      } else {
        req(input$barAggCat, input$barAggFn)
        new_code <- generate_default_agg_bar_code(
          cat_col = input$barAggCat,
          num_col = input$barAggNum,
          agg_fn  = input$barAggFn
        )
      }
    } else if (ct == "Hist") {
      req(input$histCol)
      new_code <- generate_default_hist_code(input$histCol)
    }
    
    current_code(new_code)
  })
  
  # LLM FLOW (CHARTTYPE == 'Other') 
  # 
  # or Tweak feedback, etc. Then the final code is stored in current_code().
  
  observeEvent(input$generate, {
    req(input$prompt, dataset())
    
    new_code <- generate_plot_code(
      prompt = input$prompt,
      df = dataset(),
      llm = llm
    )
    log_to_gsheet("llm_generate", paste("Prompt:", input$prompt),user_id = user_id)
    log_to_gsheet("llm_code_generated", new_code,user_id = user_id)
    # Reset stacks
    undo_stack(list(new_code))
    redo_stack(list())
    undo_counter(0)
    feedback_history("")
    current_code(new_code)
  })
  
  observeEvent(input$update, {
    req(input$feedbackInput, current_code())
    
    updated_feedback <- paste(feedback_history(), input$feedbackInput, sep="\n")
    feedback_history(updated_feedback)
    
    refined_code <- refine_plot_code(
      current_code(),
      input$feedbackInput,
      llm
    )
    
    log_to_gsheet("llm_code_tweaked", refined_code,user_id = user_id)
    new_stack <- c(list(refined_code), undo_stack())
    undo_stack(head(new_stack, 3))
    redo_stack(list())
    undo_counter(length(undo_stack()) - 1)
    current_code(refined_code)
  })
  
  observeEvent(input$undo, {
    stack <- undo_stack()
    if (length(stack) > 1) {
      redo_stack(c(list(stack[[1]]), redo_stack()))
      undo_stack(stack[-1])
      current_code(undo_stack()[[1]])
      undo_counter(length(undo_stack()) - 1)
    }
  })
  
  observeEvent(input$redo, {
    stack <- redo_stack()
    if (length(stack) > 0) {
      undo_stack(c(list(stack[[1]]), undo_stack()))
      redo_stack(stack[-1])
      current_code(stack[[1]])
      undo_counter(length(undo_stack()) - 1)
    }
  })
  
  # PLOT RENDERING
  output$plot <- renderPlot({
    req(dataset())
    code_to_run <- current_code()
    if (is.null(code_to_run) || code_to_run == "") return(NULL)
    if (!validate_plot_code(code_to_run)) {
      stop("Unsafe or invalid code detected. Please revise your input.")
    }
    df <- dataset()
    expr <- try(parse(text = code_to_run), silent = TRUE)
    if (inherits(expr, "try-error")) {
      return(NULL)
    }
    val <- try(eval(expr), silent = TRUE)
    if (inherits(val, "try-error")) {
      return(NULL)
    }
    # If val is a ggplot object, print it:
    if (inherits(val, "ggplot")) {
      print(val)
    }
  })
  
  # CODE DISPLAY 
  output$code_output <- renderText({
    current_code()
  })
  
  output$undo_counter <- renderText({
    paste(undo_counter(), "undos available")
  })
  
  output$feedback_history <- renderText({
    feedback_history()
  })
}

shinyApp(ui, server)
