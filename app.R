# =============================================================================
# Project 2: Data Explorer — R Shiny Web Application
# =============================================================================
# Student 1: App Skeleton, Data Loading, User Guide
# Student 2: Data Cleaning
# Student 3: Feature Engineering + EDA summary/correlation
# =============================================================================

# ---- Packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(DT)
library(readxl)
library(jsonlite)
library(tools)

# ---- Global Options ----------------------------------------------------------
options(shiny.maxRequestSize = 30 * 1024^2)  # 30 MB upload limit

# ---- Helper: Read uploaded file by extension ---------------------------------
read_uploaded_file <- function(file_path, file_name) {
  ext <- tolower(file_ext(file_name))
  switch(ext,
         "csv"  = read.csv(file_path, stringsAsFactors = FALSE),
         "xlsx" = as.data.frame(read_excel(file_path)),
         "xls"  = as.data.frame(read_excel(file_path)),
         "json" = {
           result <- fromJSON(file_path, flatten = TRUE)
           if (!is.data.frame(result)) {
             stop("JSON file must contain a flat array of objects (tabular data).")
           }
           result
         },
         "rds"  = {
           result <- readRDS(file_path)
           if (!is.data.frame(result)) {
             stop("RDS file must contain a data.frame.")
           }
           result
         },
         stop(paste0("Unsupported file format: .", ext,
                     ". Please upload CSV, Excel, JSON, or RDS."))
  )
}

# ---- Demo dataset choices ----------------------------------------------------
demo_choices <- c(
  "Select a dataset..." = "none",
  "mtcars (Motor Trend Cars)" = "mtcars",
  "iris (Fisher's Iris)" = "iris"
)

# ---- Custom CSS --------------------------------------------------------------
custom_css <- "
body {
  background-color: #f5f7fa;
  font-family: 'Segoe UI', system-ui, -apple-system, sans-serif;
}
.navbar {
  background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%) !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.15);
  padding: 0.5rem 1rem;
}
.navbar-brand {
  font-weight: 700;
  font-size: 1.3rem;
  letter-spacing: 0.5px;
}
.navbar .nav-link {
  color: rgba(255,255,255,0.85) !important;
  font-weight: 500;
  transition: color 0.2s ease, background 0.2s ease;
  border-radius: 6px;
  margin: 0 2px;
  padding: 0.5rem 1rem !important;
}
.navbar .nav-link:hover,
.navbar .nav-link.active {
  color: #fff !important;
  background: rgba(255,255,255,0.15);
}
.hero-banner {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  border-radius: 16px;
  padding: 3rem 2rem;
  margin-bottom: 2rem;
  text-align: center;
  box-shadow: 0 8px 30px rgba(102, 126, 234, 0.3);
}
.hero-banner h1 {
  font-weight: 800;
  font-size: 2.5rem;
  margin-bottom: 0.5rem;
}
.hero-banner .lead {
  color: rgba(255,255,255,0.9);
  font-size: 1.15rem;
  max-width: 600px;
  margin: 0 auto;
}
.step-card {
  border: none;
  border-radius: 14px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.06);
  transition: transform 0.25s ease, box-shadow 0.25s ease;
  overflow: hidden;
  height: 100%;
  background: white;
}
.step-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 25px rgba(0,0,0,0.1);
}
.step-icon-circle {
  width: 48px;
  height: 48px;
  border-radius: 50%;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 1.2rem;
  color: white;
  flex-shrink: 0;
}
.step-header {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 1.25rem 1.25rem 0.75rem;
  font-weight: 700;
  font-size: 1.05rem;
}
.step-body {
  padding: 0 1.25rem 1.25rem;
  font-size: 0.92rem;
  color: #555;
}
.step-body ul {
  padding-left: 1.1rem;
  margin-bottom: 0;
}
.step-body li {
  margin-bottom: 0.3rem;
}
.tips-card {
  border: none;
  border-radius: 14px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.06);
  background: white;
}
.tips-card .card-header {
  background: transparent;
  border-bottom: 1px solid #eee;
  font-weight: 700;
  font-size: 1.05rem;
  color: #333;
}
.upload-section {
  background: white;
  border-radius: 14px;
  padding: 1.5rem;
  box-shadow: 0 4px 15px rgba(0,0,0,0.06);
  height: 100%;
}
.upload-section h4 {
  font-weight: 700;
  font-size: 1.1rem;
  color: #2c3e50;
  margin-bottom: 1rem;
}
.upload-section hr {
  border-color: #eee;
}
.section-divider {
  display: flex;
  align-items: center;
  text-align: center;
  color: #aaa;
  font-size: 0.85rem;
  font-weight: 600;
  margin: 1.25rem 0;
}
.section-divider::before,
.section-divider::after {
  content: '';
  flex: 1;
  border-bottom: 1px solid #e0e0e0;
}
.section-divider::before { margin-right: 0.75rem; }
.section-divider::after  { margin-left: 0.75rem; }
.stat-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 10px;
  margin-top: 1rem;
}
.stat-box {
  background: #f8f9fa;
  border-radius: 10px;
  padding: 0.75rem;
  text-align: center;
  border: 1px solid #e9ecef;
}
.stat-box .stat-value {
  font-size: 1.4rem;
  font-weight: 700;
  color: #2c3e50;
  display: block;
}
.stat-box .stat-label {
  font-size: 0.75rem;
  color: #888;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}
.status-banner {
  border-radius: 12px;
  padding: 1rem 1.25rem;
  display: flex;
  align-items: center;
  gap: 10px;
  font-weight: 500;
  margin-bottom: 1.25rem;
}
.status-info {
  background: #eef2ff;
  color: #4361ee;
  border: 1px solid #c7d2fe;
}
.status-success {
  background: #ecfdf5;
  color: #059669;
  border: 1px solid #a7f3d0;
}
.preview-card {
  background: white;
  border-radius: 14px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.06);
  padding: 1.5rem;
}
.preview-card h5 {
  font-weight: 700;
  color: #2c3e50;
  margin-bottom: 1rem;
}
.placeholder-tab {
  text-align: center;
  padding: 4rem 2rem;
}
.placeholder-tab .placeholder-icon {
  width: 80px;
  height: 80px;
  border-radius: 50%;
  background: #eef2ff;
  color: #667eea;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 2rem;
  margin-bottom: 1rem;
}
.placeholder-tab h3 {
  font-weight: 700;
  color: #333;
}
.placeholder-tab p {
  color: #888;
  max-width: 500px;
  margin: 0.5rem auto;
}
.app-footer {
  text-align: center;
  padding: 1.5rem;
  margin-top: 3rem;
  color: #aaa;
  font-size: 0.85rem;
  border-top: 1px solid #e9ecef;
}
.btn-primary {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  border: none;
  border-radius: 8px;
  font-weight: 600;
  padding: 0.6rem 1.25rem;
  transition: opacity 0.2s ease, transform 0.2s ease;
}
.btn-primary:hover {
  opacity: 0.9;
  transform: translateY(-1px);
}
.form-control, .shiny-input-container .form-control {
  border-radius: 8px;
  border: 1.5px solid #dee2e6;
}
.form-control:focus {
  border-color: #667eea;
  box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.15);
}
"

# =============================================================================
# UI
# =============================================================================
ui <- navbarPage(
  title = span(icon("chart-line"), " Data Explorer"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$style(HTML(custom_css))),
  
  tabPanel(
    "User Guide",
    icon = icon("book"),
    fluidPage(
      class = "mt-4 px-3",
      div(
        class = "hero-banner",
        h1("Welcome to Data Explorer"),
        p(class = "lead",
          "Upload, clean, transform, and visualize your datasets — all from one interactive dashboard.")
      ),
      fluidRow(
        column(6, class = "mb-4",
               div(class = "step-card",
                   div(class = "step-header",
                       span(class = "step-icon-circle",
                            style = "background: linear-gradient(135deg, #4361ee, #3a86ff);",
                            icon("upload")),
                       span("Step 1: Load Your Data")
                   ),
                   div(class = "step-body",
                       p("Head to the ", strong("Data Loading"), " tab to get started."),
                       tags$ul(
                         tags$li("Upload CSV, Excel (.xlsx/.xls), JSON, or RDS files (up to 30 MB)."),
                         tags$li("Or pick a built-in demo dataset to explore the app instantly."),
                         tags$li("Preview your data in an interactive, searchable table.")
                       )
                   ))
        ),
        column(6, class = "mb-4",
               div(class = "step-card",
                   div(class = "step-header",
                       span(class = "step-icon-circle",
                            style = "background: linear-gradient(135deg, #059669, #34d399);",
                            icon("broom")),
                       span("Step 2: Clean & Preprocess")
                   ),
                   div(class = "step-body",
                       p("Use the ", strong("Data Cleaning"), " tab to prepare your data."),
                       tags$ul(
                         tags$li("Handle missing values with drop or imputation strategies."),
                         tags$li("Remove duplicate rows and standardize formats."),
                         tags$li("Scale, normalize, and encode categorical variables."),
                         tags$li("Detect and handle outliers.")
                       )
                   ))
        ),
        column(6, class = "mb-4",
               div(class = "step-card",
                   div(class = "step-header",
                       span(class = "step-icon-circle",
                            style = "background: linear-gradient(135deg, #f59e0b, #fbbf24);",
                            icon("wrench")),
                       span("Step 3: Engineer Features")
                   ),
                   div(class = "step-body",
                       p("Visit the ", strong("Feature Engineering"), " tab to create new variables."),
                       tags$ul(
                         tags$li("Build new columns from math expressions, binning, or interactions."),
                         tags$li("Rename or drop existing columns."),
                         tags$li("See before-and-after previews of every change.")
                       )
                   ))
        ),
        column(6, class = "mb-4",
               div(class = "step-card",
                   div(class = "step-header",
                       span(class = "step-icon-circle",
                            style = "background: linear-gradient(135deg, #8b5cf6, #a78bfa);",
                            icon("chart-bar")),
                       span("Step 4: Explore & Visualize")
                   ),
                   div(class = "step-body",
                       p("Go to the ", strong("EDA"), " tab to uncover insights."),
                       tags$ul(
                         tags$li("View summary statistics and correlation matrices."),
                         tags$li("Create interactive plots — histograms, boxplots, scatter plots, and more."),
                         tags$li("Filter and subset your data dynamically.")
                       )
                   ))
        )
      ),
      card(
        class = "tips-card mt-2",
        card_header(span(icon("lightbulb"), " Tips for Best Results")),
        card_body(
          tags$ul(
            tags$li("Start with the ", strong("Data Loading"), " tab — all other tabs depend on having a dataset loaded."),
            tags$li("Work through the tabs left to right: Load → Clean → Engineer → Explore.")
          )
        )
      ),
      div(class = "app-footer", "Data Explorer — Applied Data Science, Spring 2026")
    )
  ),
  
  tabPanel(
    "Data Loading",
    icon = icon("upload"),
    fluidPage(
      class = "mt-4 px-3",
      fluidRow(
        column(3,
               div(class = "upload-section",
                   h4(icon("file-arrow-up"), " Upload a File"),
                   fileInput(
                     "file_upload", NULL,
                     accept = c(".csv", ".xlsx", ".xls", ".json", ".rds"),
                     placeholder = "CSV, Excel, JSON, or RDS"
                   ),
                   helpText("Supported: .csv, .xlsx, .xls, .json, .rds", br(), "Max size: 30 MB"),
                   div(class = "section-divider", "OR"),
                   h4(icon("database"), " Demo Dataset"),
                   selectInput("demo_data", NULL, choices = demo_choices),
                   actionButton("load_demo", "Load Demo Dataset", class = "btn-primary w-100", icon = icon("play")),
                   uiOutput("data_summary_panel")
               )
        ),
        column(9,
               uiOutput("load_status"),
               div(class = "preview-card",
                   h5(icon("table"), " Data Preview"),
                   DTOutput("data_preview")
               )
        )
      ),
      div(class = "app-footer", "Data Explorer — Applied Data Science, Spring 2026")
    )
  ),
  
  tabPanel(
    "Data Cleaning",
    icon = icon("broom"),
    sidebarLayout(
      sidebarPanel(
        h4("Cleaning Options"),
        selectInput("missing_method", "Handle Missing Values:",
                    choices = c("None", "Drop", "Mean", "Median", "Mode")),
        checkboxInput("remove_dup", "Remove duplicates"),
        selectInput("scaling", "Scaling:",
                    choices = c("None", "Standardize", "Normalize")),
        selectInput("encoding", "Encoding:",
                    choices = c("None", "Label", "One-hot")),
        checkboxInput("remove_outliers", "Remove Outliers")
      ),
      mainPanel(
        h4("Cleaned Data Preview"),
        DTOutput("cleaned_preview")
      )
    )
  ),
  
  tabPanel(
    "Feature Engineering",
    icon = icon("wrench"),
    sidebarLayout(
      sidebarPanel(
        h4("Feature Engineering"),
        radioButtons(
          "fe_action",
          "Choose Action:",
          choices = c(
            "Create Math Feature" = "math",
            "Create Binned Feature" = "bin",
            "Create Interaction Feature" = "interaction",
            "Rename Column" = "rename",
            "Drop Columns" = "drop"
          ),
          selected = "math"
        ),
        conditionalPanel(
          condition = "input.fe_action == 'math'",
          textInput("fe_new_col_math", "New Column Name:", "new_feature"),
          selectInput("fe_math_col1", "Column 1:", choices = NULL),
          selectInput("fe_math_operator", "Operator:", choices = c("+", "-", "*", "/")),
          selectInput("fe_math_col2", "Column 2:", choices = NULL),
          actionButton("apply_math_feature", "Create Math Feature", class = "btn-primary")
        ),
        conditionalPanel(
          condition = "input.fe_action == 'bin'",
          selectInput("fe_bin_col", "Numeric Column to Bin:", choices = NULL),
          numericInput("fe_bin_k", "Number of Bins:", value = 4, min = 2, max = 20),
          textInput("fe_bin_new_col", "New Binned Column Name:", "binned_feature"),
          actionButton("apply_bin_feature", "Create Binned Feature", class = "btn-primary")
        ),
        conditionalPanel(
          condition = "input.fe_action == 'interaction'",
          textInput("fe_inter_new_col", "New Interaction Column Name:", "interaction_feature"),
          selectInput("fe_inter_col1", "Numeric Column 1:", choices = NULL),
          selectInput("fe_inter_col2", "Numeric Column 2:", choices = NULL),
          actionButton("apply_interaction_feature", "Create Interaction Feature", class = "btn-primary")
        ),
        conditionalPanel(
          condition = "input.fe_action == 'rename'",
          selectInput("fe_rename_old", "Column to Rename:", choices = NULL),
          textInput("fe_rename_new", "New Column Name:", ""),
          actionButton("apply_rename_column", "Rename Column", class = "btn-primary")
        ),
        conditionalPanel(
          condition = "input.fe_action == 'drop'",
          selectInput("fe_drop_cols", "Columns to Drop:", choices = NULL, multiple = TRUE),
          actionButton("apply_drop_columns", "Drop Selected Columns", class = "btn-primary")
        ),
        hr(),
        actionButton("reset_engineering", "Reset to Cleaned Data", icon = icon("rotate-left"))
      ),
      mainPanel(
        fluidRow(
          column(6, h4("Before"), DTOutput("fe_before_preview")),
          column(6, h4("After"), DTOutput("fe_after_preview"))
        ),
        br(),
        h4("Engineering Log"),
        verbatimTextOutput("fe_log")
      )
    )
  ),
  
  tabPanel(
    "EDA",
    icon = icon("chart-bar"),
    sidebarLayout(
      sidebarPanel(
        h4("EDA Controls"),
        selectInput("eda_columns", "Columns to Include:", choices = NULL, multiple = TRUE),
        checkboxInput("eda_show_summary", "Show Summary Statistics", TRUE),
        checkboxInput("eda_show_corr", "Show Correlation Matrix", TRUE)
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.eda_show_summary == true",
          h4("Summary Statistics"),
          DTOutput("eda_summary_table"),
          br()
        ),
        conditionalPanel(
          condition = "input.eda_show_corr == true",
          h4("Correlation Matrix"),
          DTOutput("eda_corr_table"),
          br(),
          h4("Correlation Heatmap"),
          plotOutput("eda_corr_heatmap", height = "500px")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # ---- Shared reactive dataset ------------------------------------------------
  current_data <- reactiveVal(NULL)
  data_name <- reactiveVal(NULL)
  
  # ---- Data loading: file upload ---------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      df <- read_uploaded_file(
        input$file_upload$datapath,
        input$file_upload$name
      )
      current_data(df)
      data_name(input$file_upload$name)
      updateSelectInput(session, "demo_data", selected = "none")
      
      showNotification(
        paste0("Successfully loaded '", input$file_upload$name,
               "' (", nrow(df), " rows, ", ncol(df), " columns)"),
        type = "message", duration = 5
      )
    }, error = function(e) {
      current_data(NULL)
      data_name(NULL)
      showNotification(
        paste("Error reading file:", e$message),
        type = "error", duration = 8
      )
    })
  })
  
  # ---- Data loading: demo dataset --------------------------------------------
  observeEvent(input$load_demo, {
    req(input$demo_data != "none")
    
    df <- switch(input$demo_data,
                 "mtcars" = mtcars,
                 "iris" = iris)
    
    if (!is.null(df)) {
      current_data(df)
      data_name(input$demo_data)
      showNotification(
        paste0("Loaded demo dataset '", input$demo_data,
               "' (", nrow(df), " rows, ", ncol(df), " columns)"),
        type = "message", duration = 5
      )
    }
  })
  
  # ---- Status banner ----------------------------------------------------------
  output$load_status <- renderUI({
    if (is.null(current_data())) {
      div(
        class = "status-banner status-info",
        icon("info-circle"),
        span("No dataset loaded yet. Upload a file or select a demo dataset from the sidebar to get started.")
      )
    } else {
      div(
        class = "status-banner status-success",
        icon("check-circle"),
        span(paste0("Dataset loaded: ", data_name(),
                    " — ", nrow(current_data()), " rows, ",
                    ncol(current_data()), " columns"))
      )
    }
  })
  
  # ---- Summary panel ----------------------------------------------------------
  output$data_summary_panel <- renderUI({
    req(current_data())
    df <- current_data()
    
    num_cols <- sum(sapply(df, is.numeric))
    char_cols <- sum(sapply(df, is.character))
    factor_cols <- sum(sapply(df, is.factor))
    missing <- sum(is.na(df))
    
    tagList(
      hr(),
      h4(icon("chart-pie"), " Summary"),
      div(class = "stat-grid",
          div(class = "stat-box",
              span(class = "stat-value", nrow(df)),
              span(class = "stat-label", "Rows")),
          div(class = "stat-box",
              span(class = "stat-value", ncol(df)),
              span(class = "stat-label", "Columns")),
          div(class = "stat-box",
              span(class = "stat-value", num_cols),
              span(class = "stat-label", "Numeric")),
          div(class = "stat-box",
              span(class = "stat-value", char_cols + factor_cols),
              span(class = "stat-label", "Categorical")),
          div(class = "stat-box",
              span(class = "stat-value", missing),
              span(class = "stat-label", "Missing"))
      )
    )
  })
  
  # ---- Data preview -----------------------------------------------------------
  output$data_preview <- renderDT({
    req(current_data())
    datatable(
      current_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(emptyTable = "No data to display")
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  # =============================================================================
  # DATA CLEANING SERVER LOGIC
  # =============================================================================
  cleaned_data <- reactive({
    req(current_data())
    df <- current_data()
    
    if (!is.null(input$missing_method) && input$missing_method != "None") {
      if (input$missing_method == "Drop") {
        df <- na.omit(df)
        
      } else if (input$missing_method == "Mean") {
        num_cols <- sapply(df, is.numeric)
        df[num_cols] <- lapply(df[num_cols], function(x) {
          x[is.na(x)] <- mean(x, na.rm = TRUE)
          x
        })
        
      } else if (input$missing_method == "Median") {
        num_cols <- sapply(df, is.numeric)
        df[num_cols] <- lapply(df[num_cols], function(x) {
          x[is.na(x)] <- median(x, na.rm = TRUE)
          x
        })
        
      } else if (input$missing_method == "Mode") {
        mode_func <- function(x) {
          x_non_na <- x[!is.na(x)]
          if (length(x_non_na) == 0) return(NA)
          ux <- unique(x_non_na)
          ux[which.max(tabulate(match(x_non_na, ux)))]
        }
        
        df[] <- lapply(df, function(x) {
          fill_value <- mode_func(x)
          x[is.na(x)] <- fill_value
          x
        })
      }
    }
    
    if (!is.null(input$remove_dup) && input$remove_dup) {
      df <- unique(df)
    }
    
    if (!is.null(input$scaling) && input$scaling != "None") {
      num_cols <- sapply(df, is.numeric)
      
      if (any(num_cols)) {
        if (input$scaling == "Standardize") {
          scaled <- scale(df[num_cols])
          df[num_cols] <- as.data.frame(scaled)
          
        } else if (input$scaling == "Normalize") {
          df[num_cols] <- lapply(df[num_cols], function(x) {
            rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
            if (is.na(rng) || rng == 0) {
              rep(0, length(x))
            } else {
              (x - min(x, na.rm = TRUE)) / rng
            }
          })
        }
      }
    }
    
    if (!is.null(input$encoding) && input$encoding != "None") {
      if (input$encoding == "Label") {
        df[] <- lapply(df, function(x) {
          if (is.character(x) || is.factor(x)) {
            as.numeric(as.factor(x))
          } else {
            x
          }
        })
        
      } else if (input$encoding == "One-hot") {
        df[] <- lapply(df, function(x) {
          if (is.character(x)) as.factor(x) else x
        })
        df <- as.data.frame(model.matrix(~ . - 1, data = df))
      }
    }
    
    if (!is.null(input$remove_outliers) && input$remove_outliers) {
      num_cols <- sapply(df, is.numeric)
      
      for (col in names(df)[num_cols]) {
        Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        
        if (!is.na(IQR_val) && IQR_val > 0) {
          lower <- Q1 - 1.5 * IQR_val
          upper <- Q3 + 1.5 * IQR_val
          keep <- is.na(df[[col]]) | (df[[col]] >= lower & df[[col]] <= upper)
          df <- df[keep, , drop = FALSE]
        }
      }
    }
    
    df
  })
  
  output$cleaned_preview <- renderDT({
    req(cleaned_data())
    datatable(
      cleaned_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  # =============================================================================
  # FEATURE ENGINEERING SERVER LOGIC
  # =============================================================================
  engineered_data <- reactiveVal(NULL)
  fe_log <- reactiveVal("No feature engineering actions applied yet.")
  
  observe({
    req(cleaned_data())
    engineered_data(cleaned_data())
  })
  
  observe({
    req(engineered_data())
    df <- engineered_data()
    
    all_cols <- names(df)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "fe_math_col1", choices = numeric_cols)
    updateSelectInput(session, "fe_math_col2", choices = numeric_cols)
    
    updateSelectInput(session, "fe_bin_col", choices = numeric_cols)
    
    updateSelectInput(session, "fe_inter_col1", choices = numeric_cols)
    updateSelectInput(session, "fe_inter_col2", choices = numeric_cols)
    
    updateSelectInput(session, "fe_rename_old", choices = all_cols)
    updateSelectInput(session, "fe_drop_cols", choices = all_cols)
    
    updateSelectInput(session, "eda_columns", choices = all_cols, selected = all_cols)
  })
  
  observeEvent(input$reset_engineering, {
    req(cleaned_data())
    engineered_data(cleaned_data())
    fe_log("Reset feature-engineered dataset back to cleaned data.")
    showNotification("Feature engineering reset to cleaned data.", type = "message")
  })
  
  observeEvent(input$apply_math_feature, {
    req(engineered_data(), input$fe_new_col_math, input$fe_math_col1, input$fe_math_col2)
    
    df <- engineered_data()
    new_col <- trimws(input$fe_new_col_math)
    
    if (new_col == "") {
      showNotification("Please provide a new column name.", type = "error")
      return()
    }
    
    if (new_col %in% names(df)) {
      showNotification("New column name already exists.", type = "error")
      return()
    }
    
    x <- df[[input$fe_math_col1]]
    y <- df[[input$fe_math_col2]]
    
    df[[new_col]] <- switch(
      input$fe_math_operator,
      "+" = x + y,
      "-" = x - y,
      "*" = x * y,
      "/" = ifelse(y == 0, NA, x / y)
    )
    
    engineered_data(df)
    fe_log(paste0("Created math feature '", new_col, "' using ",
                  input$fe_math_col1, " ", input$fe_math_operator, " ", input$fe_math_col2, "."))
    showNotification(paste("Created new feature:", new_col), type = "message")
  })
  
  observeEvent(input$apply_bin_feature, {
    req(engineered_data(), input$fe_bin_col, input$fe_bin_new_col)
    
    df <- engineered_data()
    new_col <- trimws(input$fe_bin_new_col)
    
    if (new_col == "") {
      showNotification("Please provide a new column name.", type = "error")
      return()
    }
    
    if (new_col %in% names(df)) {
      showNotification("New column name already exists.", type = "error")
      return()
    }
    
    if (!is.numeric(df[[input$fe_bin_col]])) {
      showNotification("Selected column must be numeric.", type = "error")
      return()
    }
    
    breaks <- unique(quantile(
      df[[input$fe_bin_col]],
      probs = seq(0, 1, length.out = input$fe_bin_k + 1),
      na.rm = TRUE
    ))
    
    if (length(breaks) <= 2) {
      showNotification("Not enough unique values to create bins.", type = "error")
      return()
    }
    
    df[[new_col]] <- cut(
      df[[input$fe_bin_col]],
      breaks = breaks,
      include.lowest = TRUE,
      dig.lab = 8
    )
    
    engineered_data(df)
    fe_log(paste0("Created binned feature '", new_col, "' from column '",
                  input$fe_bin_col, "' with ", input$fe_bin_k, " bins."))
    showNotification(paste("Created binned feature:", new_col), type = "message")
  })
  
  observeEvent(input$apply_interaction_feature, {
    req(engineered_data(), input$fe_inter_col1, input$fe_inter_col2, input$fe_inter_new_col)
    
    df <- engineered_data()
    new_col <- trimws(input$fe_inter_new_col)
    
    if (new_col == "") {
      showNotification("Please provide a new column name.", type = "error")
      return()
    }
    
    if (new_col %in% names(df)) {
      showNotification("New column name already exists.", type = "error")
      return()
    }
    
    df[[new_col]] <- df[[input$fe_inter_col1]] * df[[input$fe_inter_col2]]
    
    engineered_data(df)
    fe_log(paste0("Created interaction feature '", new_col, "' using ",
                  input$fe_inter_col1, " * ", input$fe_inter_col2, "."))
    showNotification(paste("Created interaction feature:", new_col), type = "message")
  })
  
  observeEvent(input$apply_rename_column, {
    req(engineered_data(), input$fe_rename_old, input$fe_rename_new)
    
    df <- engineered_data()
    new_name <- trimws(input$fe_rename_new)
    
    if (new_name == "") {
      showNotification("Please provide a new column name.", type = "error")
      return()
    }
    
    if (new_name %in% names(df)) {
      showNotification("New column name already exists.", type = "error")
      return()
    }
    
    names(df)[names(df) == input$fe_rename_old] <- new_name
    engineered_data(df)
    
    fe_log(paste0("Renamed column '", input$fe_rename_old, "' to '", new_name, "'."))
    showNotification(paste("Renamed", input$fe_rename_old, "to", new_name), type = "message")
  })
  
  observeEvent(input$apply_drop_columns, {
    req(engineered_data())
    
    df <- engineered_data()
    
    if (is.null(input$fe_drop_cols) || length(input$fe_drop_cols) == 0) {
      showNotification("Please select at least one column to drop.", type = "error")
      return()
    }
    
    if (length(input$fe_drop_cols) >= ncol(df)) {
      showNotification("Cannot drop all columns.", type = "error")
      return()
    }
    
    df <- df[, !(names(df) %in% input$fe_drop_cols), drop = FALSE]
    engineered_data(df)
    
    fe_log(paste0("Dropped column(s): ", paste(input$fe_drop_cols, collapse = ", "), "."))
    showNotification("Selected columns dropped.", type = "warning")
  })
  
  output$fe_before_preview <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fe_after_preview <- renderDT({
    req(engineered_data())
    datatable(engineered_data(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fe_log <- renderText({
    fe_log()
  })
  
  # =============================================================================
  # EDA SERVER LOGIC
  # =============================================================================
  eda_data <- reactive({
    req(engineered_data())
    df <- engineered_data()
    
    if (!is.null(input$eda_columns) && length(input$eda_columns) > 0) {
      df <- df[, input$eda_columns, drop = FALSE]
    }
    
    df
  })
  
  output$eda_summary_table <- renderDT({
    req(eda_data())
    df <- eda_data()
    
    summary_df <- data.frame(
      Column = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      Missing = sapply(df, function(x) sum(is.na(x))),
      Unique_Values = sapply(df, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    numeric_cols <- sapply(df, is.numeric)
    
    summary_df$Mean <- NA
    summary_df$Median <- NA
    summary_df$SD <- NA
    summary_df$Min <- NA
    summary_df$Max <- NA
    
    if (any(numeric_cols)) {
      summary_df$Mean[numeric_cols]   <- sapply(df[numeric_cols], function(x) round(mean(x, na.rm = TRUE), 4))
      summary_df$Median[numeric_cols] <- sapply(df[numeric_cols], function(x) round(median(x, na.rm = TRUE), 4))
      summary_df$SD[numeric_cols]     <- sapply(df[numeric_cols], function(x) round(sd(x, na.rm = TRUE), 4))
      summary_df$Min[numeric_cols]    <- sapply(df[numeric_cols], function(x) round(min(x, na.rm = TRUE), 4))
      summary_df$Max[numeric_cols]    <- sapply(df[numeric_cols], function(x) round(max(x, na.rm = TRUE), 4))
    }
    
    datatable(summary_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$eda_corr_table <- renderDT({
    req(eda_data())
    df <- eda_data()
    numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
    
    if (ncol(numeric_df) < 2) {
      return(
        datatable(
          data.frame(
            Message = "At least two numeric columns are required for a correlation matrix."
          ),
          options = list(dom = "t"),
          rownames = FALSE
        )
      )
    }
    
    corr_mat <- round(cor(numeric_df, use = "pairwise.complete.obs"), 4)
    corr_df <- data.frame(Variable = rownames(corr_mat), corr_mat, check.names = FALSE)
    
    datatable(
      corr_df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$eda_corr_heatmap <- renderPlot({
    req(eda_data())
    df <- eda_data()
    numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
    
    if (ncol(numeric_df) < 2) {
      plot.new()
      text(0.5, 0.5, "At least two numeric columns are required for a correlation heatmap.")
      return()
    }
    
    corr_mat <- cor(numeric_df, use = "pairwise.complete.obs")
    
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mar = c(8, 8, 3, 2))
    
    image(
      1:ncol(corr_mat),
      1:nrow(corr_mat),
      t(corr_mat[nrow(corr_mat):1, ]),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Correlation Heatmap"
    )
    
    axis(1, at = 1:ncol(corr_mat), labels = colnames(corr_mat), las = 2)
    axis(2, at = 1:nrow(corr_mat), labels = rev(rownames(corr_mat)), las = 2)
    
    for (i in 1:nrow(corr_mat)) {
      for (j in 1:ncol(corr_mat)) {
        text(j, nrow(corr_mat) - i + 1, labels = sprintf("%.2f", corr_mat[i, j]), cex = 0.8)
      }
    }
  })
}
# ---- Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
