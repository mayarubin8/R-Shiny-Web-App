# =============================================================================
# Project 2: Data Explorer — R Shiny Web Application
# =============================================================================
# Student 1: App Skeleton, Data Loading, User Guide
#
# SHARED REACTIVE DATA:
#   current_data()  — reactiveVal holding the active dataset (data.frame or NULL)
#   Read with:       current_data()
#   Update with:     current_data(new_df)
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
  "mtcars (Motor Trend Cars)"    = "mtcars",
  "iris (Fisher's Iris)"         = "iris"
)

# ---- Custom CSS --------------------------------------------------------------
custom_css <- "
/* ---- Global ---- */
body {
  background-color: #f5f7fa;
  font-family: 'Segoe UI', system-ui, -apple-system, sans-serif;
}

/* ---- Navbar ---- */
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

/* ---- Hero / Welcome ---- */
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

/* ---- Step Cards ---- */
.step-card {
  border: none;
  border-radius: 14px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.06);
  transition: transform 0.25s ease, box-shadow 0.25s ease;
  overflow: hidden;
  height: 100%;
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

/* ---- Tips Card ---- */
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

/* ---- Data Loading ---- */
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

/* ---- Value Boxes ---- */
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

/* ---- Status Banners ---- */
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

/* ---- Data Preview Card ---- */
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

/* ---- Placeholder Tabs ---- */
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

/* ---- Footer ---- */
.app-footer {
  text-align: center;
  padding: 1.5rem;
  margin-top: 3rem;
  color: #aaa;
  font-size: 0.85rem;
  border-top: 1px solid #e9ecef;
}

/* ---- Button polish ---- */
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

/* ---- File input polish ---- */
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

  # ============================================================
  # TAB 1: USER GUIDE (Student 1)
  # ============================================================
  tabPanel(
    "User Guide",
    icon = icon("book"),
    fluidPage(
      class = "mt-4 px-3",

      # Hero banner
      div(
        class = "hero-banner",
        h1("Welcome to Data Explorer"),
        p(class = "lead",
          "Upload, clean, transform, and visualize your datasets
           — all from one interactive dashboard.")
      ),

      # Step cards
      fluidRow(
        # Step 1
        column(6, class = "mb-4",
          div(class = "step-card",
            div(class = "step-header",
              span(class = "step-icon-circle",
                   style = "background: linear-gradient(135deg, #4361ee, #3a86ff);",
                   icon("upload")),
              span("Step 1: Load Your Data")
            ),
            div(class = "step-body",
              p("Head to the", strong("Data Loading"), "tab to get started."),
              tags$ul(
                tags$li("Upload CSV, Excel (.xlsx/.xls), JSON, or RDS files (up to 30 MB)."),
                tags$li("Or pick a built-in demo dataset to explore the app instantly."),
                tags$li("Preview your data in an interactive, searchable table.")
              )
            )
          )
        ),

        # Step 2
        column(6, class = "mb-4",
          div(class = "step-card",
            div(class = "step-header",
              span(class = "step-icon-circle",
                   style = "background: linear-gradient(135deg, #059669, #34d399);",
                   icon("broom")),
              span("Step 2: Clean & Preprocess")
            ),
            div(class = "step-body",
              p("Use the", strong("Data Cleaning"), "tab to prepare your data."),
              tags$ul(
                tags$li("Handle missing values with drop or imputation strategies."),
                tags$li("Remove duplicate rows and standardize formats."),
                tags$li("Scale, normalize, and encode categorical variables."),
                tags$li("Detect and handle outliers.")
              )
            )
          )
        ),

        # Step 3
        column(6, class = "mb-4",
          div(class = "step-card",
            div(class = "step-header",
              span(class = "step-icon-circle",
                   style = "background: linear-gradient(135deg, #f59e0b, #fbbf24);",
                   icon("wrench")),
              span("Step 3: Engineer Features")
            ),
            div(class = "step-body",
              p("Visit the", strong("Feature Engineering"), "tab to create new variables."),
              tags$ul(
                tags$li("Build new columns from math expressions, binning, or interactions."),
                tags$li("Rename or drop existing columns."),
                tags$li("See before-and-after previews of every change.")
              )
            )
          )
        ),

        # Step 4
        column(6, class = "mb-4",
          div(class = "step-card",
            div(class = "step-header",
              span(class = "step-icon-circle",
                   style = "background: linear-gradient(135deg, #8b5cf6, #a78bfa);",
                   icon("chart-bar")),
              span("Step 4: Explore & Visualize")
            ),
            div(class = "step-body",
              p("Go to the", strong("EDA"), "tab to uncover insights."),
              tags$ul(
                tags$li("View summary statistics and correlation matrices."),
                tags$li("Create interactive plots — histograms, boxplots, scatter plots, and more."),
                tags$li("Filter and subset your data dynamically.")
              )
            )
          )
        )
      ),

      # Tips section
      card(
        class = "tips-card mt-2",
        card_header(
          span(icon("lightbulb"), " Tips for Best Results")
        ),
        card_body(
          tags$ul(
            tags$li("Start with the", strong("Data Loading"), "tab — all other
                     tabs depend on having a dataset loaded."),
            tags$li("Work through the tabs left to right:
                     Load \u2192 Clean \u2192 Engineer \u2192 Explore.")
          )
        )
      ),

      # Footer
      div(class = "app-footer",
        "Data Explorer \u2014 Applied Data Science, Spring 2026"
      )
    )
  ),

  # ============================================================
  # TAB 2: DATA LOADING (Student 1)
  # ============================================================
  tabPanel(
    "Data Loading",
    icon = icon("upload"),
    fluidPage(
      class = "mt-4 px-3",

      fluidRow(
        # Left column: upload controls
        column(3,
          div(class = "upload-section",
            h4(icon("file-arrow-up"), " Upload a File"),
            fileInput(
              "file_upload", NULL,
              accept = c(".csv", ".xlsx", ".xls", ".json", ".rds"),
              placeholder = "CSV, Excel, JSON, or RDS"
            ),
            helpText("Supported: .csv, .xlsx, .xls, .json, .rds",
                     br(), "Max size: 30 MB"),

            div(class = "section-divider", "OR"),

            h4(icon("database"), " Demo Dataset"),
            selectInput(
              "demo_data", NULL,
              choices = demo_choices
            ),
            actionButton(
              "load_demo", "Load Demo Dataset",
              class = "btn-primary w-100",
              icon = icon("play")
            ),

            # Dataset summary (value boxes, shown after data is loaded)
            uiOutput("data_summary_panel")
          )
        ),

        # Right column: status + data preview
        column(9,
          uiOutput("load_status"),
          div(class = "preview-card",
            h5(icon("table"), " Data Preview"),
            DTOutput("data_preview")
          )
        )
      ),

      # Footer
      div(class = "app-footer",
        "Data Explorer \u2014 Applied Data Science, Spring 2026"
      )
    )
  ),

  # ============================================================
  # TAB 3: DATA CLEANING (Student 2)
  # Access the shared dataset via current_data()
  # Update with current_data(cleaned_df)
  # ============================================================
  tabPanel(
    "Data Cleaning",
    icon = icon("broom"),
    fluidPage(
      class = "mt-4",
      div(class = "placeholder-tab",
        div(class = "placeholder-icon", icon("broom")),
        h3("Data Cleaning & Preprocessing"),
        p("This tab will be implemented by Student 2."),
        p("It will include: missing value handling, duplicate removal,
           scaling/normalization, categorical encoding, and outlier handling.")
      )
    )
  ),

  # ============================================================
  # TAB 4: FEATURE ENGINEERING (Student 3)
  # Access the shared dataset via current_data()
  # Update with current_data(engineered_df)
  # ============================================================
  tabPanel(
    "Feature Engineering",
    icon = icon("wrench"),
    fluidPage(
      class = "mt-4",
      div(class = "placeholder-tab",
        div(class = "placeholder-icon", icon("wrench")),
        h3("Feature Engineering"),
        p("This tab will be implemented by Student 3."),
        p("It will include: creating new features, renaming/dropping columns,
           and before/after visual feedback.")
      )
    )
  ),

  # ============================================================
  # TAB 5: EDA (Students 3 & 4)
  # Access the shared dataset via current_data()
  # ============================================================
  tabPanel(
    "EDA",
    icon = icon("chart-bar"),
    fluidPage(
      class = "mt-4",
      div(class = "placeholder-tab",
        div(class = "placeholder-icon", icon("chart-bar")),
        h3("Exploratory Data Analysis"),
        p("This tab will be implemented by Students 3 & 4."),
        p("It will include: summary statistics, correlation matrix,
           interactive plots, and dynamic filtering.")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---- SHARED REACTIVE: current_data() ---------------------------------------
  # This is the SINGLE SOURCE OF TRUTH for the active dataset.
  #   Read:   current_data()        (returns a data.frame or NULL)
  #   Update: current_data(new_df)
  # All tabs should use current_data() to access the dataset.
  # --------------------------------------------------------------------------
  current_data <- reactiveVal(NULL)

  # Track the name of the currently loaded dataset for display
  data_name <- reactiveVal(NULL)

  # ---- DATA LOADING: File upload ---------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)

    tryCatch({
      df <- read_uploaded_file(
        input$file_upload$datapath,
        input$file_upload$name
      )
      current_data(df)
      data_name(input$file_upload$name)

      # Reset demo selector to avoid confusion
      updateSelectInput(session, "demo_data", selected = "none")

      showNotification(
        paste0("Successfully loaded '", input$file_upload$name,
               "' (", nrow(df), " rows, ", ncol(df), " columns)"),
        type = "message", duration = 5
      )
    },
    error = function(e) {
      current_data(NULL)
      data_name(NULL)
      showNotification(
        paste("Error reading file:", e$message),
        type = "error", duration = 8
      )
    })
  })

  # ---- DATA LOADING: Demo dataset --------------------------------------------
  observeEvent(input$load_demo, {
    req(input$demo_data != "none")

    df <- switch(input$demo_data,
      "mtcars"     = mtcars,
      "iris"       = iris
    )

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

  # ---- DATA LOADING: Status banner -------------------------------------------
  output$load_status <- renderUI({
    if (is.null(current_data())) {
      div(
        class = "status-banner status-info",
        icon("info-circle"),
        span("No dataset loaded yet. Upload a file or select a demo dataset
              from the sidebar to get started.")
      )
    } else {
      div(
        class = "status-banner status-success",
        icon("check-circle"),
        span(paste0("Dataset loaded: ", data_name(),
               " \u2014 ", nrow(current_data()), " rows, ",
               ncol(current_data()), " columns"))
      )
    }
  })

  # ---- DATA LOADING: Summary panel — value boxes -----------------------------
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
          span(class = "stat-label", "Rows")
        ),
        div(class = "stat-box",
          span(class = "stat-value", ncol(df)),
          span(class = "stat-label", "Columns")
        ),
        div(class = "stat-box",
          span(class = "stat-value", num_cols),
          span(class = "stat-label", "Numeric")
        ),
        div(class = "stat-box",
          span(class = "stat-value", char_cols + factor_cols),
          span(class = "stat-label", "Categorical")
        ),
        div(class = "stat-box",
          span(class = "stat-value", missing),
          span(class = "stat-label", "Missing")
        )
      )
    )
  })

  # ---- DATA LOADING: Interactive data preview table --------------------------
  output$data_preview <- renderDT({
    req(current_data())
    datatable(
      current_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          emptyTable = "No data to display"
        )
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  # ============================================================
  # DATA CLEANING SERVER LOGIC (Student 2)
  # Use current_data() to read the active dataset.
  # Update with current_data(cleaned_df) after cleaning.
  # ============================================================
cleaned_data <- reactive({

  req(current_data())
  df <- current_data()

  # ---- 1. Missing Values ------------------------------------
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
        ux <- na.omit(unique(x))
        ux[which.max(tabulate(match(x, ux)))]
      }
      df[] <- lapply(df, function(x) {
        x[is.na(x)] <- mode_func(x)
        x
      })
    }
  }

  # ---- 2. Remove Duplicates ---------------------------------
  if (!is.null(input$remove_dup) && input$remove_dup) {
    df <- unique(df)
  }

  # ---- 3. Scaling / Normalization ----------------------------
  if (!is.null(input$scaling) && input$scaling != "None") {
    num_cols <- sapply(df, is.numeric)

    if (input$scaling == "Standardize") {
      df[num_cols] <- scale(df[num_cols])

    } else if (input$scaling == "Normalize") {
      df[num_cols] <- lapply(df[num_cols], function(x) {
        (x - min(x, na.rm = TRUE)) / 
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      })
    }
  }

  # ---- 4. Encoding ------------------------------------------
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
      df <- as.data.frame(model.matrix(~ . - 1, data = df))
    }
  }

  # ---- 5. Outliers ------------------------------------------
  if (!is.null(input$remove_outliers) && input$remove_outliers) {
    num_cols <- sapply(df, is.numeric)

    for (col in names(df)[num_cols]) {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1

      df <- df[df[[col]] >= (Q1 - 1.5 * IQR_val) &
               df[[col]] <= (Q3 + 1.5 * IQR_val), ]
    }
  }

  return(df)
})

  # ============================================================
  # FEATURE ENGINEERING SERVER LOGIC (Student 3)
  # Use current_data() to read the active dataset.
  # Update with current_data(engineered_df) after transformations.
  # ============================================================


  # ============================================================
  # EDA SERVER LOGIC (Students 3 & 4)
  # Use current_data() to read the active dataset.
  # ============================================================

}

# ---- Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
