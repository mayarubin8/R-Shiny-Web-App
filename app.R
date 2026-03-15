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

# =============================================================================
# UI
# =============================================================================
ui <- navbarPage(
  title = "Data Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # ============================================================
  # TAB 1: USER GUIDE (Student 1)
  # ============================================================
  tabPanel(
    "User Guide",
    icon = icon("book"),
    fluidPage(
      class = "mt-4",

      # Welcome header
      div(
        class = "text-center mb-4",
        h1("Welcome to Data Explorer"),
        p(class = "lead text-muted",
          "An interactive tool for uploading, cleaning, engineering,
           and exploring your datasets — all in one place.")
      ),

      # Step-by-step guide cards
      layout_column_wrap(
        width = 1 / 2,
        fill = FALSE,

        card(
          card_header(
            class = "bg-primary text-white",
            span(icon("upload"), " Step 1: Load Your Data")
          ),
          card_body(
            p("Head to the", strong("Data Loading"), "tab to get started."),
            tags$ul(
              tags$li("Upload your own file in CSV, Excel (.xlsx/.xls),
                       JSON, or RDS format (up to 30 MB)."),
              tags$li("Or select a built-in demo dataset (mtcars or iris)
                       to explore the app right away."),
              tags$li("A preview table will appear so you can verify
                       your data loaded correctly.")
            )
          )
        ),

        card(
          card_header(
            class = "bg-success text-white",
            span(icon("broom"), " Step 2: Clean & Preprocess")
          ),
          card_body(
            p("Use the", strong("Data Cleaning"), "tab to prepare your data."),
            tags$ul(
              tags$li("Handle missing values (drop rows or impute
                       with mean / median / mode)."),
              tags$li("Remove duplicate rows."),
              tags$li("Scale or normalize numeric columns."),
              tags$li("Encode categorical variables
                       (one-hot or label encoding)."),
              tags$li("Detect and handle outliers.")
            )
          )
        ),

        card(
          card_header(
            class = "bg-warning text-dark",
            span(icon("wrench"), " Step 3: Engineer Features")
          ),
          card_body(
            p("Visit the", strong("Feature Engineering"), "tab to create
               new variables."),
            tags$ul(
              tags$li("Build new columns from math expressions,
                       binning, or interactions."),
              tags$li("Rename or drop existing columns."),
              tags$li("See before-and-after previews of your changes.")
            )
          )
        ),

        card(
          card_header(
            class = "bg-info text-white",
            span(icon("chart-bar"), " Step 4: Explore & Visualize")
          ),
          card_body(
            p("Go to the", strong("EDA"), "tab to understand your data."),
            tags$ul(
              tags$li("View summary statistics and correlation matrices."),
              tags$li("Create interactive plots — histograms, boxplots,
                       scatter plots, and more."),
              tags$li("Filter and subset your data dynamically.")
            )
          )
        )
      ),

      # Tips section
      card(
        class = "mt-3",
        card_header(
          span(icon("lightbulb"), " Tips for Best Results")
        ),
        card_body(
          tags$ul(
            tags$li("Start with the", strong("Data Loading"), "tab — all other
                     tabs depend on having a dataset loaded."),
            tags$li("Work through the tabs left to right:
                     Load \u2192 Clean \u2192 Engineer \u2192 Explore."),
          )
        )
      )
    )
  ),

  # ============================================================
  # TAB 2: DATA LOADING (Student 1)
  # ============================================================
  tabPanel(
    "Data Loading",
    icon = icon("upload"),
    sidebarLayout(

      # -- Sidebar: upload controls --
      sidebarPanel(
        width = 3,

        h4("Upload a File"),
        fileInput(
          "file_upload", NULL,
          accept = c(".csv", ".xlsx", ".xls", ".json", ".rds"),
          placeholder = "CSV, Excel, JSON, or RDS"
        ),
        helpText("Supported formats: .csv, .xlsx, .xls, .json, .rds",
                 br(), "Max file size: 30 MB"),

        hr(),

        h4("Or Use a Demo Dataset"),
        selectInput(
          "demo_data", NULL,
          choices = demo_choices
        ),
        actionButton(
          "load_demo", "Load Demo Dataset",
          class = "btn-primary w-100",
          icon = icon("database")
        ),

        hr(),

        # Dataset summary (shown after data is loaded)
        uiOutput("data_summary_panel")
      ),

      # -- Main panel: data preview --
      mainPanel(
        width = 9,
        uiOutput("load_status"),
        DTOutput("data_preview")
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
      class = "mt-4 text-center text-muted",
      h3("Data Cleaning & Preprocessing"),
      p("This tab will be implemented by Student 2."),
      p("It will include: missing value handling, duplicate removal,
         scaling/normalization, categorical encoding, and outlier handling.")
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
      class = "mt-4 text-center text-muted",
      h3("Feature Engineering"),
      p("This tab will be implemented by Student 3."),
      p("It will include: creating new features, renaming/dropping columns,
         and before/after visual feedback.")
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
      class = "mt-4 text-center text-muted",
      h3("Exploratory Data Analysis"),
      p("This tab will be implemented by Students 3 & 4."),
      p("It will include: summary statistics, correlation matrix,
         interactive plots, and dynamic filtering.")
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
        class = "alert alert-info mt-3",
        icon("info-circle"),
        " No dataset loaded yet. Upload a file or select a demo dataset
          from the sidebar to get started."
      )
    } else {
      div(
        class = "alert alert-success mt-3",
        icon("check-circle"),
        paste0(" Dataset loaded: ", data_name(),
               " \u2014 ", nrow(current_data()), " rows, ",
               ncol(current_data()), " columns")
      )
    }
  })

  # ---- DATA LOADING: Summary panel in sidebar --------------------------------
  output$data_summary_panel <- renderUI({
    req(current_data())
    df <- current_data()

    num_cols <- sum(sapply(df, is.numeric))
    char_cols <- sum(sapply(df, is.character))
    factor_cols <- sum(sapply(df, is.factor))
    missing <- sum(is.na(df))

    tagList(
      h5("Dataset Summary"),
      tags$table(
        class = "table table-sm table-borderless",
        tags$tr(tags$td("Rows:"),        tags$td(strong(nrow(df)))),
        tags$tr(tags$td("Columns:"),     tags$td(strong(ncol(df)))),
        tags$tr(tags$td("Numeric:"),     tags$td(num_cols)),
        tags$tr(tags$td("Character:"),   tags$td(char_cols)),
        tags$tr(tags$td("Factor:"),      tags$td(factor_cols)),
        tags$tr(tags$td("Missing vals:"),tags$td(missing))
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
