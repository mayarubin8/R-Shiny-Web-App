# Data Explorer — R Shiny Web Application

An interactive web application for uploading, cleaning, engineering, and exploring datasets.

## Setup

### Prerequisites

- R (>= 4.0)
- RStudio (recommended)

### Install Required Packages

```r
install.packages(c("shiny", "bslib", "DT", "readxl", "jsonlite"))
```

### Run the App

Open `app.R` in RStudio and click **Run App**, or run from the R console:

```r
shiny::runApp("app.R")
```

If you are running the project from the app folder in the R console, you can also use:

```r
setwd("path/to/R-Shiny-Web-App")
shiny::runApp(".")
```

## Project Structure

```
app.R       # Main application file (UI + Server)
README.md   # This file
```

## Features

| Tab                 | Status    | Owner         |
|---------------------|-----------|---------------|
| User Guide          | Complete  | Student 1     |
| Data Loading        | Complete  | Student 1     |
| Data Cleaning       | Complete  | Student 2     |
| Feature Engineering | Complete  | Student 3     |
| EDA                 | Complete  | Student 3     |
| Interactive EDA     | Complete  | Student 4     |

## Shared Reactive Data

All tabs share a single reactive dataset:

- **Read:** `current_data()` — returns a `data.frame` or `NULL`
- **Update:** `current_data(new_df)` — replaces the active dataset

The app also uses downstream reactive datasets for later stages of the workflow:

- **Cleaned data:** `cleaned_data()`
- **Engineered data:** `engineered_data()`
- **EDA data:** `eda_data()`
- **Interactive EDA data:** `viz_data()` and `filtered_viz_data()`

## Supported File Formats

- CSV (`.csv`)
- Excel (`.xlsx`, `.xls`)
- JSON (`.json`) — must be a flat array of objects
- RDS (`.rds`) — must contain a `data.frame`
- Max upload size: 30 MB

## Built-in Demo Datasets

- `mtcars`
- `iris`

## Data Cleaning

The Data Cleaning tab supports:

- Missing-value handling:
  - None
  - Drop
  - Mean imputation
  - Median imputation
  - Mode imputation
- Duplicate removal
- Scaling:
  - Standardize
  - Normalize
- Encoding:
  - Label
  - One-hot
- Outlier handling:
  - IQR-based removal
  - Winsorization (5th / 95th percentile)
  - Capping (1st / 99th percentile)
- Format standardization:
  - Trim whitespace in text columns
  - Convert text to lowercase
  - Convert blank / NA-like strings to missing values
  - Standardize common categorical labels
  - Convert numeric-like text columns to numeric
  - Standardize date-like text columns
- Cleaning feedback tables
- Cleaned data download

## Feature Engineering

The Feature Engineering tab supports:

- Creating math features from two numeric columns
- Creating binned features
- Creating interaction features
- Renaming columns
- Dropping columns
- Before-and-after previews
- Engineering log output
- Feature change summary
- Engineered data download

## EDA

The EDA tab includes:

- Summary statistics table
- Correlation matrix
- Correlation heatmap
- Dynamic column selection

## Interactive EDA

The Interactive EDA tab includes:

- Interactive Plotly visualizations:
  - Histogram
  - Boxplot
  - Scatter Plot
  - Bar Chart
- Dynamic row filtering:
  - Numeric range filter
  - Categorical filter
  - Date range filter
- Grouped summary table
- Filtered dataset overview
- Dynamic plot insights
- Filtered data preview
- Filtered data download

## Notes

- The app is designed to work with uploaded datasets that have different column names and structures.
- Variable selectors update dynamically when datasets change or when engineered columns are added, renamed, or dropped.
- The app follows the workflow: **Load → Clean → Engineer → Explore**.

## Repository

GitHub Repository: https://github.com/mayarubin8/R-Shiny-Web-App

## Deployment

Deployed Application: https://rui-lin.shinyapps.io/r-shiny-web-app/

