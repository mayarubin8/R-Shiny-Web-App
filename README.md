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

## Project Structure

```
app.R       # Main application file (UI + Server)
README.md   # This file
```

## Features

| Tab                 | Status       | Owner     |
|---------------------|--------------|-----------|
| User Guide          | Complete     | Student 1 |
| Data Loading        | Complete     | Student 1 |
| Data Cleaning       | Placeholder  | Student 2 |
| Feature Engineering | Placeholder  | Student 3 |
| EDA                 | Placeholder  | Student 3 & 4 |

## Shared Reactive Data

All tabs share a single reactive dataset:

- **Read:** `current_data()` — returns a `data.frame` or `NULL`
- **Update:** `current_data(new_df)` — replaces the active dataset

## Supported File Formats

- CSV (`.csv`)
- Excel (`.xlsx`, `.xls`)
- JSON (`.json`) — must be a flat array of objects
- RDS (`.rds`) — must contain a `data.frame`
- Max upload size: 30 MB
