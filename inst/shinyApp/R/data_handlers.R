#' Auto-detect separator from file
#' @param file_path Path to file
#' @return character separator (comma, semicolon, or tab)
detect_separator <- function(file_path) {
  # Read first few lines
  lines <- readLines(file_path, n = 5, warn = FALSE)

  # Count separators
  comma_count <- sum(grepl(",", lines))
  semicolon_count <- sum(grepl(";", lines))
  tab_count <- sum(grepl("\t", lines))

  # Return most common
  counts <- c("," = comma_count, ";" = semicolon_count, "\t" = tab_count)
  return(names(counts)[which.max(counts)])
}

# =============================================================================
# Data Loading and Validation Helpers
# Handles data loading from files and example datasets
# =============================================================================

#' Load example College dataset or generate synthetic data
#' @param data_path Path to College_Data file
#' @return data.frame with example data
load_example_data <- function(data_path = "../../tests/testthat/College_Data") {
  if (file.exists(data_path)) {
    data <- read.csv(data_path, row.names = 1)
    message("Example data loaded from file")
    return(data)
  } else {
    # Generate synthetic data if example not found
    set.seed(42)
    n <- 100
    data <- data.frame(
      Var1 = rnorm(n),
      Var2 = rnorm(n) + rnorm(n, sd = 0.3),
      Var3 = rnorm(n),
      Var4 = rnorm(n) * 2,
      Var5 = rnorm(n) + rnorm(n, sd = 0.5),
      Var6 = rnorm(n)
    )
    message("Synthetic example data generated")
    return(data)
  }
}

#' Load uploaded data file
#' @param file_path Path to uploaded file
#' @param header Whether file has header row
#' @param sep Column separator
#' @param sep Column separator (if NULL, auto-detect)
#' @return data.frame with loaded data
load_uploaded_data <- function(file_path, header = TRUE, sep = NULL) {
  # Auto-detect separator if not provided
  if (is.null(sep)) {
    sep <- detect_separator(file_path)
    message(paste("Auto-detected separator:", sep))
  }

  data <- read.table(
    file_path,
    header = header,
    sep = sep,
    stringsAsFactors = FALSE
  )
  return(data)
}

#' Extract numeric variable names from dataset
#' @param data data.frame
#' @return character vector of numeric column names
extract_numeric_vars <- function(data) {
  names(data)[sapply(data, is.numeric)]
}

#' Extract categorical variable names from dataset
#' @param data data.frame
#' @return character vector of categorical column names
extract_categorical_vars <- function(data) {
  names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
}

#' Detect dataset type based on variable composition
#' @param data data.frame
#' @return character "numeric", "categorical", or "mixed"
detect_dataset_type <- function(data) {
  n_numeric <- length(extract_numeric_vars(data))
  n_categorical <- length(extract_categorical_vars(data))

  if (n_numeric > 0 && n_categorical == 0) {
    return("numeric")
  } else if (n_categorical > 0 && n_numeric == 0) {
    return("categorical")
  } else {
    return("mixed")
  }
}
