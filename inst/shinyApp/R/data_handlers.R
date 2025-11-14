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
#' @return data.frame with loaded data
load_uploaded_data <- function(file_path, header = TRUE, sep = ",") {
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
