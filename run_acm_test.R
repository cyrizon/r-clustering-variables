data_path <- "datasets/vote_catvarclus.csv"
# Quick test: run ClustVarACM on vote_catvarclus.csv
# Minimal runtime checks and robust CSV reading (handle semicolon separators)

# Ensure required packages
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  stop("Package 'FactoMineR' is required but not installed. Please install it and retry.")
}
if (!requireNamespace("R6", quietly = TRUE)) stop("R6 package required")

# Source the class file
source("R/ClustVarACM.R")

# Read dataset with robust separator detection and fallbacks
data_path <- "datasets/vote_catvarclus.csv"
if (!file.exists(data_path)) stop(paste("Data file not found:", data_path))

read_with_fallback <- function(path) {
  # Try common variants: semicolon first, then read.csv2, then default comma
  attempts <- list(
    function() read.csv(path, sep = ";", stringsAsFactors = TRUE, check.names = FALSE),
    function() read.csv2(path, stringsAsFactors = TRUE, check.names = FALSE),
    function() read.csv(path, stringsAsFactors = TRUE, check.names = FALSE)
  )
  for (fn in attempts) {
    res <- tryCatch(fn(), error = function(e) e)
    if (!inherits(res, "error")) return(res)
  }
  stop("All attempts to read CSV failed. Inspect file format.")
}

X <- read_with_fallback(data_path)

# If reading produced a single column, print diagnostics and try to detect separator
if (ncol(X) == 1) {
  cat("Warning: file read resulted in a single column. Showing first 5 raw lines to help diagnose separator issues:\n")
  raw_lines <- readLines(data_path, n = 5, warn = FALSE)
  cat(paste0(seq_along(raw_lines), ": ", raw_lines, "\n"), sep = "")
  # Try a last-ditch attempt using read.table with auto-detected sep via first line
  first_line <- raw_lines[1]
  guessed_sep <- if (grepl(";", first_line)) ";" else if (grepl("\t", first_line)) "\t" else ","
  cat(sprintf("Trying read.table with guessed separator '%s'...\n", guessed_sep))
  X2 <- tryCatch(read.table(data_path, sep = guessed_sep, header = TRUE, stringsAsFactors = TRUE, check.names = FALSE), error = function(e) e)
  if (!inherits(X2, "error") && ncol(X2) > 1) {
    X <- X2
    cat("Successfully re-read file with guessed separator.\n")
  } else {
    cat("Could not re-parse file into multiple columns. Please confirm the file uses semicolons ';' as separators.\n")
  }
}

# Ensure columns are factors
X[] <- lapply(X, function(col) if (!is.factor(col)) as.factor(col) else col)

cat("Dataset loaded: nrow=", nrow(X), " ncol=", ncol(X), "\n")

# Instantiate and fit model
model <- ClustVarACM$new(K = 3, max_iter = 30, tol = 1e-4)
tryCatch({
  model$fit(X)
  cat("Model fitted. Q_final=", model$Q_final, "\n")
  print(model$summary())
}, error = function(e) {
  cat("Error during model fitting:\n")
  cat(conditionMessage(e), "\n")
  quit(status = 2)
})

cat("Done.\n")
