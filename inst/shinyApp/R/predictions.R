# =============================================================================
# Prediction Logic
# Handles variable classification using trained clustering model
# =============================================================================

#' Run prediction workflow on new variables
#' @param model Trained clustering model object with predict() method
#' @param file_path Path to uploaded prediction file
#' @param header Whether file has header
#' @param sep Column separator
#' @return data.frame with predictions (variable, cluster, distance)
run_prediction_workflow <- function(model, file_path, header = TRUE, sep = ",") {
  # Load prediction data
  pred_data <- read.table(
    file_path,
    header = header,
    sep = sep,
    stringsAsFactors = FALSE
  )
  
  # Get numeric variables
  pred_numeric <- pred_data[, sapply(pred_data, is.numeric), drop = FALSE]
  
  if (ncol(pred_numeric) == 0) {
    stop("No numeric variables found in prediction file")
  }
  
  # Make predictions using model
  predictions <- model$predict(pred_numeric)
  
  return(predictions)
}

#' Validate prediction data compatibility
#' @param pred_data data.frame Prediction dataset
#' @param train_data data.frame Training dataset (optional, for row count check)
#' @return List with valid (logical) and message (character)
validate_prediction_data <- function(pred_data, train_data = NULL) {
  # Check for numeric variables
  numeric_vars <- sapply(pred_data, is.numeric)
  
  if (sum(numeric_vars) == 0) {
    return(list(valid = FALSE, message = "No numeric variables found"))
  }
  
  # Check row count compatibility if training data provided
  if (!is.null(train_data)) {
    if (nrow(pred_data) != nrow(train_data)) {
      return(list(
        valid = FALSE, 
        message = sprintf(
          "Row count mismatch: prediction data has %d rows, training data has %d rows",
          nrow(pred_data), nrow(train_data)
        )
      ))
    }
  }
  
  return(list(valid = TRUE, message = "Valid"))
}
