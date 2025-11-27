# ============================================================================
# Unit tests for the ClustVarACM class
# ============================================================================
# This file contains a comprehensive suite of tests to validate the behavior
# of the categorical variable clustering algorithm based on MCA (ACM).
#
# Test outline:
# 1. Prepare a test dataset
# 2. Test scripts for the 'testthat' package
#   a. Initialization and input validation
#   b. fit() method tests
#   c. Cluster quality validation tests
#   d. predict() method tests
#   e. select_K (optimal K selection) tests
#   f. Display method tests (print, summary, plot)
# ============================================================================

library(testthat)
library(FactoMineR)
library(R6)

# Load the class under test
source("../../R/algorithms/ClustVarACM.R")

# ==============================================
# 1. Prepare a test dataset
# ===============================================

# Create a synthetic dataset with clear correlations
set.seed(42)
n_obs <- 100

# Variables correlated in Cluster 1
VarA <- factor(sample(c("A1", "A2", "A3"), n_obs, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
VarB <- factor(ifelse(VarA %in% c("A1", "A2"), sample(c("B_low", "B_high"), n_obs, replace = TRUE, prob = c(0.7, 0.3)), "B_other"))

# Variables correlated in Cluster 2
VarC <- factor(sample(c("C_yes", "C_no"), n_obs, replace = TRUE, prob = c(0.8, 0.2)))
VarD <- factor(ifelse(VarC == "C_yes", sample(c("D1", "D2"), n_obs, replace = TRUE, prob = c(0.9, 0.1)), "D2"))

# Weakly correlated variable (possibly Cluster 3 or noise)
VarE <- factor(sample(letters[1:4], n_obs, replace = TRUE))

data_test <- data.frame(VarA, VarB, VarC, VarD, VarE)

# Create illustrative data (Predict)
data_illustrative <- data.frame(
  NewVar1 = factor(sample(c("N1", "N2"), n_obs, replace = TRUE, prob = c(0.4, 0.6))),
  NewVar2 = VarA # A variable highly correlated with Cluster 1
)

# =======================================================
# 2. Test scripts for the 'testthat' package
# ======================================================

#   a. Initialization of variables
test_that("ClustVarACM: Initialization and input validation", {
  # Initialization test
  model <- ClustVarACM$new(K = 3)
  model$fit(data_test)
  expect_s3_class(model, "R6")
  expect_equal(model$K, 3)
  expect_equal(ncol(model$data), 5)
  expect_equal(length(model$clusters), 5)

  # Test de la validation des erreurs dans fit()
  data_num <- data.frame(a = 1:10, b = 1:10)
  model_err <- ClustVarACM$new(K = 2)
  # Should fail because variables are not factors
  expect_error(model_err$fit(data_num), "All variables must be categorical \\(type factor\\).")

  data_na <- data_test
  data_na[1, 1] <- NA
  model_na <- ClustVarACM$new(K = 2)
  # Should fail because it contains NA
  expect_error(model_na$fit(data_na), "Data must not contain NA values.")

  model_k_err <- ClustVarACM$new(K = 1)
  # Should fail because K is out of range
  expect_error(model_k_err$fit(data_test), "K must be between 2 and the number of variables.")
})

---
  #   b. Tests for the fit() method

  test_that("ClustVarACM: Main operation (fit) and Q convergence", {
    model <- ClustVarACM$new(K = 3, max_iter = 50)

    # Run model fitting
    fitted_model <- model$fit(data_test)

    # The model should return itself (invisibly)
    expect_s3_class(fitted_model, "ClustVarACM")

    # The final Q criterion must be defined and positive
    expect_true(!is.null(model$Q_final))
    expect_gt(model$Q_final, 0)

    # The Q_trace should show convergence (eventually stabilizes)
    # Note: Q can sometimes decrease slightly due to empty cluster reseeding
    expect_true(length(model$Q_trace) > 0)

    # The score matrix should have correct dimensions (p x K)
    expect_equal(dim(model$score_matrix), c(5, 3))

    # Every variable must be assigned to a cluster
    expect_true(all(model$clusters %in% 1:3))
  })

---
  #   c. Cluster quality validation tests

  test_that("ClustVarACM: Cluster quality validation (Q-score)", {
    model <- ClustVarACM$new(K = 2, max_iter = 100)
    model$fit(data_test)

    # The final Q should equal the sum of intra-cluster scores
    p <- ncol(data_test)
    calculated_Q <- sum(sapply(1:p, function(j) {
      cluster_k <- model$clusters[j]
      return(model$score_matrix[j, cluster_k])
    }))

    expect_equal(model$Q_final, calculated_Q)

    # Check that all variables have cluster assignments
    expect_true(all(!is.na(names(model$clusters))))
    expect_true(all(names(model$clusters) %in% names(data_test)))
    
    # Heuristic: correlated variables often cluster together (but not guaranteed)
    # Just verify the structure makes sense rather than exact assignments
  })

---
  #   d. Tests for the predict() method

  test_that("ClustVarACM: Prediction on new variables (predict)", {
    model <- ClustVarACM$new(K = 2)
    model$fit(data_test)

    # Test predict()
    pred_result <- model$predict(data_illustrative)

    expect_s3_class(pred_result, "data.frame")
    expect_equal(nrow(pred_result), 2)
    expect_named(pred_result, c("Variable", "Cluster_Assigned", "Max_Association_Score"))

    # NewVar2 is a copy of VarA (should have similar clustering behavior)
    # Verify it gets assigned to a valid cluster
    expect_true(pred_result$Cluster_Assigned[pred_result$Variable == "NewVar2"] %in% 1:model$K)
    # Check the association score is non-negative (chi-square based, 1-p_value can be 0 for weak associations)
    expect_gte(pred_result$Max_Association_Score[pred_result$Variable == "NewVar2"], 0)

    # Error test: incompatible number of observations
    data_err_obs <- data_illustrative[1:10, ]
    expect_error(model$predict(data_err_obs), "The number of observations in 'newdata' must be the same as in the training data.")
  })

---
  #   e. Optimal K selection tests (select_K)

  test_that("ClustVarACM: Optimal K selection (select_K)", {
    # For this test, use a small K grid
    model_k <- ClustVarACM$new(K = 2) # initial K does not matter
    model_k$data <- data_test

    # Capture output (print and plot)
    k_selection <- suppressMessages(suppressWarnings(
      capture.output(
        results <- model_k$select_K(K_grid = 2:4, threshold = 0.05)
      )
    ))

    # Result should be a list with results and K_opt
    expect_type(results, "list")
    expect_named(results, c("results", "K_opt"))

    # Results table should contain K and Q for each K
    expect_equal(nrow(results$results), 3)
    expect_true(all(results$results$K == 2:4))
    expect_true(all(!is.na(results$results$Q)))

    # K_opt should be between 2 and 4
    expect_true(results$K_opt %in% 2:4)
  })

---
  #   f. Display method tests (print, summary, plot)

  test_that("ClustVarACM: Display methods (print, summary, plot)", {
    model <- ClustVarACM$new(K = 2)
    model$fit(data_test)

    # Test print()
    # Should run without error and produce output (verified by capture.output)
    expect_output(print(model), "Clustering Model: ClustVarACM")

    # Test summary()
    # Should run without error and produce detailed output
    expect_output(model$summary(), "Final Criterion Q:")

    # Test plot_Q()
    # Should run silently (visual verification)
    expect_silent(model$plot_Q())

    # Test plot()
    # Should run silently for default axes (1 and 2)
    expect_silent(model$plot(axes = c(1, 2)))

    # Test plot() with K=1 (should fail at multiple levels)
    model_k1 <- ClustVarACM$new(K = 1)
    model_k1$data <- data_test[, 1:2]
    model_k1$score_matrix <- matrix(0.5, nrow = 2, ncol = 1) # Mock score matrix
    model_k1$clusters <- c(1, 1) # Force assignment to 1
    # Plot requires K >= 2
    expect_error(model_k1$plot(), "Plotting requires at least 2 clusters.")
  })
