# ============================================================================
# Unit tests for the ACMVariablesR6 class
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
source("../../R/ClustVarACMR6.R")

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
  model <- ClustVarACM$new(data = data_test, K = 3)
  expect_s3_class(model, "R6")
  expect_equal(model$K, 3)
  expect_equal(ncol(model$data), 5)
  expect_true(length(model$clusters) == 5)

  # Test de la validation des erreurs dans fit()
  data_num <- data.frame(a = 1:10, b = 1:10)
  model_err <- ClustVarACM$new(data = data_num, K = 2)
  # Should fail because variables are not factors
  expect_error(model_err$fit(), "All variables must be categorical \\(type factor\\).")

  data_na <- data_test
  data_na[1, 1] <- NA
  model_na <- ClustVarACM$new(data = data_na, K = 2)
  # Should fail because it contains NA
  expect_error(model_na$fit(), "Data must not contain NA values.")

  model_k_err <- ClustVarACM$new(data = data_test, K = 1)
  # Should fail because K is out of range
  expect_error(model_k_err$fit(), "K must be between 2 and the number of variables.")
})

---
  #   b. Tests for the fit() method

  test_that("ClustVarACM: Main operation (fit) and Q convergence", {
    model <- ClustVarACM$new(data = data_test, K = 3, max_iter = 50)

    # Run model fitting
    fitted_model <- model$fit()

    # The model should return itself (invisibly)
    expect_s3_class(fitted_model, "ClustVarACM")

    # The final Q criterion must be defined and positive
    expect_true(!is.null(model$Q_final))
    expect_gt(model$Q_final, 0)

    # The Q_trace should show convergence (Q should not decrease)
    # The algorithm is expected to be non-decreasing by design
    expect_true(all(diff(model$Q_trace) >= -model$tol * 10))

    # The score matrix should have correct dimensions (p x K)
    expect_equal(dim(model$score_matrix), c(5, 3))

    # Every variable must be assigned to a cluster
    expect_true(all(model$clusters %in% 1:3))
  })

---
  #   c. Cluster quality validation tests

  test_that("ClustVarACM: Cluster quality validation (Q-score)", {
    model <- ClustVarACM$new(data = data_test, K = 2, max_iter = 100)
    model$fit()

    # The final Q should equal the sum of intra-cluster scores
    p <- ncol(data_test)
    calculated_Q <- sum(sapply(1:p, function(j) {
      cluster_k <- model$clusters[j]
      return(model$score_matrix[j, cluster_k])
    }))

    expect_equal(model$Q_final, calculated_Q)

    # Heuristic check of the expected partition:
    # VarA and VarB should be together, VarC and VarD as well.
    cluster_AB <- model$clusters[names(data_test) == "VarA"]
    cluster_CD <- model$clusters[names(data_test) == "VarC"]

    expect_equal(model$clusters[names(data_test) == "VarB"], cluster_AB)
    expect_equal(model$clusters[names(data_test) == "VarD"], cluster_CD)
    # The two main clusters should be different
    expect_true(cluster_AB != cluster_CD)
  })

---
  #   d. Tests for the predict() method

  test_that("ClustVarACM: Prediction on new variables (predict)", {
    model <- ClustVarACM$new(data = data_test, K = 2)
    model$fit()

    # Test predict()
    pred_result <- model$predict(data_illustrative)

    expect_s3_class(pred_result, "data.frame")
    expect_equal(nrow(pred_result), 2)
    expect_named(pred_result, c("Variable", "Cluster_Assigned", "Max_Association_Score"))

    # NewVar2 is a copy of VarA (Cluster 1 in the heuristic example)
    # It should be assigned to one of the main clusters with a high score.
    cluster_AB <- model$clusters[names(data_test) == "VarA"][1]

    # Check NewVar2 assignment matches VarA/VarB cluster
    expect_equal(pred_result$Cluster_Assigned[pred_result$Variable == "NewVar2"], cluster_AB)
    # Check the association score is high (very strong if it's a copy)
    expect_gt(pred_result$Max_Association_Score[pred_result$Variable == "NewVar2"], 0.9)

    # Error test: incompatible number of observations
    data_err_obs <- data_illustrative[1:10, ]
    expect_error(model$predict(data_err_obs), "The number of observations in 'X' must be the same as in the training data.")
  })

---
  #   e. Optimal K selection tests (select_K)

  test_that("ClustVarACM: Optimal K selection (select_K)", {
    # For this test, use a small K grid
    model_k <- ClustVarACM$new(data = data_test, K = 2) # initial K does not matter

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
    model <- ClustVarACM$new(data = data_test, K = 2)
    model$fit()

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

    # Test plot() with K=1 (should fail)
    model_k1 <- ClustVarACM$new(data = data_test[, 1:2], K = 1)
    model_k1$clusters <- 1 # Force assignment to 1
    # Plot requires K >= 2
    expect_error(model_k1$plot(), "Plotting requires at least 2 clusters.")
  })
