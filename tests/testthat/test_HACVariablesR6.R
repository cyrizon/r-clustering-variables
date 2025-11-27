# ==============================================================================
# Unit tests for the HACVariablesR6 class
# ==============================================================================
library(testthat)
library(R6)

# Load the class under test
source("../../R/algorithms/HACVariablesR6.R")

# ==============================================================================
# 1. TESTS D'INITIALISATION
# ==============================================================================

test_that("HACVariablesR6 initializes correctly with default parameters", {
  model <- HACVariablesR6$new()
  expect_equal(model$k, 2)
  expect_equal(model$method, "correlation")
  expect_equal(model$linkage_method, "ward.D2")
  expect_false(model$fitted)
})

test_that("HACVariablesR6 initializes with custom parameters", {
  model <- HACVariablesR6$new(k = 4, method = "euclidean", linkage_method = "complete")
  expect_equal(model$k, 4)
  expect_equal(model$method, "euclidean")
  expect_equal(model$linkage_method, "complete")
})

# ============================================================================
# 2. DATA VALIDATION TESTS
# ============================================================================

test_that("fit() rejects non-numeric data", {
  model <- HACVariablesR6$new(k = 2)
  data_invalid <- data.frame(x1 = 1:5, x2 = letters[1:5], x3 = 1:5)
  expect_error(model$fit(data_invalid), "All variables must be numeric")
})

test_that("fit() rejects data with missing values", {
  model <- HACVariablesR6$new(k = 2)
  data_with_na <- data.frame(x1 = c(1, 2, NA), x2 = c(4, 5, 6), x3 = c(7, 8, 9))
  expect_error(model$fit(data_with_na), "Data must not contain NA")
})

test_that("fit() warns if k is invalid (k >= n_variables)", {
  # On met 2 variables, mais on demande 2 clusters.
  # La condition est k < ncol(X), donc 2 < 2 est Faux -> Warning
  data <- data.frame(x1 = 1:10, x2 = 11:20)
  model <- HACVariablesR6$new(k = 2)

  expect_warning(model$fit(data), "number of clusters 'k' must be between")
  expect_null(model$clusters)
})

# ============================================================================
# 3. TESTS FOR THE fit() METHOD
# ============================================================================

test_that("fit() works with correlation method", {
  set.seed(42)
  # CORRECTION: On met 4 variables pour k=2 (car 2 < 4)
  data <- data.frame(
    var1 = rnorm(20), var2 = rnorm(20),
    var3 = rnorm(20), var4 = rnorm(20)
  )

  model <- HACVariablesR6$new(k = 2, method = "correlation")
  model$fit(data)

  expect_true(model$fitted)
  expect_equal(length(model$clusters), 2)
  expect_setequal(unlist(model$clusters), colnames(data))
})

test_that("fit() works with euclidean method", {
  set.seed(42)
  # CORRECTION: On met 3 variables pour k=2
  data <- data.frame(
    var1 = rnorm(20), var2 = rnorm(20), var3 = rnorm(20)
  )

  model <- HACVariablesR6$new(k = 2, method = "euclidean")
  model$fit(data)

  expect_true(model$fitted)
  expect_equal(length(model$clusters), 2)
})

test_that("fit() respects the linkage method", {
  set.seed(42)
  # CORRECTION: On utilise un data.frame (plus sûr que matrix pour les noms)
  # et on met assez de variables
  data <- data.frame(
    v1 = rnorm(10), v2 = rnorm(10),
    v3 = rnorm(10), v4 = rnorm(10)
  )

  model <- HACVariablesR6$new(k=2, linkage_method = "single")
  model$fit(data)
  expect_equal(model$model$method, "single")

  model2 <- HACVariablesR6$new(k=2, linkage_method = "complete")
  model2$fit(data)
  expect_equal(model2$model$method, "complete")
})

# ============================================================================
# 4. TESTS FOR THE predict() METHOD
# ============================================================================

test_that("predict() requires a fitted model", {
  model <- HACVariablesR6$new(k = 2)
  new_data <- data.frame(new_var = rnorm(10))
  expect_error(model$predict(new_data), "Model must be fitted")
})

test_that("predict() rejects data with different row count", {
  # CORRECTION: 3 variables pour l'entraînement
  train_data <- data.frame(v1=rnorm(20), v2=rnorm(20), v3=rnorm(20))
  model <- HACVariablesR6$new(k = 2)
  model$fit(train_data)

  new_data <- data.frame(n1 = rnorm(10)) # Pas le même nombre de lignes (10 vs 20)
  expect_error(model$predict(new_data), "must have the same number of observations")
})

test_that("predict() assigns new variables correctly", {
  set.seed(42)
  # Train: 4 variables, 2 clusters clairs
  train_data <- data.frame(
    v1 = rnorm(50, mean = 0), v2 = rnorm(50, mean = 0),
    v3 = rnorm(50, mean = 10), v4 = rnorm(50, mean = 10)
  )

  model <- HACVariablesR6$new(k = 2, method = "correlation")
  model$fit(train_data)

  new_data <- data.frame(
    new_A = rnorm(50, mean = 0),
    new_B = rnorm(50, mean = 10)
  )

  predictions <- model$predict(new_data)

  expect_equal(nrow(predictions), 2)
  expect_true(all(!is.na(predictions$cluster)))
})

# ============================================================================
# 5. TESTS FOR THE plot() METHOD
# ============================================================================

test_that("plot() runs without error on fitted model", {
  # On ignore les warnings graphiques ("horiz" paramètre)
  # On veut juste vérifier que la fonction ne CRASH pas (Error)
  data <- data.frame(v1=rnorm(20), v2=rnorm(20), v3=rnorm(20))
  model <- HACVariablesR6$new(k=2)
  model$fit(data)

  # On s'attend à ce qu'il n'y ait PAS d'erreur
  expect_error(model$plot(), NA)
})

test_that("plot() throws error if not fitted", {
  model <- HACVariablesR6$new()
  expect_error(model$plot(), "Model must be fitted")
})

# ============================================================================
# 6. UTILITY METHOD TESTS
# ============================================================================

test_that("print() and summary() output correctly", {
  # CORRECTION: Assez de variables pour que les clusters existent
  data <- data.frame(v1=rnorm(20), v2=rnorm(20), v3=rnorm(20))
  model <- HACVariablesR6$new(k=2)
  model$fit(data)

  expect_output(model$print(), "HACVariablesR6 Model")
  # Maintenant que les clusters sont formés, "Cluster composition" doit apparaître
  expect_output(model$summary(), "Cluster composition")
})

message("\n✓ HACVariablesR6 unit tests completed successfully!")
