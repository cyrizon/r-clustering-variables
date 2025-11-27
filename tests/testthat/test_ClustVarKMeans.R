# ==============================================================================
# Unit tests for the ClustVarKMeans class
# ==============================================================================
# This file contains a comprehensive suite of tests to validate the behavior
# of the K-means algorithm adapted for variable clustering.
#
# Test organization:
# 1. Initialization tests
# 2. Data validation tests
# 3. Tests for the fit() method
# 4. Tests for the predict() method
# 5. Tests for the elbow_method() function
# 6. Utility method tests (print, summary, get_center_variables)
# ==============================================================================

library(testthat)
library(R6)

# ==============================================================================
# 1. TESTS D'INITIALISATION
# ==============================================================================

test_that("ClustVarKMeans initializes correctly with default parameters", {
    # Test: create an object with default parameters
    model <- ClustVarKMeans$new()

    # Default checks
    expect_equal(model$K, 3)
    expect_equal(model$method, "correlation")
    expect_equal(model$max_iter, 100)
    expect_equal(model$nstart, 10)
    expect_false(model$fitted)
    expect_null(model$seed)
})

test_that("ClustVarKMeans initializes with custom parameters", {
    # Test: creation with specific parameters
    model <- ClustVarKMeans$new(K = 5,
        method = "euclidean",
        max_iter = 50,
        nstart = 20,
        seed = 123
    )

    # Checks
    expect_equal(model$K, 5)
    expect_equal(model$method, "euclidean")
    expect_equal(model$max_iter, 50)
    expect_equal(model$nstart, 20)
    expect_equal(model$seed, 123)
})

test_that("Distance method must be valid", {
    # Test: invalid method should throw an error
    expect_error(
        ClustVarKMeans$new(method = "invalid_method"),
        "'arg' should be one of"
    )
})

# ============================================================================
# 2. DATA VALIDATION TESTS
# ============================================================================

test_that("fit() rejects non-numeric data", {
    # Test: data with non-numeric columns
    model <- ClustVarKMeans$new(K = 2)
    data_invalid <- data.frame(
        x1 = c(1, 2, 3),
        x2 = c("a", "b", "c"), # Non-numeric column
        x3 = c(4, 5, 6)
    )

    expect_error(
        model$fit(data_invalid),
        "All variables must be numeric"
    )
})

test_that("fit() rejects data with missing values", {
    # Test: data containing NA
    model <- ClustVarKMeans$new(K = 2)
    data_with_na <- data.frame(
        x1 = c(1, 2, NA),
        x2 = c(4, 5, 6),
        x3 = c(7, 8, 9)
    )

    expect_error(
        model$fit(data_with_na),
        "Data must not contain missing values"
    )
})

test_that("fit() rejects an invalid k", {
    # Test: K too small (< 2)
    model <- ClustVarKMeans$new(K = 1)
    data <- data.frame(x1 = 1:10, x2 = 11:20, x3 = 21:30)

    expect_error(
        model$fit(data),
        "K must be between 2 and the number of variables"
    )

    # Test : K trop grand (> nombre de variables)
    model <- ClustVarKMeans$new(K = 10)
    expect_error(
        model$fit(data),
        "K must be between 2 and the number of variables"
    )
})

test_that("fit() accepts a matrix input", {
    # Test: matrices should be accepted like data.frames
    model <- ClustVarKMeans$new(K = 2, seed = 42)
    data_matrix <- matrix(rnorm(30), nrow = 10, ncol = 3)

    # fit() produces a success message
    expect_message(model$fit(data_matrix), "Best of")
    expect_true(model$fitted)
})

# ============================================================================
# 3. TESTS FOR THE fit() METHOD
# ============================================================================

test_that("fit() creates clusters using the correlation method", {
    # Test: clustering based on correlation
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50)
    )

    model <- ClustVarKMeans$new(K = 2, method = "correlation", seed = 42)
    model$fit(data)

    # Checks
    expect_true(model$fitted)
    expect_equal(length(model$clusters), 2) # 2 clusters
    expect_equal(length(model$centers), 2) # 2 centres
    expect_true(is.numeric(model$inertia))
    expect_true(model$inertia >= 0)

    # Verify that all variables are assigned
    all_vars <- unlist(model$clusters)
    expect_equal(length(all_vars), 4)
    expect_setequal(all_vars, colnames(data))
})

test_that("fit() creates clusters using the euclidean method", {
    # Test: clustering based on Euclidean distance
    set.seed(123)
    data <- data.frame(
        var1 = rnorm(50, mean = 0),
        var2 = rnorm(50, mean = 0),
        var3 = rnorm(50, mean = 5),
        var4 = rnorm(50, mean = 5)
    )

    model <- ClustVarKMeans$new(K = 2, method = "euclidean", seed = 123)
    model$fit(data)

    # Checks
    expect_true(model$fitted)
    expect_equal(length(model$clusters), 2)
    expect_true(model$inertia >= 0)
})

test_that("fit() generates default variable names if absent", {
    # Test: data without column names
    model <- ClustVarKMeans$new(K = 2, seed = 42)
    data_matrix <- matrix(rnorm(40), nrow = 10, ncol = 4)
    # La matrice n'a pas de colnames initialement

    model$fit(data_matrix)

    # Verify that default names were generated (V1, V2, V3, V4)
    all_vars <- unlist(model$clusters)
    expect_true(all(grepl("^V[0-9]+$", all_vars)))
    expect_equal(length(all_vars), 4)
})

test_that("fit() with multiple starts (nstart) improves the solution", {
    # Test: nstart > 1 should potentially improve inertia
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50),
        var5 = rnorm(50)
    )

    # Model with 1 random start
    model1 <- ClustVarKMeans$new(K = 3, nstart = 1, seed = 42)
    model1$fit(data)

    # Model with 20 random starts
    model2 <- ClustVarKMeans$new(K = 3, nstart = 20, seed = 42)
    model2$fit(data)

    # Inertia with more starts should be <= (better or equal)
    expect_lte(model2$inertia, model1$inertia)
})

test_that("fit() stores normalization parameters correctly", {
    # Test: scaling parameters should be stored
    data <- data.frame(
        var1 = c(1, 2, 3, 4, 5),
        var2 = c(10, 20, 30, 40, 50),
        var3 = c(100, 200, 300, 400, 500)
    )

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(data)

    # Verify that scale_center and scale_scale are stored
    expect_equal(length(model$scale_center), 3)
    expect_equal(length(model$scale_scale), 3)
    expect_true(all(!is.na(model$scale_center)))
    expect_true(all(!is.na(model$scale_scale)))
})

# ============================================================================
# 4. TESTS FOR THE predict() METHOD
# ============================================================================

test_that("predict() requires a fitted model", {
    # Test: predict() without prior fit() should fail
    model <- ClustVarKMeans$new(K = 2)
    new_data <- data.frame(new_var = rnorm(50))

    expect_error(
        model$predict(new_data),
        "Model must be fitted"
    )
})

test_that("predict() rejects data with a different number of observations", {
    # Test: new data must have the same number of rows
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50), # 50 observations
        var2 = rnorm(50)
    )

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(train_data)

    # Data with 30 observations (different from 50)
    new_data <- data.frame(new_var = rnorm(30))

    expect_error(
        model$predict(new_data),
        "must have the same number of observations"
    )
})

test_that("predict() assigns new variables correctly (scaling='self')", {
    # Test: prediction with independent normalization
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50, mean = 0, sd = 1),
        var2 = rnorm(50, mean = 0, sd = 1),
        var3 = rnorm(50, mean = 5, sd = 2)
    )

    model <- ClustVarKMeans$new(K = 2, method = "correlation", seed = 42)
    model$fit(train_data)

    # New variables to predict
    new_data <- data.frame(
        new_var1 = rnorm(50, mean = 0, sd = 1),
        new_var2 = rnorm(50, mean = 5, sd = 2)
    )

    predictions <- model$predict(new_data, scaling = "self")

    # Checks
    expect_s3_class(predictions, "data.frame")
    expect_equal(nrow(predictions), 2) # 2 nouvelles variables
    expect_equal(ncol(predictions), 3) # variable, cluster, distance
    expect_true(all(predictions$cluster %in% 1:2))
    expect_true(all(!is.na(predictions$cluster)))
    expect_true(all(!is.na(predictions$distance)))
})

test_that("predict() works with scaling='training'", {
    # Test: prediction using training normalization parameters
    # Note: scaling='training' requires new data to have the same
    # number of variables as the training data
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50, mean = 10, sd = 2),
        var2 = rnorm(50, mean = 10, sd = 2),
        var3 = rnorm(50, mean = 5, sd = 1)
    )

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(train_data)

    # The new data must have the same number of columns (3)
    new_data <- data.frame(
        new_var1 = rnorm(50, mean = 10, sd = 2),
        new_var2 = rnorm(50, mean = 10, sd = 2),
        new_var3 = rnorm(50, mean = 5, sd = 1)
    )

    predictions <- model$predict(new_data, scaling = "training")

    expect_true(all(!is.na(predictions$cluster)))
    expect_equal(nrow(predictions), 3)
})

test_that("predict() works with scaling='none'", {
    # Test: prediction without normalization
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50)
    )

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(train_data)

    new_data <- data.frame(new_var = rnorm(50))

    predictions <- model$predict(new_data, scaling = "none")

    expect_true(all(!is.na(predictions$cluster)))
})

test_that("predict() generates default variable names if absent", {
    # Test: prediction with data without column names
    set.seed(42)
    train_data <- data.frame(var1 = rnorm(50), var2 = rnorm(50))

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(train_data)

    new_data_matrix <- matrix(rnorm(50), ncol = 1)
    # Pas de colnames

    predictions <- model$predict(new_data_matrix)

    expect_true(grepl("^V[0-9]+$", predictions$variable[1]))
})

# ============================================================================
# 5. TESTS FOR THE ELBOW_METHOD()
# ============================================================================

test_that("elbow_method() computes inertia for different k values", {
    # Test: the elbow method should return an optimal k
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50),
        var5 = rnorm(50),
        var6 = rnorm(50)
    )

    model <- ClustVarKMeans$new(seed = 42)

    # Run elbow_method without plotting
    K_opt <- model$elbow_method(data, K_min = 2, K_max = 5, plot = FALSE)

    # Checks
    expect_true(is.numeric(K_opt))
    expect_true(K_opt >= 2 && K_opt <= 5)
})

test_that("elbow_method() rejects invalid data", {
    # Test: data with NA should be rejected
    model <- ClustVarKMeans$new()
    data_with_na <- data.frame(
        var1 = c(1, 2, NA),
        var2 = c(4, 5, 6),
        var3 = c(7, 8, 9)
    )

    expect_error(
        model$elbow_method(data_with_na, plot = FALSE),
        "Data must not contain missing values"
    )
})

test_that("elbow_method() adjusts K_max if too large", {
    # Test: K_max > number of variables - 1 should be adjusted automatically
    set.seed(42)
    # Use 8 variables to provide a sufficient range (k from 2 to 7)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50),
        var5 = rnorm(50),
        var6 = rnorm(50),
        var7 = rnorm(50),
        var8 = rnorm(50)
    )

    model <- ClustVarKMeans$new(seed = 42)

    # K_max = 20 mais seulement 8 variables (max possible = 7)
    # The method should adjust K_max to 7 automatically
    K_opt <- model$elbow_method(data, K_min = 2, K_max = 20, plot = FALSE)

    # The optimal K must be valid (between 2 and 7)
    expect_true(is.numeric(K_opt))
    expect_equal(length(K_opt), 1) # Doit retourner une seule valeur
    expect_true(K_opt >= 2 && K_opt <= 7) # Maximum possible avec 8 variables
})

test_that("elbow_method() works with different distance methods", {
    # Test: elbow_method with euclidean method
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50, mean = 0),
        var2 = rnorm(50, mean = 0),
        var3 = rnorm(50, mean = 5),
        var4 = rnorm(50, mean = 5),
        var5 = rnorm(50, mean = 10)
    )

    model <- ClustVarKMeans$new(method = "euclidean", seed = 42)
    K_opt <- model$elbow_method(data, K_min = 2, K_max = 4, plot = FALSE)

    expect_true(K_opt >= 2 && K_opt <= 4)
})

# ============================================================================
# 6. UTILITY METHOD TESTS
# ============================================================================

test_that("get_center_variables() returns the center variable names", {
    # Test: retrieval of center variables (medoids)
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50)
    )

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(data)

    centers <- model$get_center_variables()

    # Checks
    expect_equal(length(centers), 2) # 2 clusters = 2 centres
    expect_true(all(centers %in% colnames(data)))
})

test_that("get_center_variables() fails if the model is not fitted", {
    # Test: calling before fit() should fail
    model <- ClustVarKMeans$new(K = 2)

    expect_error(
        model$get_center_variables(),
        "Model not fitted"
    )
})

test_that("print() displays model information", {
    # Test: print method should not throw an error
    set.seed(42)
    data <- data.frame(var1 = rnorm(50), var2 = rnorm(50))

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(data)

    expect_output(model$print(), "ClustVarKMeans")
    expect_output(model$print(), "fitted: TRUE")
})

test_that("summary() displays a detailed summary", {
    # Test: summary method shows comprehensive information
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50)
    )

    model <- ClustVarKMeans$new(K = 2, seed = 42)
    model$fit(data)

    expect_output(model$summary(), "ClustVarKMeans Summary")
    expect_output(model$summary(), "Cluster sizes")
    expect_output(model$summary(), "Total inertia")
})

# ============================================================================
# 7. INTEGRATION TESTS AND EDGE CASES
# ============================================================================

test_that("Full workflow: fit() then predict()", {
    # Integration test: complete usage scenario
    set.seed(42)

    # 1. Create training data
    train_data <- data.frame(
        height = rnorm(100, mean = 170, sd = 10),
        weight = rnorm(100, mean = 70, sd = 15),
        age = rnorm(100, mean = 30, sd = 5),
        salary = rnorm(100, mean = 50000, sd = 10000)
    )

    # 2. Initialize and fit the model
    model <- ClustVarKMeans$new(K = 2, method = "correlation", seed = 42)
    model$fit(train_data)

    # 3. Check the fit
    expect_true(model$fitted)
    expect_equal(length(model$clusters), 2)

    # 4. Predict on new variables
    new_data <- data.frame(
        shoe_size = rnorm(100, mean = 42, sd = 2),
        income = rnorm(100, mean = 55000, sd = 12000)
    )

    predictions <- model$predict(new_data)

    # 5. Verify predictions
    expect_equal(nrow(predictions), 2)
    expect_true(all(predictions$cluster %in% 1:2))

    # 6. Obtain a summary
    expect_output(model$summary(), "ClustVarKMeans Summary")
})

test_that("Model is reproducible with a fixed seed", {
    # Test: same seed = same results
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50)
    )

    # First fit
    model1 <- ClustVarKMeans$new(K = 2, seed = 999)
    model1$fit(data)
    inertia1 <- model1$inertia

    # Second fit with the same seed
    model2 <- ClustVarKMeans$new(K = 2, seed = 999)
    model2$fit(data)
    inertia2 <- model2$inertia

    # The inertias should be identical
    expect_equal(inertia1, inertia2)
})

test_that("Model handles a large number of variables correctly", {
    # Test: performance with many variables
    set.seed(42)
    n_vars <- 20
    data <- as.data.frame(matrix(rnorm(50 * n_vars), nrow = 50, ncol = n_vars))
    colnames(data) <- paste0("var", 1:n_vars)

    model <- ClustVarKMeans$new(K = 5, seed = 42, nstart = 5)

    # fit() produces a success message, so use expect_message
    expect_message(model$fit(data), "Best of")
    expect_true(model$fitted)
    expect_equal(length(unlist(model$clusters)), n_vars)
})

# ============================================================================
# END OF TESTS MESSAGE
# ============================================================================

message("\n✓ ClustVarKMeans unit tests completed successfully!")
