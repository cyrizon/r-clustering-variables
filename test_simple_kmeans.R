# Test script for SimpleKMeansVariables implementation

# Load the new implementation
source("R/algorithms/KMeansVariablesR6.R")

# Create sample data
set.seed(123)
data <- data.frame(
    var1 = rnorm(50, mean = 10, sd = 2),
    var2 = rnorm(50, mean = 10, sd = 2), # Similar to var1
    var3 = rnorm(50, mean = 50, sd = 5),
    var4 = rnorm(50, mean = 50, sd = 5), # Similar to var3
    var5 = rnorm(50, mean = 100, sd = 10),
    var6 = rnorm(50, mean = 100, sd = 10) # Similar to var5
)

# Add some correlation
data$var2 <- data$var1 * 0.8 + rnorm(50, 0, 1)
data$var4 <- data$var3 * 0.9 + rnorm(50, 0, 2)
data$var6 <- data$var5 * 0.85 + rnorm(50, 0, 5)

cat("\n=== TEST 1: Basic fit with k=3 ===\n")
model <- KMeansVariablesR6$new(k = 3, method = "correlation", seed = 42)
model$fit(data)
print(model)

cat("\n=== TEST 2: Elbow method (automatic k selection) ===\n")
model2 <- KMeansVariablesR6$new(method = "correlation", seed = 42)
optimal_k <- model2$elbow_method(data, k_max = 5)
cat(sprintf("Optimal k selected: %d\n", optimal_k))
model2$fit(data)
print(model2)

cat("\n=== TEST 3: Get center variables ===\n")
centers <- model2$get_center_variables()
cat("Center variables:\n")
print(centers)

cat("\n=== TEST 4: Prediction on new data ===\n")
# Create new data with similar structure
new_data <- data.frame(
    new_var1 = rnorm(50, mean = 10, sd = 2),
    new_var2 = rnorm(50, mean = 50, sd = 5)
)
new_data$new_var1 <- data$var1 * 0.7 + rnorm(50, 0, 1.5)
new_data$new_var2 <- data$var3 * 0.75 + rnorm(50, 0, 3)

predictions <- model2$predict(new_data)
cat("Predictions for new variables:\n")
print(predictions)

cat("\n=== ALL TESTS COMPLETED ===\n")
