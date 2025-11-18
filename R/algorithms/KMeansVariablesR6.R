#' Simple K-Means Clustering for Variables (Educational Implementation)
#'
#' @description
#' Version simplifiée et pédagogique du K-Means pour clustering de variables.
#' Illustre les concepts de base avec médoïdes et méthode du coude intégrée.
#' Variables are grouped based on their similarity (correlation or euclidean distance).
#'
#' @export
KMeansVariablesR6 <- R6::R6Class(
  "KMeansVariablesR6",
  public = list(
    #' @field k Number of clusters
    k = NULL,
    #' @field method Distance method ("correlation" or "euclidean")
    method = NULL,
    #' @field max_iter Maximum iterations for convergence
    max_iter = 100,
    #' @field centers Cluster centers (indices of medoid variables)
    centers = NULL,
    #' @field clusters List of variable names per cluster
    clusters = NULL,
    #' @field inertia Total within-cluster sum of squares
    inertia = NULL,
    #' @field fitted Logical indicating if model has been fitted
    fitted = FALSE,
    #' @field data_fit Normalized data used for fitting
    data_fit = NULL,
    #' @field scale_center Centering parameters from scaling
    scale_center = NULL,
    #' @field scale_scale Scaling parameters from scaling
    scale_scale = NULL,
    #' @field seed Optional seed for reproducibility
    seed = NULL,
    #' @field dist_strategy Kept for compatibility (ignored in simple version)
    dist_strategy = NULL,
    #' @field nstart Kept for compatibility (ignored in simple version)
    nstart = 25,

    #' @description
    #' Create a new KMeansVariablesR6 object
    #' @param k Number of clusters (default: 3)
    #' @param method Distance method: "correlation" or "euclidean" (default: "correlation")
    #' @param max_iter Maximum iterations for convergence (default: 100)
    #' @param seed Optional seed for reproducibility
    #' @return A new `KMeansVariablesR6` object
    initialize = function(k = 3,
                          method = c("correlation", "euclidean"),
                          max_iter = 100,
                          dist_strategy = NULL,
                          nstart = 25,
                          seed = NULL) {
      self$k <- k
      self$method <- match.arg(method)
      self$max_iter <- max_iter
      self$seed <- if (!is.null(seed)) as.integer(seed) else NULL
      # Keep for compatibility but not used in simple version
      self$dist_strategy <- if (!is.null(dist_strategy)) dist_strategy else "medoid"
      self$nstart <- nstart
    },

    #' @description
    #' Fit the K-means model on variables
    #' @param X A data.frame or matrix with numeric variables to cluster
    #' @return Self (invisibly) for method chaining
    fit = function(X) {
      # === 1. VALIDATION ===
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("X must be a data.frame or matrix.")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("All variables must be numeric.")
      }
      if (anyNA(X)) {
        stop("Data must not contain missing values (NA).")
      }
      if (self$k < 2 || self$k > ncol(X)) {
        stop("k must be between 2 and the number of variables.")
      }

      # Ensure variables have names; generate defaults if missing
      if (is.null(colnames(X))) {
        colnames(X) <- paste0("V", seq_len(ncol(X)))
      }

      # === 2. PRÉPARATION DES DONNÉES ===
      # Normalize data (center and scale) so all variables are comparable
      X_norm <- scale(X)
      self$data_fit <- X_norm
      # Store normalization parameters for predict()
      self$scale_center <- attr(X_norm, "scaled:center")
      self$scale_scale <- attr(X_norm, "scaled:scale")

      n_obs <- nrow(X_norm)
      n_vars <- ncol(X_norm)
      var_names <- colnames(X_norm)

      # === 3. CALCUL DE LA MATRICE DE DISTANCE ENTRE VARIABLES ===
      # Key: we want to group VARIABLES (columns), not observations (rows)
      if (self$method == "correlation") {
        # Distance based on correlation: D(i,j) = 1 - |cor(var_i, var_j)|
        # More correlated = closer
        cor_matrix <- cor(X_norm)
        dist_matrix <- 1 - abs(cor_matrix) # Distance in [0, 2]
      } else {
        # Euclidean distance between variable vectors (transpose to have variables as rows)
        dist_matrix <- as.matrix(dist(t(X_norm)))
      }

      # === 4. INITIALISATION DES CENTRES (k random variables) ===
      # Choose k random variables as initial centers
      if (!is.null(self$seed)) set.seed(self$seed)
      initial_centers_idx <- sample(seq_len(n_vars), self$k, replace = FALSE)
      current_centers_idx <- initial_centers_idx

      # === 5. ALGORITHME K-MEANS (LLOYD) ===
      converged <- FALSE
      iter <- 0
      cluster_assignment <- rep(0, n_vars)

      while (!converged && iter < self$max_iter) {
        iter <- iter + 1
        old_assignment <- cluster_assignment

        # --- STEP A: ASSIGNMENT ---
        # Assign each variable to the nearest center
        for (i in seq_len(n_vars)) {
          # Calculate distance from variable i to each center
          distances_to_centers <- sapply(seq_len(self$k), function(c) {
            center_idx <- current_centers_idx[c]
            dist_matrix[i, center_idx]
          })
          # Assign to cluster of nearest center
          cluster_assignment[i] <- which.min(distances_to_centers)
        }

        # --- STEP B: UPDATE CENTERS ---
        # Recalculate center of each cluster (medoid = most central variable)
        new_centers_idx <- sapply(seq_len(self$k), function(c) {
          # Variables belonging to cluster c
          vars_in_cluster <- which(cluster_assignment == c)

          if (length(vars_in_cluster) == 0) {
            # Empty cluster: keep old center or reinitialize randomly
            warning(paste("Cluster", c, "is empty at iteration", iter))
            return(current_centers_idx[c])
          }

          if (length(vars_in_cluster) == 1) {
            # Single member: it becomes the center
            return(vars_in_cluster[1])
          }

          # Calculate the "medoid" variable: one that minimizes average distance to other members
          avg_distances <- sapply(vars_in_cluster, function(candidate) {
            mean(dist_matrix[candidate, vars_in_cluster])
          })
          vars_in_cluster[which.min(avg_distances)]
        })

        # --- CHECK CONVERGENCE ---
        # If assignments don't change, we've converged
        if (all(cluster_assignment == old_assignment)) {
          converged <- TRUE
        }

        current_centers_idx <- new_centers_idx
      }

      # === 6. STORE RESULTS ===
      names(cluster_assignment) <- var_names
      self$clusters <- split(names(cluster_assignment), cluster_assignment)
      self$centers <- current_centers_idx # Indices of center variables

      # Calculate total inertia (sum of squared distances within clusters)
      self$inertia <- sum(sapply(seq_len(self$k), function(c) {
        vars_idx <- which(cluster_assignment == c)
        if (length(vars_idx) <= 1) {
          return(0)
        }
        center_idx <- current_centers_idx[c]
        sum(dist_matrix[vars_idx, center_idx]^2)
      }))

      self$fitted <- TRUE
      message(sprintf("\u2713 Converged in %d iterations | Inertia: %.3f", iter, self$inertia))
      invisible(self)
    },

    #' @description
    #' Predict cluster membership for new variables
    #' @param X A data.frame or matrix with numeric variables to classify
    #' @return A data.frame with variable names, assigned clusters, and distances
    predict = function(X, scaling = c("self", "training", "none")) {
      scaling <- match.arg(scaling)
      if (!self$fitted) {
        stop("Model must be fitted with $fit() before prediction.")
      }
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("X must be a data.frame or matrix.")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("All variables must be numeric.")
      }
      if (anyNA(X)) {
        stop("Data must not contain missing values (NA).")
      }

      # Check that X has the same number of observations as training data
      if (nrow(X) != nrow(self$data_fit)) {
        stop(
          "X must have the same number of observations (rows) as training data. Expected: ",
          nrow(self$data_fit), ", got: ", nrow(X)
        )
      }

      # Ensure variable names exist for predictions
      if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(ncol(X)))

      # Normalize X according to user choice
      if (scaling == "self") {
        X_norm <- scale(X)
      } else if (scaling == "training") {
        if (is.null(self$scale_center) || is.null(self$scale_scale)) stop("No training scaling parameters available. Fit the model first.")
        X_norm <- scale(X, center = self$scale_center, scale = self$scale_scale)
      } else {
        X_norm <- as.matrix(X)
      }
      var_names <- colnames(X_norm)

      # Prepare output vectors
      clusters_pred <- rep(NA, length(var_names))
      distances_pred <- rep(NA, length(var_names))

      if (self$method == "correlation") {
        # Calculate correlation-based distances
        for (i in seq_along(var_names)) {
          var_vector <- X_norm[, i]
          min_dist <- Inf
          best_cluster <- NA

          for (k in 1:self$k) {
            # Get variables in cluster k
            cluster_vars <- unlist(self$clusters[[k]])
            if (length(cluster_vars) > 0) {
              # Calculate mean correlation distance to cluster
              correlations <- sapply(cluster_vars, function(v) {
                if (v %in% colnames(self$data_fit)) {
                  abs(cor(var_vector, self$data_fit[, v]))
                } else {
                  NA
                }
              })
              if (all(is.na(correlations))) {
                dist <- NA
              } else {
                dist <- 1 - mean(correlations, na.rm = TRUE)
              }
              if (!is.na(dist) && dist < min_dist) {
                min_dist <- dist
                best_cluster <- k
              }
            }
          }
          clusters_pred[i] <- best_cluster
          distances_pred[i] <- min_dist
        }
      } else if (self$method == "euclidean") {
        # For euclidean: calculate Euclidean distance between standardized variable vectors
        for (i in seq_along(var_names)) {
          var_vector <- X_norm[, i]
          min_dist <- Inf
          best_cluster <- NA

          for (k in 1:self$k) {
            cluster_vars <- unlist(self$clusters[[k]])
            if (length(cluster_vars) > 0) {
              # Calculate mean distance to cluster variables
              distances_to_cluster <- sapply(cluster_vars, function(v) {
                if (v %in% colnames(self$data_fit)) {
                  # Euclidean distance between standardized variable vectors
                  sqrt(mean((var_vector - self$data_fit[, v])^2))
                } else {
                  NA
                }
              })
              if (all(is.na(distances_to_cluster))) {
                dist <- NA
              } else {
                dist <- mean(distances_to_cluster, na.rm = TRUE)
              }
              if (!is.na(dist) && dist < min_dist) {
                min_dist <- dist
                best_cluster <- k
              }
            }
          }
          clusters_pred[i] <- best_cluster
          distances_pred[i] <- min_dist
        }
      }

      df <- data.frame(
        variable = var_names,
        cluster = clusters_pred,
        distance = distances_pred,
        stringsAsFactors = FALSE
      )
      return(df)
    },

    #' @description
    #' Print brief model information
    print = function() {
      cat("KMeansVariablesR6 model\n")
      cat("k:", self$k, "| method:", self$method, "| fitted:", self$fitted, "\n")
      if (self$fitted) {
        cat("Variables per cluster:\n")
        print(self$clusters)
      }
    },

    #' @description
    #' Print detailed model summary
    summary = function() {
      cat("=== KMeansVariablesR6 Summary ===\n")
      cat("Number of clusters:", self$k, "\n")
      cat("Distance method:", self$method, "\n")
      
      if (self$fitted) {
        cat("\nCluster sizes:\n")
        print(sapply(self$clusters, length))
        
        cat("\nVariables per cluster:\n")
        print(self$clusters)
        
        cat("\nCenter variables (medoids):\n")
        center_vars <- self$get_center_variables()
        print(data.frame(
          cluster = seq_along(center_vars),
          center_variable = center_vars,
          stringsAsFactors = FALSE
        ))
        
        cat("\nTotal inertia:", round(self$inertia, 3), "\n")
      } else {
        cat("Model not fitted yet.\n")
      }
    },

    #' @description
    #' Get the center variable (medoid) for each cluster
    #' @return Character vector of center variable names
    get_center_variables = function() {
      if (!self$fitted) stop("Model not fitted")
      var_names <- colnames(self$data_fit)
      return(var_names[self$centers])
    },

    #' @description
    #' Elbow method: automatically determine optimal k using curvature detection
    #' @param X A data.frame or matrix with numeric variables
    #' @param k_max Maximum number of clusters to test (default: 10)
    #' @param plot Whether to plot the elbow curve (default: TRUE)
    #' @return Optimal number of clusters
    elbow_method = function(X, k_max = 10, plot = TRUE) {
      # === 1. VALIDATION ===
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("X must be a data.frame or matrix.")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("All variables must be numeric.")
      }
      if (anyNA(X)) {
        stop("Data must not contain missing values (NA).")
      }
      
      n_vars <- ncol(X)
      k_max <- min(k_max, n_vars - 1)
      if (k_max < 2) stop("Not enough variables to run elbow method.")
      
      # === 2. NORMALIZE DATA ===
      X_norm <- scale(X)
      
      # === 3. CALCULATE DISTANCE MATRIX ===
      if (self$method == "correlation") {
        cor_matrix <- cor(X_norm)
        dist_matrix <- 1 - abs(cor_matrix)
      } else {
        dist_matrix <- as.matrix(dist(t(X_norm)))
      }
      
      # === 4. COMPUTE INERTIA FOR EACH k ===
      inertias <- numeric(k_max)
      
      for (k in 1:k_max) {
        # Run simple k-means for this k
        if (!is.null(self$seed)) set.seed(self$seed)
        centers_idx <- sample(seq_len(n_vars), k, replace = FALSE)
        
        # Assignment step (simplified, single iteration for speed)
        cluster_assignment <- sapply(seq_len(n_vars), function(i) {
          distances <- sapply(centers_idx, function(c) dist_matrix[i, c])
          which.min(distances)
        })
        
        # Calculate inertia
        inertias[k] <- sum(sapply(1:k, function(c) {
          vars_idx <- which(cluster_assignment == c)
          if (length(vars_idx) <= 1) return(0)
          center_idx <- centers_idx[c]
          sum(dist_matrix[vars_idx, center_idx]^2)
        }))
      }
      
      # === 5. DETECT ELBOW VIA CURVATURE ===
      # Normalize inertias to [0, 1]
      inertias_norm <- (inertias - min(inertias)) / (max(inertias) - min(inertias))
      
      # Calculate curvature (second derivative approximation)
      curvature <- numeric(k_max - 2)
      for (i in 2:(k_max - 1)) {
        curvature[i - 1] <- inertias_norm[i - 1] - 2 * inertias_norm[i] + inertias_norm[i + 1]
      }
      
      # Optimal k is where curvature is maximum (sharpest bend)
      k_optimal <- which.max(curvature) + 1
      
      # === 6. VISUALIZATION ===
      if (plot) {
        plot(1:k_max, inertias,
             type = "b", pch = 19, col = "steelblue",
             xlab = "Number of clusters (k)",
             ylab = "Inertia (within-cluster sum of squares)",
             main = "Elbow Method for Optimal k Selection")
        abline(v = k_optimal, col = "red", lty = 2, lwd = 2)
        text(k_optimal, max(inertias) * 0.9,
             labels = paste("Optimal k =", k_optimal),
             pos = 4, col = "red", font = 2)
      }
      
      message(sprintf("✓ Elbow method selected k = %d", k_optimal))
      return(k_optimal)
    },

    #' @description
    #' Automatically suggest optimal k using elbow method (wrapper for compatibility)
    #' @param X A data.frame or matrix with numeric variables
    #' @param max_k Maximum number of clusters to test (default: 10)
    #' @param method Method (kept for compatibility, always uses elbow)
    #' @return Optimal number of clusters
    suggest_k_automatic = function(X, max_k = 10, method = "elbow") {
      return(self$elbow_method(X, k_max = max_k, plot = FALSE))
    }
  )
)
