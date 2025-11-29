#' Simple K-Means Clustering for Variables (Educational Implementation)
#'
#' @description
#' A simplified, educational version of K-Means for variable clustering.
#' Illustrates basic concepts with medoids and an integrated elbow method.
#' Variables are grouped based on their similarity (correlation or euclidean distance).
#'
#' @export
ClustVarKMeans <- R6::R6Class(
  "ClustVarKMeans",
  public = list(
    #' @field K Number of clusters
    K = NULL,
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
    #' @field data Normalized data used for fitting
    data = NULL,
    #' @field scale_center Centering parameters from scaling
    scale_center = NULL,
    #' @field scale_scale Scaling parameters from scaling
    scale_scale = NULL,
    #' @field seed Optional seed for reproducibility
    seed = NULL,
    #' @field nstart Number of random starts (default: 10)
    nstart = 10,

    #' @description
    #' Create a new ClustVarKMeans object
    #' @param K Number of clusters (default: 3)
    #' @param method Distance method: "correlation" or "euclidean" (default: "correlation")
    #' @param max_iter Maximum iterations for convergence (default: 100)
    #' @param nstart Number of random starts used to initialize centroids (default: 10)
    #' @param seed Optional seed for reproducibility
    #' @return A new `ClustVarKMeans` object
    initialize = function(K = 3,
                          method = c("correlation", "euclidean"),
                          max_iter = 100,
                          nstart = 10,
                          seed = NULL) {
      self$K <- K
      self$method <- match.arg(method)
      self$max_iter <- max_iter
      self$nstart <- nstart
      self$seed <- if (!is.null(seed)) as.integer(seed) else NULL
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
      if (self$K < 2 || self$K > ncol(X)) {
        stop("K must be between 2 and the number of variables.")
      }

      # Ensure variables have names; generate defaults if missing
      if (is.null(colnames(X))) {
        colnames(X) <- paste0("V", seq_len(ncol(X)))
      }

      # === 2. DATA PREPARATION ===
      # Normalize data (center and scale) so all variables are comparable
      X_norm <- scale(X)
      self$data <- X_norm
      # Store normalization parameters for predict()
      self$scale_center <- attr(X_norm, "scaled:center")
      self$scale_scale <- attr(X_norm, "scaled:scale")

      n_obs <- nrow(X_norm)
      n_vars <- ncol(X_norm)
      var_names <- colnames(X_norm)

      # === 3. CALCULATION OF THE DISTANCE MATRIX BETWEEN VARIABLES ===
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

      # === 4. MULTI-STARTS FOR STABILITY ===
      best_inertia <- Inf
      best_centers <- NULL
      best_clusters <- NULL

      for (start_run in 1:self$nstart) {
        # Different seed for each start
        if (!is.null(self$seed)) set.seed(self$seed + start_run * 1000)

        # First center: random
        initial_centers_idx <- numeric(self$K)
        initial_centers_idx[1] <- sample(n_vars, 1)

        # Subsequent centers: probability proportional to D^2
        for (i in 2:self$K) {
          # Minimum distance of each variable to already chosen centers
          min_dists <- apply(dist_matrix[, initial_centers_idx[1:(i - 1)], drop = FALSE], 1, min)

          # Probabilities proportional to D^2
          probs <- min_dists^2
          probs[initial_centers_idx[1:(i - 1)]] <- 0 # Avoid re-selecting a center

          # Handle edge case where all probs are 0
          if (sum(probs) == 0) {
            remaining <- setdiff(1:n_vars, initial_centers_idx[1:(i - 1)])
            initial_centers_idx[i] <- sample(remaining, 1)
          } else {
            probs <- probs / sum(probs)
            initial_centers_idx[i] <- sample(n_vars, 1, prob = probs)
          }
        }

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
            distances_to_centers <- sapply(seq_len(self$K), function(c) {
              center_idx <- current_centers_idx[c]
              dist_matrix[i, center_idx]
            })
            # Assign to cluster of nearest center
            cluster_assignment[i] <- which.min(distances_to_centers)
          }

          # --- STEP B: UPDATE CENTERS ---
          # Recalculate center of each cluster (medoid = most central variable)
          new_centers_idx <- sapply(seq_len(self$K), function(c) {
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

        # Calculate inertia for this run
        run_inertia <- sum(sapply(seq_len(self$K), function(c) {
          vars_idx <- which(cluster_assignment == c)
          if (length(vars_idx) <= 1) {
            return(0)
          }
          center_idx <- current_centers_idx[c]
          sum(dist_matrix[vars_idx, center_idx]^2)
        }))

        # Keep best result
        if (run_inertia < best_inertia) {
          best_inertia <- run_inertia
          best_centers <- current_centers_idx
          names(cluster_assignment) <- var_names
          best_clusters <- split(names(cluster_assignment), cluster_assignment)
        }
      } # End multi-starts loop

      # === 6. STORE BEST RESULTS ===
      self$clusters <- best_clusters
      self$centers <- best_centers
      self$inertia <- best_inertia

      self$fitted <- TRUE
      message(sprintf("\u2713 Best of %d starts | Inertia: %.3f", self$nstart, self$inertia))
      invisible(self)
    },

    #' @description
    #' Predict cluster membership for new variables
    #' @param newdata A data.frame or matrix with numeric variables to classify (observations as rows)
    #' @param scaling How to scale `newdata`: one of `"self"` (scale newdata independently), `"training"` (use training scaling), or `"none"` (no scaling)
    #' @return A data.frame with variable names, assigned clusters, and distances
    predict = function(newdata, scaling = c("self", "training", "none")) {
      scaling <- match.arg(scaling)
      if (!self$fitted) {
        stop("Model must be fitted with $fit() before prediction.")
      }
      if (!is.data.frame(newdata) && !is.matrix(newdata)) {
        stop("newdata must be a data.frame or matrix.")
      }
      if (!all(sapply(newdata, is.numeric))) {
        stop("All variables must be numeric.")
      }
      if (anyNA(newdata)) {
        stop("Data must not contain missing values (NA).")
      }

      # Check that newdata has the same number of observations as training data
      if (nrow(newdata) != nrow(self$data)) {
        stop(
          "newdata must have the same number of observations (rows) as training data. Expected: ",
          nrow(self$data), ", got: ", nrow(newdata)
        )
      }

      # Ensure variable names exist for predictions
      if (is.null(colnames(newdata))) colnames(newdata) <- paste0("V", seq_len(ncol(newdata)))

      # Normalize newdata according to user choice
      if (scaling == "self") {
        X_norm <- scale(newdata)
      } else if (scaling == "training") {
        if (is.null(self$scale_center) || is.null(self$scale_scale)) stop("No training scaling parameters available. Fit the model first.")
        X_norm <- scale(newdata, center = self$scale_center, scale = self$scale_scale)
      } else {
        X_norm <- as.matrix(newdata)
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

          for (k in 1:self$K) {
            # Get variables in cluster k
            cluster_vars <- unlist(self$clusters[[k]])
            if (length(cluster_vars) > 0) {
              # Calculate mean correlation distance to cluster
              correlations <- sapply(cluster_vars, function(v) {
                if (v %in% colnames(self$data)) {
                  abs(cor(var_vector, self$data[, v]))
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

          for (k in 1:self$K) {
            cluster_vars <- unlist(self$clusters[[k]])
            if (length(cluster_vars) > 0) {
              # Calculate mean distance to cluster variables
              distances_to_cluster <- sapply(cluster_vars, function(v) {
                if (v %in% colnames(self$data)) {
                  # Euclidean distance between standardized variable vectors
                  sqrt(mean((var_vector - self$data[, v])^2))
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
      cat("ClustVarKMeans model\n")
      cat("K:", self$K, "| method:", self$method, "| fitted:", self$fitted, "\n")
      if (self$fitted) {
        cat("Variables per cluster:\n")
        print(self$clusters)
      }
    },

    #' @description
    #' Print detailed model summary
    summary = function() {
      cat("=== ClustVarKMeans Summary ===\n")
      cat("Number of clusters:", self$K, "\n")
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
      var_names <- colnames(self$data)
      return(var_names[self$centers])
    },

    #' @description
    #' Elbow method: automatically determine optimal K using distance-to-line method
    #' @param X A data.frame or matrix with numeric variables
    #' @param K_min Minimum number of clusters to test (default: 2)
    #' @param K_max Maximum number of clusters to test (default: 10)
    #' @param plot Whether to plot the elbow curve (default: TRUE)
    #' @return Optimal number of clusters
    elbow_method = function(X, K_min = 2, K_max = 10, plot = TRUE) {
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
      K_max <- min(K_max, n_vars - 1)
      K_min <- max(K_min, 2)
      if (K_max < K_min) stop("Not enough variables to run elbow method.")

      # === 2. NORMALIZE DATA ===
      X_norm <- scale(X)

      # === 3. CALCULATE DISTANCE MATRIX ===
      if (self$method == "correlation") {
        cor_matrix <- cor(X_norm)
        dist_matrix <- 1 - abs(cor_matrix)
      } else {
        dist_matrix <- as.matrix(dist(t(X_norm)))
      }

      # === 4. COMPUTE INERTIA FOR EACH K (starting from K_min, not K=1) ===
      K_range <- K_min:K_max
      inertias <- numeric(length(K_range))

      for (i in seq_along(K_range)) {
        k <- K_range[i]

        # Run k-means with convergence for this k
        if (!is.null(self$seed)) set.seed(self$seed + k) # Vary seed per k

        centers_idx <- numeric(k)
        centers_idx[1] <- sample(n_vars, 1)

        if (k > 1) {
          for (j in 2:k) {
            min_dists <- apply(dist_matrix[, centers_idx[1:(j - 1)], drop = FALSE], 1, min)
            probs <- min_dists^2
            probs[centers_idx[1:(j - 1)]] <- 0

            if (sum(probs) == 0) {
              remaining <- setdiff(1:n_vars, centers_idx[1:(j - 1)])
              centers_idx[j] <- sample(remaining, 1)
            } else {
              probs <- probs / sum(probs)
              centers_idx[j] <- sample(n_vars, 1, prob = probs)
            }
          }
        }

        # Lloyd's algorithm (simplified, fewer iterations for speed)
        converged <- FALSE
        iter <- 0
        max_iter_elbow <- 20

        while (!converged && iter < max_iter_elbow) {
          iter <- iter + 1

          # Assignment step
          cluster_assignment <- sapply(seq_len(n_vars), function(j) {
            distances <- sapply(centers_idx, function(c) dist_matrix[j, c])
            which.min(distances)
          })

          # Update centers (medoids)
          old_centers <- centers_idx
          centers_idx <- sapply(1:k, function(c) {
            vars_in_c <- which(cluster_assignment == c)
            if (length(vars_in_c) == 0) {
              return(old_centers[c])
            }
            if (length(vars_in_c) == 1) {
              return(vars_in_c[1])
            }
            avg_dist <- sapply(vars_in_c, function(v) mean(dist_matrix[v, vars_in_c]))
            vars_in_c[which.min(avg_dist)]
          })

          if (all(centers_idx == old_centers)) converged <- TRUE
        }

        # Calculate inertia
        inertias[i] <- sum(sapply(1:k, function(c) {
          vars_idx <- which(cluster_assignment == c)
          if (length(vars_idx) <= 1) {
            return(0)
          }
          center_idx <- centers_idx[c]
          sum(dist_matrix[vars_idx, center_idx]^2)
        }))
      }

      # === 5. DETECT ELBOW USING DISTANCE-TO-LINE METHOD ===
      # Find point farthest from line connecting first and last point
      n_points <- length(K_range)

      # Normalize coordinates to [0, 1] for distance calculation
      K_norm <- (K_range - min(K_range)) / (max(K_range) - min(K_range))
      inertia_norm <- (inertias - min(inertias)) / (max(inertias) - min(inertias))

      # Line from first to last point
      x1 <- K_norm[1]
      y1 <- inertia_norm[1]
      x2 <- K_norm[n_points]
      y2 <- inertia_norm[n_points]

      # Calculate perpendicular distance from each point to the line
      distances_to_line <- numeric(n_points)
      for (i in seq_along(K_range)) {
        x0 <- K_norm[i]
        y0 <- inertia_norm[i]

        # Distance from point (x0, y0) to line through (x1, y1) and (x2, y2)
        numerator <- abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1)
        denominator <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
        distances_to_line[i] <- numerator / denominator
      }

      # The elbow is the point with maximum distance to the line
      elbow_idx <- which.max(distances_to_line)
      K_optimal <- K_range[elbow_idx]

      # === 6. VISUALIZATION ===
      if (plot) {
        plot(K_range, inertias,
          type = "b", pch = 19, col = "steelblue", lwd = 2,
          xlab = "Number of clusters (K)",
          ylab = "Inertia (within-cluster sum of squares)",
          main = "Elbow Method for Optimal K Selection",
          las = 1
        )

        # Draw the reference line
        lines(c(K_range[1], K_range[n_points]),
          c(inertias[1], inertias[n_points]),
          col = "gray50", lty = 2, lwd = 1
        )

        # Mark the elbow
        points(K_optimal, inertias[elbow_idx],
          col = "red", pch = 19, cex = 2
        )
        abline(v = K_optimal, col = "red", lty = 2, lwd = 2)
        text(K_optimal, max(inertias) * 0.9,
          labels = paste("Optimal K =", K_optimal),
          pos = 4, col = "red", font = 2
        )

        grid(col = "gray90", lty = "dotted")
      }

      message(sprintf("\u2713 Elbow method selected K = %d", K_optimal))
      return(K_optimal)
    },

    # ============================================================================
    # MÉTHODE plot() CORRIGÉE pour ClustVarKMeans
    # ============================================================================

    plot = function(type = c("heatmap", "representativeness"), ...) {
      type <- match.arg(type)

      if (!self$fitted) stop("Model must be fitted before plotting.")

      if (type == "heatmap") {
        # Extract cluster assignments
        clusters_vec <- integer(ncol(self$data))
        names(clusters_vec) <- colnames(self$data)
        for (k in seq_along(self$clusters)) {
          vars_in_k <- self$clusters[[k]]
          clusters_vec[vars_in_k] <- k
        }

        # Order variables by cluster
        var_order <- names(clusters_vec)[order(clusters_vec)]
        data_ordered <- self$data[, var_order, drop = FALSE]

        # Compute correlation matrix
        cor_mat <- cor(data_ordered)
        cor_df <- reshape2::melt(cor_mat)
        colnames(cor_df) <- c("Var1", "Var2", "Correlation")

        cor_df$Var1 <- factor(cor_df$Var1, levels = var_order)
        cor_df$Var2 <- factor(cor_df$Var2, levels = rev(var_order))

        # Plot with professional color scheme
        p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_distiller(palette = "RdBu", limit = c(-1, 1),
                                        direction = 1) +
          ggplot2::labs(title = "Correlation Heatmap (Ordered by Clusters)",
                        x = NULL, y = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
          ggplot2::coord_fixed()

        print(p)

      } else if (type == "representativeness") {
        # Calculate correlation to centers (higher = more representative)
        X_norm <- self$data

        if (self$method == "correlation") {
          # Use direct correlation (range 0 to 1 after abs)
          cor_mat <- abs(cor(X_norm))

          # Build dataframe
          repr_list <- list()
          for (k in seq_along(self$clusters)) {
            vars <- self$clusters[[k]]
            center_idx <- self$centers[k]
            center_var <- colnames(X_norm)[center_idx]

            for (v in vars) {
              repr_list[[length(repr_list) + 1]] <- data.frame(
                variable = v,
                cluster = as.factor(k),
                representativeness = cor_mat[v, center_var],
                stringsAsFactors = FALSE
              )
            }
          }
          repr_df <- do.call(rbind, repr_list)

        } else {
          # Euclidean: use 1 - normalized distance
          dist_matrix <- as.matrix(dist(t(X_norm)))

          repr_list <- list()
          for (k in seq_along(self$clusters)) {
            vars <- self$clusters[[k]]
            center_idx <- self$centers[k]
            center_var <- colnames(X_norm)[center_idx]

            # Get distances for this cluster
            cluster_dists <- dist_matrix[vars, center_var]
            max_dist <- max(cluster_dists)

            for (v in vars) {
              # Normalize: 1 = most representative (distance 0), 0 = least representative
              repr_val <- if (max_dist > 0) {
                1 - (dist_matrix[v, center_var] / max_dist)
              } else {
                1  # Single variable cluster
              }

              repr_list[[length(repr_list) + 1]] <- data.frame(
                variable = v,
                cluster = as.factor(k),
                representativeness = repr_val,
                stringsAsFactors = FALSE
              )
            }
          }
          repr_df <- do.call(rbind, repr_list)
        }

        # Plot with horizontal bars, sorted by representativeness
        p <- ggplot2::ggplot(repr_df, ggplot2::aes(x = reorder(variable, representativeness),
                                                   y = representativeness, fill = cluster)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Variable Representativeness",
                        subtitle = "Similarity to cluster center (higher = more representative)",
                        x = "Variable",
                        y = if (self$method == "correlation") "Absolute Correlation" else "Normalized Similarity") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
          ggplot2::facet_wrap(~cluster, scales = "free_y", ncol = 1)

        print(p)
      }

      invisible(self)
    }
  )
)
