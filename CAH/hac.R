#' R6 Class for Hierarchical Agglomerative Clustering (HAC) of Variables
#'
#' This class encapsulates the HAC algorithm for clustering variables.
#' It utilizes the base R function hclust().

HACVariablesR6 <- R6::R6Class(
  "HACVariablesR6",
  public = list(
    # --- PROPERTIES (Fields) ---
    k = NULL,               # Desired number of clusters (for cutting the tree)
    distance_method = NULL, # Distance calculation method ("correlation" or "euclidean")
    linkage_method = NULL,  # Aggregation (linkage) method for hclust ("ward.D2", "complete", etc.)
    model = NULL,           # Stores the hclust object resulting from the fit
    clusters = NULL,        # Named list of variables per cluster
    fitted = FALSE,         # Model fit status
    data_fit = NULL,        # Data (normalized and transposed) used for fitting

    # --- CONSTRUCTOR ---
    initialize = function(k = 2, distance_method = "correlation", linkage_method = "ward.D2") {
      # The constructor sets the initial parameters
      self$k <- k
      self$distance_method <- distance_method
      self$linkage_method <- linkage_method
    },

    # --- FIT METHOD : Model fitting on X ---
    fit = function(X) {
      # 1. Checks and Pre-processing
      if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data.frame or a matrix.")
      if (!all(sapply(X, is.numeric))) stop("All variables must be numeric.")
      if (anyNA(X)) stop("Data must not contain NA values.")

      # 2. Pre-processing: Normalization (centering and scaling)
      X_norm <- scale(X)

      # 3. Calculate Distance Matrix between VARIABLES
      if (self$distance_method == "correlation") {
        # Distance based on absolute correlation (1 - |r|)
        dist_mat <- stats::as.dist(1 - abs(stats::cor(X_norm)))
      } else if (self$distance_method == "euclidean") {
        # Euclidean distance on the transpose (variables as rows)
        dist_mat <- stats::dist(t(X_norm))
      } else {
        stop("Unrecognized distance method. Use 'correlation' or 'euclidean'.")
      }

      # Store the transposed normalized data for later use (e.g., predict)
      self$data_fit <- t(X_norm)

      # 4. Apply HAC (hclust)
      hcl <- stats::hclust(dist_mat, method = self$linkage_method)

      # 5. Store Results
      self$model <- hcl

      # Cut the tree to obtain clusters (cut based on k)
      if (self$k > 1 && self$k < ncol(X)) {
        cluster_assignment <- stats::cutree(hcl, k = self$k)
        self$clusters <- split(colnames(X), cluster_assignment)
      } else {
        warning("The number of clusters 'k' must be between 2 and the number of variables")
        self$clusters <- NULL
      }

      self$fitted <- TRUE
      invisible(self)
    },

    # Predict: Assigns the variables in X (illustrative variables) to the best cluster
    # X must contain the same observations (rows) as the training data, in the same order.
    predict = function(X) {
      if (!self$fitted) stop("The model must be fitted with $fit() before prediction.")
      if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data.frame or a matrix.")
      if (!all(sapply(X, is.numeric))) stop("All variables must be numeric.")

      # CRITICAL CHECK (Must be handled externally/by user but good to warn)
      if (nrow(X) != ncol(self$data_fit)) {
        stop("X must have the same number of observations (rows) as the data used for fitting.")
      }

      # 1. Normalization (same as fit)
      # Normalization must be done independently on X as these are new variables.
      X_pred_norm <- scale(X)

      # 2. Extract original variables' names
      original_vars <- unlist(self$clusters)
      illustrative_vars <- colnames(X)

      # 3. Combine the (original and illustrative) data for correlation calculation
      # We need original variables in columns (t(self$data_fit))
      # and new variables in columns (X_pred_norm).
      X_all <- cbind(t(self$data_fit), X_pred_norm)

      # 4. Calculate correlation matrix between illustrative variables and ALL original variables
      # Correlate X variables (rows) with original variables (columns)
      cor_mat_all <- stats::cor(X_all)
      cor_mat_pred <- cor_mat_all[illustrative_vars, original_vars, drop = FALSE]

      cluster_pred <- data.frame(
        variable = illustrative_vars,
        cluster = NA,
        avg_correlation = NA
      )

      # 5. Assignment: Assign to the cluster with the highest average absolute correlation
      for (i in seq_along(illustrative_vars)) {
        var_name <- illustrative_vars[i]

        # Calculate average absolute correlation to variables in each existing cluster
        avg_corrs <- sapply(self$clusters, function(cluster_vars) {
          # Take the correlation values for the current illustrative variable (var_name)
          # only for the variables belonging to the cluster (cluster_vars)
          mean(abs(cor_mat_pred[var_name, cluster_vars]))
        })

        best_cluster <- which.max(avg_corrs)

        cluster_pred[i, "cluster"] <- best_cluster
        cluster_pred[i, "avg_correlation"] <- max(avg_corrs) # Numerical indicator
      }

      return(cluster_pred)
    },

    # plot: Plots the hierarchical clustering tree (Dendrogram)
    plot = function(k = self$k) {
      if (!self$fitted) stop("The model must be fitted with $fit() before plotting.")

      # L'argument horiz=TRUE est transmis directement à plot.hclust
      # Le système R génère souvent des avertissements si les autres paramètres
      # (comme xlab, ylab) sont utilisés en même temps, car ils sont gérés différemment.

      # Solution pour le rendu horizontal : utiliser le paramètre standard de plot.hclust
      # Note: on simplifie les labels pour réduire le risque d'avertissements de transmission.

      plot(self$model,
           main = paste("HAC of Variables (Lien:", self$linkage_method, ")"),
           xlab = "Distance",
           ylab = "Variables",
           sub = "",
           horiz = TRUE # Ceci est la façon correcte de le faire, malgré les avertissements de votre session.
      )

      # Correction de l'erreur de namespace (utilisation de stats::)
      if (k > 1 && k < length(self$model$labels)) {
        stats::rect.hclust(self$model, k = k, border = 2:(k+1))
      }
      invisible(self)
    },

    # Print: Displays succinct information about the model
    print = function() {
      cat("HACVariablesR6 Model\n")
      cat("--------------------\n")
      cat("k (Cut):", self$k,
          "| Distance:", self$distance_method,
          "| Linkage:", self$linkage_method,
          "| Fitted:", self$fitted, "\n")
      if (self$fitted && !is.null(self$clusters)) {
        cat("Variables per cluster (k =", self$k, ") :\n")
        # Displays the count of variables in each cluster
        print(sapply(self$clusters, length))
      }
      invisible(self)
    },

    # Summary: Displays detailed information about the model
    summary = function() {
      cat("HACVariablesR6 Summary\n")
      cat("----------------------\n")
      cat("Model Parameters:\n")
      cat("  Number of clusters (k):", self$k, "\n")
      cat("  Distance method:", self$distance_method, "\n")
      cat("  Linkage method:", self$linkage_method, "\n")
      cat("  Status:", ifelse(self$fitted, "Fitted", "Not Fitted"), "\n")

      if (self$fitted && !is.null(self$clusters)) {
        cat("\nCluster Composition (k =", self$k, "):\n")

        # Lists the variables in each cluster (detailed information)
        for (cluster_name in names(self$clusters)) {
          vars <- self$clusters[[cluster_name]]
          cat("  Cluster", cluster_name, "(N =", length(vars), "):", paste(vars, collapse = ", "), "\n")
        }

        cat("\nInterpretation Tools Guide:\n")
        cat("  - To visualize the dendrogram (tree structure): call $plot().\n")
        cat("  - To predict new variables: call $predict(X_new).\n")
      }
      invisible(self)
    }
  )
)
