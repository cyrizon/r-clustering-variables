#' R6 class for Hierarchical Agglomerative Clustering (HAC) of variables
#'
#' @description
#' This class wraps the HAC algorithm for clustering numeric variables.
#' It uses the base R function stats::hclust() on a distance matrix computed
#' between the variables (columns of the input data).
#'
#' @export
ClustVarHAC <- R6::R6Class(
  "ClustVarHAC",
  public = list(
    # --- PROPERTIES (Fields) ---
    #' @field K Integer, the desired number of clusters (for cutting the tree).
    K = NULL,
    #' @field method Character string, the distance metric to use: "correlation" (1 - |r|) or "euclidean" (on transposed data).
    method = NULL, # Distance calculation method ("correlation" or "euclidean")
    #' @field linkage_method Character string, the agglomeration (linkage) method for stats::hclust: e.g., "ward.D2", "complete", "average".
    linkage_method = NULL, # Agglomeration (linkage) method for hclust ("ward.D2", "complete", etc.)
    #' @field model Stores the hclust object resulting from fit.
    model = NULL, # Stores the hclust object resulting from fit
    #' @field dist_matrix The distance matrix (stats::dist object) used for hclust.
    dist_matrix = NULL, # Distance matrix used for hclust
    #' @field clusters Named list where keys are cluster IDs (1 to K) and values are vectors of variable names.
    clusters = NULL, # Named list of variables per cluster
    #' @field fitted Logical, fit status of the model (TRUE if $fit() has been executed).
    fitted = FALSE, # Fit status of the model
    #' @field data The transposed, normalized (scaled) data matrix used for fitting (variables as rows, observations as columns).
    data = NULL, # Data (normalized and transposed) used for fitting

    # --- CONSTRUCTOR ---
    #' @description
    #' Create a new ClustVarHAC object.
    #' @param K Integer, the number of clusters to cut the dendrogram into (default: 2).
    #' @param method Character string, the distance metric to use (default: "correlation").
    #' @param linkage_method Character string, the agglomeration method for hclust (default: "ward.D2").
    #' @return A new `ClustVarHAC` object.
    initialize = function(K = 2, method = "correlation", linkage_method = "ward.D2") {
      # Constructor: set initial parameters
      self$K <- K
      self$method <- method
      self$linkage_method <- linkage_method
    },

    # --- fit METHOD: Fit the model on X ---
    #' @description
    #' Fits the HAC model by calculating the distance matrix between variables and performing hierarchical clustering.
    #' @param X A data.frame or matrix containing only numeric variables to cluster.
    #' @return The object itself (invisibly) for method chaining.
    fit = function(X) {
      # 1. Checks and preprocessing
      if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data.frame or matrix.")
      if (!all(sapply(X, is.numeric))) stop("All variables must be numeric.")
      if (anyNA(X)) stop("Data must not contain NA.")

      # 2. Preprocessing: normalization (center and scale)
      X_norm <- scale(X)

      # 3. Computing distance matrix between variables
      if (self$method == "correlation") {
        # Distance based on absolute correlation (1 - |r|)
        dist_mat <- stats::as.dist(1 - abs(stats::cor(X_norm)))
      } else if (self$method == "euclidean") {
        # Euclidean distance on the transpose (variables as rows)
        dist_mat <- stats::dist(t(X_norm))
      } else {
        stop("Unrecognized distance method. Use 'correlation' or 'euclidean'.")
      }

      # Store normalized transposed data for later use (e.g., predict)
      self$data <- t(X_norm)
      # Store the distance matrix for cophenetic correlation calculation
      self$dist_matrix <- dist_mat

      # 4. Apply HAC (hclust)
      hcl <- stats::hclust(dist_mat, method = self$linkage_method)

      # 5. Store the results
      self$model <- hcl

      # Cut the tree to obtain clusters (cut based on K)
      if (self$K > 1 && self$K < ncol(X)) {
        cluster_assignment <- stats::cutree(hcl, k = self$K)
        self$clusters <- split(colnames(X), cluster_assignment)
      } else {
        warning("The number of clusters 'K' must be between 2 and the number of variables")
        self$clusters <- NULL
      }

      self$fitted <- TRUE
      invisible(self)
    },

    # Predict: attach variables in X (illustrative variables) to the best cluster
    #' @description
    #' Predicts the cluster membership for new, illustrative numeric variables based on their mean absolute correlation to the existing clusters.
    #' @param newdata A data.frame or matrix of new numeric variables (must have the same number of observations as the training data).
    #' @return A data.frame with the variable name, assigned cluster ID, and the maximal average absolute correlation score.
    predict = function(newdata) {
      if (!self$fitted) stop("Model must be fitted with $fit() before prediction.")
      if (!is.data.frame(newdata) && !is.matrix(newdata)) stop("newdata must be a data.frame or matrix.")
      if (!all(sapply(newdata, is.numeric))) stop("All variables must be numeric.")

      # CRITICAL CHECK (Should be handled externally/by the user but good to warn)
      if (nrow(newdata) != ncol(self$data)) {
        stop("newdata must have the same number of observations (rows) as the data used for fitting.")
      }

      # 1. Normalization (same as in fit)
      # Normalization must be done independently on newdata because these are new variables.
      X_pred_norm <- scale(newdata)

      # 2. Extraire les noms des variables originales
      original_vars <- unlist(self$clusters)
      illustrative_vars <- colnames(newdata)

      # 3. Combine original and illustrative data for correlation computation
      # We need original variables as columns (t(self$data))
      # and new variables as columns (X_pred_norm).
      X_all <- cbind(t(self$data), X_pred_norm)

      # 4. Compute correlation matrix between illustrative variables and ALL original variables
      # Correlate variables newdata (rows) with original variables (columns)
      cor_mat_all <- stats::cor(X_all)
      cor_mat_pred <- cor_mat_all[illustrative_vars, original_vars, drop = FALSE]

      cluster_pred <- data.frame(
        variable = illustrative_vars,
        cluster = NA,
        avg_correlation = NA
      )

      # 5. Assignment: assign to the cluster with the highest mean absolute correlation
      for (i in seq_along(illustrative_vars)) {
        var_name <- illustrative_vars[i]

        # Compute the mean absolute correlation to variables of each existing cluster
        avg_corrs <- sapply(self$clusters, function(cluster_vars) {
          # Take the correlation values for the current illustrative variable (var_name)
          # uniquement pour les variables appartenant au cluster (cluster_vars)
          mean(abs(cor_mat_pred[var_name, cluster_vars]))
        })

        best_cluster <- which.max(avg_corrs)

        cluster_pred[i, "cluster"] <- best_cluster
        cluster_pred[i, "avg_correlation"] <- max(avg_corrs) # Numeric indicator
      }

      return(cluster_pred)
    },

    #' @description
    #' Generates various diagnostic and interpretation plots for the HAC model.
    #' @details Requires the \code{ggplot2}, \code{ggdendro}, and \code{reshape2} packages.
    #' @param type Character string indicating the type of plot to generate. Must be one of:
    #'   \itemize{
    #'     \item \code{"dendrogram"}: Displays the hierarchical tree with the cut line for the selected \code{self$K}. Requires \code{ggdendro}.
    #'     \item \code{"heights"}: Plots the fusion heights of the agglomerative steps (similar to a scree plot). This helps in visually selecting the optimal \code{K} based on the "elbow" rule.
    #'     \item \code{"heatmap"}: Displays the correlation matrix of the variables, ordered by their cluster assignment. This confirms intra-cluster cohesion.
    #'     \item \code{"representativeness"}: Bar chart showing each variable's similarity (correlation or normalized distance) to its calculated cluster centroid. Variables with high scores are considered the most representative of their cluster.
    #'   }
    #' @param ... Additional arguments passed to specific plot functions.
    #' @return A ggplot2 object.
    plot = function(type = c("dendrogram", "heights", "heatmap", "representativeness"), ...) {
      # Validation
      if (!self$fitted) stop("Model must be fitted with $fit() before plotting.")
      type <- match.arg(type)

      # --- 1. DENDROGRAM (Ameliorated "S3" style ) ---
      if (type == "dendrogram") {
        if (!requireNamespace("ggdendro", quietly = TRUE)) stop("Package 'ggdendro' is required.")
        # Conversion to dendrogramme for ggdendro
        dend <- as.dendrogram(self$model)
        dend_data <- ggdendro::dendro_data(dend, type = "rectangle")

        # Precise calculation of cut height
        # hclust$height contains the sorted heights.
        # For K clusters, the cut will be between N-K and N-K+1
        n_vars <- length(self$model$labels)
        h <- self$model$height

        # If K=2, the cut is between the penultimate and the last merger
        # Indexing the merger which creates K clusters :
        idx_cut <- n_vars - self$K

        if (self$K == 1) {
          cut_val <- max(h) * 1.05
        } else if (self$K == n_vars) {
          cut_val <- min(h) / 2
        } else {
          # Average between the height where we have K clusters and the height where we reach K-1
          cut_val <- (h[idx_cut] + h[idx_cut + 1]) / 2
        }

        # Plot Building
        p <- ggplot2::ggplot(ggdendro::segment(dend_data)) +
          ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            linewidth = 0.6, color = "gray30"
          ) + # linewidth au lieu de size
          ggplot2::geom_text(
            data = ggdendro::label(dend_data),
            ggplot2::aes(x = x, y = y, label = label),
            hjust = 1, nudge_y = -0.01, size = 3.5
          ) +
          # Cutting line
          ggplot2::geom_hline(
            yintercept = cut_val,
            color = "#E41A1C", linetype = "dashed", linewidth = 0.8
          ) +
          # Labels and theme
          ggplot2::labs(
            title = "Hierarchical Clustering Dendrogram",
            subtitle = paste(
              "Linkage:", self$linkage_method, "| Distance:", self$method,
              "\nRed line indicates cut for K =", self$K
            ),
            x = NULL, y = "Height"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(), # On cache les indices numériques Y
            axis.ticks.y = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(face = "bold", size = 12),
            plot.subtitle = ggplot2::element_text(color = "gray50", size = 10)
          ) +
          # Horizontal orientation (for better readability)
          ggplot2::coord_flip() +
          ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = c(0.15, 0.05)))

        print(p)
        return(invisible(p))

        # --- 2. HEIGHTS (Scree Plot) ---
      } else if (type == "heights") {
        h <- self$model$height
        n <- length(h)
        n_show <- min(20, n)
        last_heights <- rev(tail(h, n_show))

        df_h <- data.frame(k = 2:(n_show + 1), height = last_heights)

        p <- ggplot2::ggplot(df_h, ggplot2::aes(x = k, y = height)) +
          ggplot2::geom_line(color = "steelblue", linewidth = 1) +
          ggplot2::geom_point(size = 3, color = "steelblue") +
          ggplot2::geom_vline(xintercept = self$K, color = "#E41A1C", linetype = "dashed", linewidth = 0.8) +
          ggplot2::geom_text(ggplot2::aes(label = round(height, 2)), vjust = -0.8, size = 3) +
          ggplot2::labs(
            title = "Cluster Fusion Heights (Inertia Gain)",
            subtitle = "Look for the 'elbow' to choose K",
            x = "Number of Clusters (K)",
            y = "Fusion Height"
          ) +
          ggplot2::scale_x_continuous(breaks = df_h$k) +
          ggplot2::theme_minimal()

        print(p)
        return(invisible(p))

        # --- 3. HEATMAP (Code Heatmap Pro) ---
      } else if (type == "heatmap") {
        if (is.null(self$clusters)) stop("No clusters found.")

        # Collecting and sorting clusters
        var_names <- rownames(self$data)
        cluster_vec <- setNames(rep(NA, length(var_names)), var_names)
        for (k_idx in seq_along(self$clusters)) {
          cluster_vec[self$clusters[[k_idx]]] <- k_idx
        }

        ordered_vars <- names(sort(cluster_vec))
        X_ordered <- t(self$data[ordered_vars, , drop = FALSE])
        cor_mat <- cor(X_ordered)

        cor_df <- reshape2::melt(cor_mat)
        colnames(cor_df) <- c("Var1", "Var2", "Correlation")
        cor_df$Var1 <- factor(cor_df$Var1, levels = ordered_vars)
        cor_df$Var2 <- factor(cor_df$Var2, levels = rev(ordered_vars))

        p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
          ggplot2::geom_tile(color = "white", linewidth = 0.2) +
          ggplot2::scale_fill_distiller(palette = "RdBu", direction = -1, limit = c(-1, 1), name = "Corr") +
          ggplot2::labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid = ggplot2::element_blank()
          ) +
          ggplot2::coord_fixed()

        print(p)
        return(invisible(p))

        # --- 4. REPRESENTATIVENESS (Adapted from K-Means) ---
      } else if (type == "representativeness") {
        # Note : While using HAC, variables must on LINES in self$data
        # Calculating a "centroid" (average profile) fo each cluster.

        repr_list <- list()

        for (k_idx in seq_along(self$clusters)) {
          vars_in_k <- self$clusters[[k_idx]]
          if (length(vars_in_k) == 0) next

          # Extracting data from cluster (Vars x Obs)
          sub_data <- self$data[vars_in_k, , drop = FALSE]

          # Computing centroid (Average of the cluster variables)
          # if number of variables = 1, it is the center itself
          if (length(vars_in_k) == 1) {
            centroid <- sub_data[1, ]
          } else {
            centroid <- colMeans(sub_data)
          }

          # --- CASE 1 : CORRELATION ---
          if (self$method == "correlation") {
            # Calculate the correlation of each variable with the average profile (centroid)
            # cor(x, y) calcule la corrélation linéaire
            # Transposing sub_data to get (Obs x Vars) for function cor
            corrs <- as.vector(abs(cor(t(sub_data), centroid)))

            df_k <- data.frame(
              variable = vars_in_k,
              cluster = as.factor(k_idx),
              representativeness = corrs,
              stringsAsFactors = FALSE
            )

            # --- CASE 2 : EUCLIDEAN  ---
          } else {
            # Computen euclidean distance to the centroid
            dists <- apply(sub_data, 1, function(x) sqrt(sum((x - centroid)^2)))
            max_dist <- max(dists)

            # Normalizing : 1 = in the center (dist 0), 0 = the furthest away
            repr_val <- if (max_dist > 0) {
              1 - (dists / max_dist)
            } else {
              rep(1, length(dists))
            }

            df_k <- data.frame(
              variable = vars_in_k,
              cluster = as.factor(k_idx),
              representativeness = repr_val,
              stringsAsFactors = FALSE
            )
          }
          repr_list[[k_idx]] <- df_k
        }

        repr_df <- do.call(rbind, repr_list)

        # Plot (Identic style to the K-Means one : Horizontal sorted bars)
        p <- ggplot2::ggplot(repr_df, ggplot2::aes(
          x = reorder(variable, representativeness),
          y = representativeness, fill = cluster
        )) +
          ggplot2::geom_col() + # Barres standard (plus fines que le pavé précédent)
          ggplot2::coord_flip() +
          ggplot2::labs(
            title = "Variable Representativeness",
            subtitle = "Similarity to cluster centroid (higher = more representative)",
            x = "Variable",
            y = if (self$method == "correlation") "Absolute Correlation w/ Centroid" else "Normalized Similarity"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
          # ncol = 1 pour empiler les clusters comme dans ton exemple K-Means
          ggplot2::facet_wrap(~cluster, scales = "free_y", ncol = 1)

        print(p)
        return(invisible(p))
      }
    },

    # Print: display concise model information
    #' @description
    #' Prints a concise summary of the HAC model parameters and status.
    #' @return The object itself (invisibly).
    print = function() {
      cat("ClustVarHAC Model\n")
      cat("--------------------\n")
      cat(
        "K (Cut):", self$K,
        "| Distance:", self$method,
        "| Linkage:", self$linkage_method,
        "| Fitted:", self$fitted, "\n"
      )
      if (self$fitted && !is.null(self$clusters)) {
        cat("Variables per cluster (K =", self$K, ") :\n")
        # Print the number of variables in each cluster
        print(sapply(self$clusters, length))
      }
      invisible(self)
    },

    # Summary: display detailed model information
    #' @description
    #' Displays a detailed summary including model parameters, the final cluster composition, and the cophenetic correlation.
    #' @return The object itself (invisibly).
    summary = function() {
      cat("ClustVarHAC Summary\n")
      cat("----------------------\n")
      cat("Model parameters:\n")
      cat("  Number of clusters (K):", self$K, "\n")
      cat("  Distance method:", self$method, "\n")
      cat("  Linkage method:", self$linkage_method, "\n")
      cat("  Status:", ifelse(self$fitted, "Fitted", "Not fitted"), "\n")

      if (self$fitted && !is.null(self$clusters)) {
        cat("\nCluster composition (K =", self$K, "):\n")

        # List variables in each cluster (detailed info)
        for (cluster_name in names(self$clusters)) {
          vars <- self$clusters[[cluster_name]]
          cat("  Cluster", cluster_name, "(N =", length(vars), "):", paste(vars, collapse = ", "), "\n")
        }

        cat("\nInterpretation tools guide:\n")
        cat("  - To view the dendrogram (tree structure): call $plot().\n")
        cat("  - To predict new variables: call $predict(newdata).\n")
      }
      invisible(self)
    }
  )
)
