#' Clustering for categorical variables using MCA
#'
#' @description
#' This R6 class implements the iterative algorithm for clustering categorical variables using MCA (Multiple Correspondence Analysis)
#' as a method to define the cluster's synthetic axis. It aims to maximize the association (measured by Khi-squared)
#' between each variable and its cluster's axis.
#'
#' @export
ClustVarACM <- R6::R6Class(
  classname = "ClustVarACM",
  public = list(
    #' @field data dataframe containing the variables to cluster; each variable must be categorical(type factor)
    data = NULL,
    #' @field K integer, the number of clusters requested
    K = NULL,
    #' @field clusters integer vector of length p (number of variables), holding the cluster assignment for each variable
    clusters = NULL,
    #' @field axes_list list of length K, containing the factorial axis (first principal component) associated with each cluster
    axes_list = NULL,
    #' @field score_matrix matrix (p x K) containing the association strength between each variable and each cluster axis; the score corresponds to 1-p_value from KHI^2 test
    score_matrix = NULL,
    #' @field cramer_matrix Matrix (p x K) storing Cramer's V for visualization
    cramer_matrix = NULL,
    #' @field Q_trace numeric vector, holding the successive values ofr the criterion Q (sum of best scores)
    Q_trace = NULL,
    #' @field Q_final numeric, the final measure for the clustering quality (sum of intra-cluster associations)
    Q_final = NULL,
    #' @field tol numeric tolerance for the stop criterion (difference in Q between iterations)
    tol = NULL,
    #' @field max_iter integer maximal number of iterations for the reallocation algorithm
    max_iter = NULL,


    #' @description
    #' Create a new ClustVarACM R6 object.
    #' @param K Number of clusters (integer).
    #' @param max_iter Maximal number of iterations for the clustering algorithm (integer, default 30).
    #' @param tol Tolerance for stop criterion (numeric, default 1e-4).
    #' @return A new `ClustVarACM` object.
    initialize = function(K = 3, max_iter = 30, tol = 1e-4) {
      # Do not require `data` at initialization; it's provided to fit().
      self$K <- K
      self$max_iter <- max_iter
      self$tol <- tol
      self$clusters <- NULL
    },


    #' @description
    #' Fit the clustering model on the active variables.
    #' @param X A data.frame with categorical variables (factors) to cluster (optional if provided at initialization).
    #' @return Self (invisibly) for method chaining.
    fit = function(X = self$data) {
      self$data <- X # Use X if provided, otherwise use self$data
      p <- ncol(self$data)
      Q_old <- 0
      self$Q_trace <- c()
      # Input validation
      if (!is.data.frame(self$data)) stop("The input object must be a data.frame.")
      if (!all(sapply(self$data, is.factor))) stop("All variables must be categorical (type factor).")
      if (anyNA(self$data)) stop("Data must not contain NA values.")
      if (self$K < 2 || self$K > p) stop("K must be between 2 and the number of variables.")

      # Initialize clusters: use a balanced round-robin initialization (then shuffle)
      if (is.null(self$clusters) || length(self$clusters) != p) {
        init_seq <- rep(1:self$K, length.out = p)
        self$clusters <- sample(init_seq)
        names(self$clusters) <- names(self$data)
      }

      # Main iterative algorithm
      for (iter in 1:self$max_iter) {
        # Wrap iteration in tryCatch
        iter_result <- tryCatch({
          # 1. Axis calculation (Synthetic variable for each cluster via MCA)
          self$axes_list <- vector("list", self$K)
          for (k in 1:self$K) {
            vars_k <- names(self$data)[self$clusters == k]
            if (length(vars_k) == 0) {
              self$axes_list[[k]] <- NULL
            } else if (length(vars_k) == 1) {
              self$axes_list[[k]] <- scale(as.numeric(self$data[[vars_k[1]]]), center = TRUE, scale = FALSE)
            } else {
              acm_k <- FactoMineR::MCA(self$data[, vars_k, drop = FALSE], ncp = 1, graph = FALSE)
              if (!is.null(acm_k$ind) && !is.null(acm_k$ind$coord) && ncol(acm_k$ind$coord) >= 1) {
                self$axes_list[[k]] <- acm_k$ind$coord[, 1]
              } else {
                warning(sprintf("MCA for cluster %d returned unexpected structure; using fallback axis.", k))
                self$axes_list[[k]] <- scale(as.numeric(self$data[[vars_k[1]]]), center = TRUE, scale = FALSE)
              }
            }
          }

          # 2. Reallocation through KHI^2 test score calculation
          self$score_matrix <- matrix(0,
                                      nrow = p, ncol = self$K,
                                      dimnames = list(names(self$data), paste0("Cluster_", 1:self$K))
          )

          for (j in 1:p) {
            fac <- self$data[[j]]

            for (k in 1:self$K) {
              if (!is.null(self$axes_list[[k]])) {
                axis_vec <- self$axes_list[[k]]
                if (length(axis_vec) != nrow(self$data)) {
                  axis_vec <- rep(NA_real_, nrow(self$data))
                }
                zdisc <- cut(axis_vec, breaks = 5, include.lowest = TRUE, ordered_result = TRUE)

                # --- CORRECTION ICI : Création PUIS Nettoyage ---
                tab <- table(fac, zdisc)
                tab <- tab[, colSums(tab) > 0, drop = FALSE]

                if (nrow(tab) < 2 || ncol(tab) < 2) {
                  score <- 0
                } else {
                  test <- suppressWarnings(chisq.test(tab, correct = FALSE))
                  pval <- test$p.value
                  if (is.na(pval) || !is.finite(pval)) {
                    score <- 0
                  } else {
                    score <- 1 - pval
                  }
                }

                self$score_matrix[j, k] <- score
              } else {
                self$score_matrix[j, k] <- -Inf
              }
            }
          }

          # Assign each variable to the cluster with the maximum association score
          new_clusters <- apply(self$score_matrix, 1, which.max)

          # Handle empty clusters
          empty_clusters <- setdiff(seq_len(self$K), unique(new_clusters))
          if (length(empty_clusters) > 0) {
            cluster_counts <- table(factor(new_clusters, levels = seq_len(self$K)))
            for (empty_k in empty_clusters) {
              largest_k <- as.integer(names(cluster_counts)[which.max(cluster_counts)])
              members <- which(new_clusters == largest_k)
              if (length(members) == 0) {
                candidate <- sample(seq_len(p), 1)
              } else {
                scores_in_largest <- sapply(members, function(i) {
                  val <- self$score_matrix[i, largest_k]
                  if (is.na(val) || !is.finite(val)) return(Inf)
                  return(val)
                })
                candidate <- members[which.min(scores_in_largest)]
              }
              new_clusters[candidate] <- empty_k
              cluster_counts <- table(factor(new_clusters, levels = seq_len(self$K)))
            }
          }

          # Ensure no NA cluster assignments
          if (any(is.na(new_clusters))) {
            na_idx <- which(is.na(new_clusters))
            for (ri in na_idx) {
              row_scores <- self$score_matrix[ri, , drop = TRUE]
              row_scores[is.na(row_scores)] <- -Inf
              new_clusters[ri] <- which.max(row_scores)
              if (is.na(new_clusters[ri]) || !is.finite(new_clusters[ri])) {
                new_clusters[ri] <- sample(1:self$K, 1)
              }
            }
          }

          list(new_clusters = new_clusters, score_matrix = self$score_matrix)
        }, error = function(err) { stop(err) })

        new_clusters <- iter_result$new_clusters
        self$score_matrix <- iter_result$score_matrix

        # 3. Criterion Q calculation
        q_values <- numeric(p)
        for (j in seq_len(p)) {
          idx <- new_clusters[j]
          valid_idx <- !(is.na(idx) || idx < 1 || idx > ncol(self$score_matrix))
          if (!valid_idx) {
            q_values[j] <- 0
          } else {
            q_values[j] <- self$score_matrix[j, idx]
          }
        }
        Q_new <- sum(q_values, na.rm = TRUE)
        self$Q_trace <- c(self$Q_trace, Q_new)

        if (abs(Q_new - Q_old) < self$tol) {
          break
        }

        self$clusters <- new_clusters
        names(self$clusters) <- names(self$data)
        Q_old <- Q_new
      }

      # --- Calcul de la matrice de Cramér pour la visualisation (Heatmap) ---
      p <- ncol(self$data)
      self$cramer_matrix <- matrix(0, nrow = p, ncol = self$K,
                                   dimnames = list(names(self$data), paste0("Cluster_", 1:self$K)))

      for (j in 1:p) {
        fac <- self$data[[j]]
        for (k in 1:self$K) {
          if (!is.null(self$axes_list[[k]])) {
            axis_vec <- self$axes_list[[k]]
            if(length(axis_vec) != nrow(self$data)) axis_vec <- rep(NA, nrow(self$data))
            zdisc <- cut(axis_vec, breaks = 5, include.lowest = TRUE, ordered_result = TRUE)

            # Création et nettoyage table
            tab <- table(fac, zdisc)
            tab <- tab[, colSums(tab) > 0, drop = FALSE]

            if (nrow(tab) >= 2 && ncol(tab) >= 2) {
              test <- suppressWarnings(chisq.test(tab, correct = FALSE))
              n_obs <- sum(tab)
              min_dim <- min(nrow(tab), ncol(tab))
              self$cramer_matrix[j, k] <- sqrt(test$statistic / (n_obs * (min_dim - 1)))
            }
          }
        }
      }

      self$Q_final <- Q_old
      invisible(self)
    },

    #' @description
    #' Print a succinct summary of the model.
    #' @return Nothing (invisible NULL).
    print = function() {
      if (is.null(self$Q_final)) {
        cat("Clustering Model: ClustVarACM (K=", self$K, ") - Not yet fitted.\n", sep = "")
      } else {
        cat("Clustering Model: ClustVarACM\n")
        cat("  - Number of variables: ", ncol(self$data), "\n", sep = "")
        cat("  - Number of clusters (K): ", self$K, "\n", sep = "")
        cat("  - Final Criterion Q: ", round(self$Q_final, 4), "\n", sep = "")
      }
      invisible(NULL)
    },


    #' @description
    #' Display a detailed summary of the clustering results.
    #' @return A list (invisibly) containing clusters, Q, and normalized Q.
    summary = function() {
      if (is.null(self$Q_final)) stop("The model has not been fitted yet (run fit()).")

      cat("---- Model Summary: ClustVarACM ----\n")
      cat("Number of variables:", ncol(self$data), "\n")
      cat("Number of clusters (K):", self$K, "\n")
      cat("Final Criterion Q:", round(self$Q_final, 4), "\n")
      cat("Average Quality (Q/p):", round(self$Q_final / ncol(self$data), 4), "\n")
      cat("Iterations performed:", length(self$Q_trace), "\n\n")

      cat("Final Variable Partition:\n")
      print(data.frame(Variable = names(self$data), Cluster = self$clusters))
      # Additional detailed info: variables per cluster
      cat("\nVariables per Cluster:\n")
      cluster_counts <- table(self$clusters)
      for (k in 1:self$K) {
        vars_k <- names(self$data)[self$clusters == k]
        cat(sprintf("  Cluster %d (%d var.): %s\n", k, cluster_counts[as.character(k)], paste(vars_k, collapse = ", ")))
      }
      invisible(list(
        clusters = self$clusters,
        Q = self$Q_final,
        Q_normalized = self$Q_final / ncol(self$data)
      ))
    },


    #' @description
    #' Automatic selection for the optimal number of clusters K using the elbow method on the Q criterion.
    #' @param X A data.frame with categorical variables (factors) to cluster
    #' @param K_grid Integer vector of K values to test (e.g., 2:6).
    #' @param threshold Numeric tolerance for the relative gain in Q (default 0.1, or 10%).
    #' @return A list containing the results (K and Q values) and the suggested optimal K.
    select_K = function(X = self$data, K_grid = 2:6, threshold = 0.1) {
      self$data <- X # Use X if provided, otherwise use self$data
      if (is.null(self$data)) stop("Data must be provided to select K.")
      if (min(K_grid) < 2 || max(K_grid) > ncol(self$data)) stop("K_grid must be within [2, number of variables].")

      results <- data.frame(K = K_grid, Q = NA)
      cat(sprintf("Testing K from %d to %d...\n", min(K_grid), max(K_grid)))

      for (i in seq_along(K_grid)) {
        tmp <- ClustVarACM$new(K = K_grid[i])
        tmp$fit(self$data)
        results$Q[i] <- tmp$Q_final
      }

      # Calculate successive differences in Q (gain)
      dQ <- diff(results$Q)
      # Calculate relative gain compared to the maximum observed gain
      rel_gain <- dQ / max(dQ, na.rm = TRUE)

      # Find K where the relative gain falls below the threshold (elbow)
      K_opt_index <- which(rel_gain < threshold)[1]

      if (is.na(K_opt_index)) {
        # If no elbow found, suggest the K that maximizes Q (the largest tested K)
        K_opt <- results$K[which.max(results$Q)]
      } else {
        # The optimal K is the one *after* the drop in gain
        K_opt <- results$K[K_opt_index + 1]
      }

      # Visualization for the selection of K, based on criterion Q
      plot(results$K, results$Q,
        type = "b", pch = 19, col = "blue",
        xlab = "Nombre de clusters (K)", ylab = "Critere global Q",
        main = "Methode du coude pour la selection du K optimal"
      )
      abline(v = K_opt, col = "red", lty = 2)
      text(K_opt, max(results$Q), labels = paste("K* =", K_opt), pos = 4, col = "red")

      cat(sprintf("- Optimal K automatically selected: %d (threshold %.0f%%)\n", K_opt, threshold * 100))
      return(invisible(list(results = results, K_opt = K_opt)))
    },


    #' @description
    #' Visualization for Q convergence (tracking Q across iterations).
    #' @return Nothing (invisible NULL), generates a plot.
    plot_Q = function() {
      if (is.null(self$Q_trace)) stop("The model has not been fitted yet (fit() must be run).")
      plot(self$Q_trace,
        type = "b", pch = 19, col = "blue",
        xlab = "Iteration", ylab = "Criterion Q",
        main = "Evolution of Criterion Q"
      )
      abline(h = self$Q_final, col = "red", lty = 2)
      text(length(self$Q_trace), self$Q_final, labels = "Q_final", pos = 1, col = "red")
    },


    #' @description
    #' Predict cluster membership for new illustrative variables.
    #' The new variables are not used to modify the clusters (non-active).
    #' @param newdata A data.frame with categorical variables to classify (must have the same number of observations as the training data).
    #' @return A data.frame with variable names and their assigned cluster.
    predict = function(newdata) {
      if (is.null(self$axes_list) || all(sapply(self$axes_list, is.null))) {
        stop("The model must be fitted (fit() must be run) before prediction.")
      }
      if (nrow(newdata) != nrow(self$data)) {
        stop("The number of observations in 'newdata' must be the same as in the training data.")
      }
      if (!all(sapply(newdata, is.factor))) {
        warning("All variables in 'newdata' should be factors for proper analysis.")
      }
      if (!is.data.frame(newdata)) {
        stop("Your object must be a data.frame")
      }


      p_new <- ncol(newdata)
      score_new <- matrix(0,
        nrow = p_new, ncol = self$K,
        dimnames = list(names(newdata), paste0("Cluster_", 1:self$K))
      )

      for (j in 1:p_new) { # Iterate over new variables
        fac <- as.factor(newdata[[j]])

        for (k in 1:self$K) { # Test association with each cluster axis
          if (!is.null(self$axes_list[[k]])) {
            # Discretize the cluster axis (based on training data)
            zdisc <- cut(self$axes_list[[k]], breaks = 5, include.lowest = TRUE, ordered_result = TRUE)


            # Contingency table
            tab <- table(fac, zdisc)
            tab <- tab[, colSums(tab) > 0, drop = FALSE]

            if (nrow(tab) < 2 || ncol(tab) < 2) {
              score <- 0
            } else {
              test <- suppressWarnings(chisq.test(tab, correct = FALSE))
              score <- 1 - test$p.value
            }

            score_new[j, k] <- score
          } else {
            score_new[j, k] <- -Inf
          }
        }
      }

      clusters_pred <- apply(score_new, 1, which.max)

      # Prepare the output: variable, assigned cluster, and max score (association strength)
      max_scores <- apply(score_new, 1, max, na.rm = TRUE)

      return(data.frame(
        Variable = names(newdata),
        Cluster_Assigned = clusters_pred,
        Max_Association_Score = max_scores
      ))
    },

    #' @description
    #' Plot visualizations for the ACM clustering model.
    #' @param type Character string indicating the type of plot to generate. Must be one of:
        #'   \itemize{
        #'     \item \code{"biplot"}: A scatter plot projecting variables onto two specified cluster axes, visualizing their associations (e.g., to Cluster 1 vs Cluster 2). Requires \code{ggrepel}.
        #'     \item \code{"representativeness"}: A bar chart showing the association strength (Cramer's V or Score) of each variable to its \strong{assigned} cluster. Useful for identifying core variables.
        #'     \item \code{"heatmap"}: A matrix plot visualizing the association strength between \strong{all} variables and \strong{all} clusters. Variables are typically ordered by their cluster group.
        #'   }
        #' @param axes Numeric vector of length 2, specifying the cluster axes to plot (e.g., \code{c(1, 2)} for the first two cluster dimensions in the biplot).
        #' @param ... Additional arguments passed to specific plot functions (currently unused).
        #' @return The generated \code{ggplot2} object (invisibly).
    plot = function(type = c("biplot", "representativeness", "heatmap"), axes = c(1, 2), ...) {
      if (is.null(self$score_matrix)) stop("Model must be fitted with $fit() before plotting.")
      type <- match.arg(type)

      # --- CHOICE OF METRICS ---
      # If Cramer's matrix has been computed (in fit()) it will be used for visualization
      # since it allows more nuances than Khi^2 test score (Intensity vs Significativity)
      if (!is.null(self$cramer_matrix)) {
        data_matrix <- self$cramer_matrix
        metric_label <- "Cramer's V (Association Strength)"
        # Biplot axes must be between 0 and 1 since Cramer's V max value is 1
        axis_limits <- c(-0.05, 1.05)
      } else {
        data_matrix <- self$score_matrix
        metric_label <- "Score (1 - p.value)"
        axis_limits <- c(-0.05, 1.1)
      }

      # --- 1. BIPLOT ---
      if (type == "biplot") {
        if (self$K < 2) stop("Biplot requires at least 2 clusters.")

        # Checking ggrepel package to avoid overlapping
        if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Package 'ggrepel' is required.")

        df_plot <- data.frame(
          variable = rownames(data_matrix),
          x = data_matrix[, axes[1]],
          y = data_matrix[, axes[2]],
          cluster = as.factor(self$clusters),
          stringsAsFactors = FALSE
        )

        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, color = cluster, label = variable)) +
          ggplot2::geom_hline(yintercept = 0, color = "gray80", linetype = "dashed") +
          ggplot2::geom_vline(xintercept = 0, color = "gray80", linetype = "dashed") +
          # Jitter léger pour éviter les superpositions parfaites
          ggplot2::geom_jitter(size = 3, alpha = 0.8, width = 0.01, height = 0.01) +
          ggrepel::geom_text_repel(size = 3.5, box.padding = 0.5, max.overlaps = 20) +
          ggplot2::scale_color_brewer(palette = "Set1", name = "Assigned Cluster") +
          ggplot2::labs(
            title = "Cluster Axes Projection (Biplot)",
            subtitle = paste0(metric_label, " on Cluster ", axes[1], " vs ", axes[2]),
            x = paste("Association to Cluster", axes[1]),
            y = paste("Association to Cluster", axes[2])
          ) +
          ggplot2::theme_minimal() +
          ggplot2::coord_fixed(ratio = 1) +
          ggplot2::scale_x_continuous(limits = axis_limits) +
          ggplot2::scale_y_continuous(limits = axis_limits)

        print(p)
        return(invisible(p))

        # --- 2. REPRESENTATIVENESS ---
      } else if (type == "representativeness") {

        # Getting back MAX value for the chosen metrics for each variable.
        vars <- names(self$clusters)
        vals <- numeric(length(vars))

        for(i in seq_along(vars)) {
          k <- self$clusters[i]
          vals[i] <- data_matrix[vars[i], k]
        }

        df_repr <- data.frame(
          variable = vars,
          cluster = as.factor(self$clusters),
          value = vals,
          stringsAsFactors = FALSE
        )

        p <- ggplot2::ggplot(df_repr, ggplot2::aes(x = reorder(variable, value), y = value, fill = cluster)) +
          ggplot2::geom_col(width = 0.7) +
          ggplot2::coord_flip() +
          ggplot2::labs(
            title = "Variable Representativeness",
            subtitle = paste("Metric:", metric_label),
            x = "Variable",
            y = metric_label
          ) +
          ggplot2::theme_minimal() +
          # Le Cramér est borné à 1, le score aussi
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::facet_wrap(~cluster, scales = "free_y", ncol = 1) +
          ggplot2::scale_fill_brewer(palette = "Set1", guide = "none")

        print(p)
        return(invisible(p))

        # --- 3. HEATMAP ---
      } else if (type == "heatmap") {

        # Transformation for ggplot
        df_heat <- reshape2::melt(data_matrix)
        colnames(df_heat) <- c("Variable", "Cluster", "Value")

        # Sorting variables
        if (!is.null(self$clusters)) {
          ord <- order(self$clusters)
          vars_ordered <- names(self$clusters)[ord]
          df_heat$Variable <- factor(df_heat$Variable, levels = vars_ordered)
        }

        p <- ggplot2::ggplot(df_heat, ggplot2::aes(x = Cluster, y = Variable, fill = Value)) +
          ggplot2::geom_tile(color = "white", linewidth = 0.2) +
          ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1,
                                        limits = c(0, 1), name = metric_label) +
          ggplot2::labs(title = "Association Heatmap",
                        subtitle = paste("Metric:", metric_label),
                        x = NULL, y = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(panel.grid = ggplot2::element_blank()) +
          ggplot2::coord_fixed(ratio = 0.5)

        print(p)
        return(invisible(p))
      }
    }
  )
)
