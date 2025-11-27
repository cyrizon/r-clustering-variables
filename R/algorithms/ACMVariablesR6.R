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
	#' @field score_matrix matrix (p x K) containing the association strength between each variable and each cluster axis; the score corresponds to 1-p_value from KHI² test
	score_matrix = NULL,
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
    #' @param data A data.frame with categorical variables (must be of type factor).
    #' @param K Number of clusters (integer).
    #' @param max_iter Maximal number of iterations for the clustering algorithm (integer, default 30).
    #' @param tol Tolerance for stop criterion (numeric, default 1e-4).
    #' @return A new `ClustVarACM` object.
    initialize = function(data, K, max_iter = 30, tol = 1e-4) {
	  # Input validation is performed in fit() to allow initialisation without data if needed,
      # but here we perform initial checks and storage.
      self$data <- data
      self$K <- K
      self$max_iter <- max_iter
      self$tol <- tol
	  # Initial random cluster assignment
      self$clusters <- sample(1:K, ncol(data), replace = TRUE)
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
          if (!all(sapply(data, is.factor))) stop("All variables must be categorical (type factor).")
          if (anyNA(self$data)) stop("Data must not contain NA values.")
          if (self$K < 2 || self$K > p) stop("K must be between 2 and the number of variables.")
    
		# Main iterative algorithm (K-means for variables)
      for (iter in 1:self$max_iter) {
        # 1. Axis calculation (Synthetic variable for each cluster via MCA)
        self$axes_list <- vector("list", self$K)
        for (k in 1:self$K) {
          vars_k <- names(self$data)[self$clusters == k]
          if (length(vars_k) == 0) {
            # Empty cluster
            self$axes_list[[k]] <- NULL
          } else if (length(vars_k) == 1) {
            # Cluster with one variable: use the variable's numerical representation as axis
            self$axes_list[[k]] <- scale(as.numeric(self$data[[vars_k[1]]]), center = TRUE, scale = FALSE)
          } else {
            # Cluster with multiple variables: use the first principal component (FPC) from MCA
            acm_k <- FactoMineR::MCA(self$data[, vars_k, drop = FALSE], ncp = 1, graph = FALSE)
            self$axes_list[[k]] <- acm_k$ind$coord[, 1]
          }
        }

        # 2. Reallocation through KHI² test score calculation
        self$score_matrix <- matrix(0, nrow = p, ncol = self$K,
                                    dimnames=list(names(self$data), paste0("Cluster_",1:self$K)))
        
        for (j in 1:p) { # Iterate over all variables
          fac <- self$data[[j]]
          
          for (k in 1:self$K) { # Iterate over all cluster axes
            if (!is.null(self$axes_list[[k]])) {
              # Discretize the cluster axis into 5 classes
              zdisc <- cut(self$axes_list[[k]], breaks = 5, include.lowest = TRUE, ordered_result = TRUE)
              
              # Contingency table between variable j and discretized cluster axis k
              tab <- table(fac, zdisc)
              
              if (any(rowSums(tab)==0) || any(colSums(tab)==0)) {
                score <- 0 # Avoid chi-square test with zero marginals
              } else {
				          # Calculate the association: 1 - p_value of the Khi² test
				          test <- suppressWarnings(chisq.test(tab, correct = FALSE))
				          score <- 1 - test$p.value
              }
              
              self$score_matrix[j,k] <- score
              
            } else {
				        # Cluster is empty
				        self$score_matrix[j,k] <- -Inf
            }
          }
        }
        
        # Assign each variable to the cluster with the maximum association score
		      new_clusters <- apply(self$score_matrix, 1, which.max)
        
        # 3. Criterion Q calculation
		    # Q is the sum of the maximum association scores (intra-cluster quality)
        Q_new <- sum(sapply(1:p, function(j) self$score_matrix[j, new_clusters[j]]), na.rm = TRUE)
        self$Q_trace <- c(self$Q_trace, Q_new)

         # 4. Stop criterion
        if (abs(Q_new - Q_old) < self$tol) {
          break
        }
        
        # Update clusters for the next iteration
		    self$clusters <- new_clusters
        Q_old <- Q_new
      }
      
      self$Q_final <- Q_old
      invisible(self)
    },
                            
    #' @description
    #' Print a succinct summary of the model.
    #' @return Nothing (invisible NULL).
    print = function() {
      if (is.null(self$Q_final)) {
        cat("Clustering Model: ClustVarACM (K=", self$K, ") - Not yet fitted.\n", sep="")
      } else {
        cat("Clustering Model: ClustVarACM\n")
        cat("  - Number of variables: ", ncol(self$data), "\n", sep="")
        cat("  - Number of clusters (K): ", self$K, "\n", sep="")
        cat("  - Final Criterion Q: ", round(self$Q_final, 4), "\n", sep="")
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
    #' Plot the variables in the space of the cluster axes (Factorial Map).
    #' This is a conceptual representation, visualizing the association between variables and cluster synthetic axes.
    #' @param axes Numeric vector of length 2, specifying the cluster axes to plot (e.g., c(1, 2)).
    #' @return Nothing (invisible NULL), generates a plot.
    plot = function(axes = 1:2) {
      if (is.null(self$score_matrix)) stop("The model has not been fitted yet (run fit()).")
      if (self$K < 2) stop("Plotting requires at least 2 clusters.")
      if (length(axes) != 2 || any(axes < 1) || any(axes > self$K)) {
        stop("The 'axes' parameter must be a vector of two distinct integers between 1 and K.")
      }
      
      # Use the association scores for visualization
      # We project the variables onto the space defined by two cluster axes
      scores_for_plot <- self$score_matrix[, axes, drop = FALSE]
      
      plot(scores_for_plot,
           pch = 19,
           col = self$clusters, # Color points by their assigned cluster
           xlab = paste("Association Score to Cluster", axes[1]),
           ylab = paste("Association Score to Cluster", axes[2]),
           main = "Projection des Variables sur les Axes de Clusters",
           sub = "Couleur = Cluster Assigné (Score = 1 - p-value du Khi²)",
           asp = 1 # Keep the axes at the same scale
      )
      
      text(scores_for_plot, labels = names(self$data), pos = 4, col = self$clusters)
      abline(h = 0, v = 0, lty = 2, col = "gray")
      
      # Add legend for clusters
      legend_text <- paste("Cluster", 1:self$K)
      legend("topright", legend = legend_text, col = 1:self$K, pch = 19, title = "Clusters")
      
      invisible(NULL)
    },

                            
    #' @description
    #' Automatic selection for the optimal number of clusters K using the elbow method on the Q criterion.
    #' @param K_grid Integer vector of K values to test (e.g., 2:6).
    #' @param threshold Numeric tolerance for the relative gain in Q (default 0.1, or 10%).
    #' @return A list containing the results (K and Q values) and the suggested optimal K.
    select_K = function(K_grid = 2:6, threshold = 0.1) {
      if (is.null(self$data)) stop("Data must be provided to select K.")
      if (min(K_grid) < 2 || max(K_grid) > ncol(self$data)) stop("K_grid must be within [2, number of variables].")
      
      results <- data.frame(K = K_grid, Q = NA)
      cat(sprintf("Testing K from %d to %d...\n", min(K_grid), max(K_grid)))
      
      for (i in seq_along(K_grid)) {
        tmp <- ClustVarACM$new(data = self$data, K = K_grid[i])
        tmp$fit()
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
      plot(results$K, results$Q, type = "b", pch = 19, col = "blue",
           xlab = "Nombre de clusters (K)", ylab = "Critère global Q",
           main = "Méthode du coude pour la sélection du K optimal")
      abline(v = K_opt, col = "red", lty = 2)
      text(K_opt, max(results$Q), labels = paste("K* =", K_opt), pos = 4, col = "red")
      
      cat(sprintf("→ Optimal K automatically selected: %d (threshold %.0f%%)\n", K_opt, threshold*100))
      return(invisible(list(results = results, K_opt = K_opt)))
    },

                            
    #' @description
    #' Visualization for Q convergence (tracking Q across iterations).
    #' @return Nothing (invisible NULL), generates a plot.
    plot_Q = function() {
      if (is.null(self$Q_trace)) stop("The model has not been fitted yet (fit() must be run).")
      plot(self$Q_trace, type = "b", pch = 19, col = "blue",
           xlab = "Iteration", ylab = "Criterion Q",
           main = "Evolution of Criterion Q")
      abline(h = self$Q_final, col = "red", lty = 2)
      text(length(self$Q_trace), self$Q_final, labels = "Q_final", pos = 1, col = "red")
    },

                            
    #' @description
    #' Predict cluster membership for new illustrative variables.
    #' The new variables are not used to modify the clusters (non-active).
    #' @param X A data.frame with categorical variables to classify (must have the same number of observations as the training data).
    #' @return A data.frame with variable names and their assigned cluster.
    predict = function(newdata) {
      if if (is.null(self$axes_list) || all(sapply(self$axes_list, is.null))) {
        stop("The model must be fitted (fit() must be run) before prediction.")
      }
      if (nrow(X) != nrow(self$data)) {
        stop("The number of observations in 'X' must be the same as in the training data.")
      }
      if (!all(sapply(X, is.factor))) {
        warning("All variables in 'X' should be factors for proper analysis.")
      }
      if (!is.data.frame(newdata)){
        stop("Your object must be a data.frame")
      }
    


      p_new <- ncol(X)
      score_new <- matrix(0, nrow=p_new, ncol=self$K,
                          dimnames=list(names(X), paste0("Cluster_",1:self$K)))

      for (j in 1:p_new) { # Iterate over new variables
        fac <- as.factor(X[[j]])
        
        for (k in 1:self$K) { # Test association with each cluster axis
          if (!is.null(self$axes_list[[k]])) {
            
            # Discretize the cluster axis (based on training data)
            zdisc <- cut(self$axes_list[[k]], breaks = 5, include.lowest = TRUE, ordered_result = TRUE)
            
            # Contingency table
            tab <- table(fac, zdisc)
            
            if (any(rowSums(tab)==0) || any(colSums(tab)==0)) {
              score <- 0
            } else {
              test <- suppressWarnings(chisq.test(tab, correct = FALSE))
              score <- 1 - test$p.value
            }
            
            score_new[j,k] <- score
            
          } else {
            score_new[j,k] <- -Inf
          }
        }
      }
      
      clusters_pred <- apply(score_new, 1, which.max)
      
      # Prepare the output: variable, assigned cluster, and max score (association strength)
      max_scores <- apply(score_new, 1, max, na.rm = TRUE)
      
      return(data.frame(
        Variable = names(X),
        Cluster_Assigned = clusters_pred,
        Max_Association_Score = max_scores
      ))
    }
  )
)
