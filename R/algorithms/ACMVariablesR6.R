# R6 class for categorical variable clustering via MCA (ACM)

# --- Utility function: compute η² (effect size measure)
eta2_manual <- function(fac, z) {
  fac <- as.factor(fac)
  z <- as.numeric(z)
  ss_total <- sum((z - mean(z))^2)
  if (ss_total == 0) {
    return(0)
  }
  ss_between <- sum(tapply(z, fac, function(v) length(v) * (mean(v) - mean(z))^2))
  return(ss_between / ss_total)
}

# This class encapsulates the categorical variable clustering algorithm via MCA (ACM)
ACMVariablesR6 <- R6::R6Class(
  classname = "ACMVariablesR6",
  public = list(
    # === Attributes ===
    k = NULL,
    max_iter = NULL,
    tol = NULL,
    verbose = NULL,
    clusters = NULL,
    axes_list = NULL,
    Q_trace = NULL,
    Q_final = NULL,
    eta2_matrix = NULL,
    fitted = FALSE,
    data_fit = NULL,

    # Constructor
    initialize = function(k = 3, max_iter = 30, tol = 1e-4, verbose = FALSE) {
      self$k <- k
      self$max_iter <- max_iter
      self$tol <- tol
      self$verbose <- verbose
      self$fitted <- FALSE
    },


    # Fit method
    fit = function(X) {
      # Step 0: input checks
      if (!is.data.frame(X)) stop("X must be a data.frame")
      if (!all(sapply(X, function(x) is.factor(x) || is.character(x)))) {
        stop("All variables must be categorical (factor or character).")
      }
      if (anyNA(X)) stop("Data must not contain NA.")
      if (self$k < 2 || self$k > ncol(X)) stop("k must be between 2 and the number of variables.")

      # Store the data
      self$data_fit <- X

      # Initialize clusters randomly
      self$clusters <- sample(1:self$k, ncol(X), replace = TRUE)

      p <- ncol(X)
      Q_old <- 0
      self$Q_trace <- c()

      for (iter in 1:self$max_iter) {
        # Step 1: compute axes (MCA)
        self$axes_list <- vector("list", self$k)
        for (clust_idx in 1:self$k) {
          vars_k <- names(self$data_fit)[self$clusters == clust_idx]
          if (length(vars_k) == 0) {
            self$axes_list[[clust_idx]] <- NULL
          } else if (length(vars_k) == 1) {
            self$axes_list[[clust_idx]] <- scale(as.numeric(as.factor(self$data_fit[[vars_k[1]]])), center = TRUE, scale = FALSE)
          } else {
            acm_k <- FactoMineR::MCA(self$data_fit[, vars_k, drop = FALSE], ncp = 1, graph = FALSE)
            self$axes_list[[clust_idx]] <- acm_k$ind$coord[, 1]
          }
        }

        # Step 2: reallocation
        self$eta2_matrix <- matrix(0,
          nrow = p, ncol = self$k,
          dimnames = list(names(self$data_fit), paste0("Cluster_", 1:self$k))
        )
        for (j in 1:p) {
          for (clust_idx in 1:self$k) {
            if (!is.null(self$axes_list[[clust_idx]])) {
              self$eta2_matrix[j, clust_idx] <- eta2_manual(self$data_fit[[j]], self$axes_list[[clust_idx]])
            } else {
              self$eta2_matrix[j, clust_idx] <- NA
            }
          }
        }
        new_clusters <- apply(self$eta2_matrix, 1, which.max)

        # Step 3: compute the Q criterion
        Q_new <- sum(sapply(1:p, function(j) self$eta2_matrix[j, new_clusters[j]]), na.rm = TRUE)
        self$Q_trace <- c(self$Q_trace, Q_new)
        if (self$verbose) cat(sprintf("Iteration %2d: Q = %.4f\n", iter, Q_new))

        # Stopping criterion
        if (abs(Q_new - Q_old) < self$tol) {
          if (self$verbose) cat("-> Convergence reached (Q change < tolerance)\n")
          break
        }

        self$clusters <- new_clusters
        Q_old <- Q_new
      }

      self$Q_final <- Q_old
      self$fitted <- TRUE
      invisible(self)
    },

    # Summary method
    summary = function() {
      cat("---- Model Summary ----\n")
      cat("Number of clusters:", self$k, "\n")
      cat("Final Q criterion:", round(self$Q_final, 4), "\n")
      cat("Average quality (Q/p):", round(self$Q_final / ncol(self$data_fit), 4), "\n\n")
      cat("Final partition of variables:\n")
      print(data.frame(Variable = names(self$data_fit), Cluster = self$clusters))

      invisible(list(
        clusters = self$clusters,
        Q = self$Q_final,
        Q_normalized = self$Q_final / ncol(self$data_fit)
      ))
    },

    # Automatic selection of optimal k
    select_k = function(k_grid = 2:6, threshold = 0.1) {
      if (!self$fitted) stop("Model must be fitted before selecting optimal k.")
      results <- data.frame(k = k_grid, Q = NA)
      for (i in seq_along(k_grid)) {
        if (self$verbose) cat("Testing k =", k_grid[i], "\n")
        tmp <- ACMVariablesR6$new(k = k_grid[i], verbose = FALSE)
        tmp$fit(self$data_fit)
        results$Q[i] <- tmp$Q_final
      }

      # Compute successive differences of Q
      dQ <- diff(results$Q)
      rel_gain <- dQ / max(dQ)
      k_opt <- results$k[which(rel_gain < threshold)[1] + 1]
      if (is.na(k_opt)) k_opt <- results$k[which.max(results$Q)]

      # Visualization
      plot(results$k, results$Q,
        type = "b", pch = 19, col = "blue",
        xlab = "Number of clusters (k)", ylab = "Global criterion Q",
        main = "Elbow method for optimal k selection"
      )
      abline(v = k_opt, col = "red", lty = 2)
      text(k_opt, max(results$Q), labels = paste("k* =", k_opt), pos = 4, col = "red")

      cat(sprintf("→ automatically selected optimal k: %d (threshold %.0f%%)\n", k_opt, threshold * 100))
      return(list(results = results, k_opt = k_opt))
    },

    # Method for plotting the Q convergence
    plot_Q = function() {
      if (is.null(self$Q_trace)) stop("The model has not been fitted yet (fit() not executed).")
      plot(self$Q_trace,
        type = "b", pch = 19, col = "blue",
        xlab = "Iteration", ylab = "Criterion Q",
        main = "Q criterion evolution"
      )
    },

    # Predict method for new variables
    predict = function(newdata) {
      if (is.null(self$axes_list)) {
        stop("The model must be trained with fit() before using predict().")
      }
      if (!is.data.frame(newdata)) stop("Your object must be a data.frame")
      if (!all(sapply(newdata, function(x) is.factor(x) || is.character(x)))) {
        stop("All variables must be categorical (factor or character).")
      }


      p_new <- ncol(newdata)
      eta2_mat_new <- matrix(0,
        nrow = p_new, ncol = self$k,
        dimnames = list(names(newdata), paste0("Cluster_", 1:self$k))
      )

      for (j in 1:p_new) {
        for (clust_idx in 1:self$k) {
          if (!is.null(self$axes_list[[clust_idx]])) {
            eta2_mat_new[j, clust_idx] <- eta2_manual(newdata[[j]], self$axes_list[[clust_idx]])
          } else {
            eta2_mat_new[j, clust_idx] <- NA
          }
        }
      }

      clusters_pred <- apply(eta2_mat_new, 1, which.max)
      cat("-> New variables assigned to existing clusters.\n")
      return(data.frame(Variable = names(newdata), Cluster_Assigned = clusters_pred))
    }
  )
)
