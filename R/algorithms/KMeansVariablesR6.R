#' K-Means Clustering for Variables
#'
#' @description
#' This class encapsulates the K-means algorithm for variable clustering.
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
    #' @field dist_strategy Strategy to handle correlation distances ("pam", "mds")
    dist_strategy = NULL,
    #' @field model K-means model object
    model = NULL,
    #' @field clusters List of variable names per cluster
    clusters = NULL,
    #' @field centers Cluster centers
    centers = NULL,
    #' @field fitted Logical indicating if model has been fitted
    fitted = FALSE,
    #' @field data_fit Normalized data used for fitting
    data_fit = NULL,
    #' @field scale_center Centering parameters from scaling
    scale_center = NULL,
    #' @field scale_scale Scaling parameters from scaling
    scale_scale = NULL,
    #' @field nstart Number of random starts passed to kmeans
    nstart = 25,
    #' @field seed Optional seed for reproducibility
    seed = NULL,

    #' @description
    #' Create a new KMeansVariablesR6 object
    #' @param k Number of clusters (default: 2)
    #' @param method Distance method: "correlation" or "euclidean" (default: "correlation")
    #' @return A new `KMeansVariablesR6` object
    initialize = function(k = 2,
                          method = c("correlation", "euclidean"),
                          dist_strategy = c("pam", "mds"),
                          nstart = 25,
                          seed = NULL) {
      # Validate and store core parameters
      self$k <- k
      self$method <- match.arg(method)
      self$dist_strategy <- match.arg(dist_strategy)
      self$nstart <- as.integer(nstart)
      self$seed <- if (!is.null(seed)) as.integer(seed) else NULL
    },

    #' @description
    #' Fit the K-means model on variables
    #' @param X A data.frame or matrix with numeric variables to cluster
    #' @return Self (invisibly) for method chaining
    fit = function(X) {
      # Input validations
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

      # Normalize data (center and scale)
      X_norm <- scale(X)
      self$data_fit <- X_norm
      # Store normalization parameters for predict()
      self$scale_center <- attr(X_norm, "scaled:center")
      self$scale_scale <- attr(X_norm, "scaled:scale")

      # Transpose to have variables as rows (observations)
      X_t <- t(X_norm)

      # Apply clustering on variables
      if (!is.null(self$seed)) set.seed(self$seed)

      if (self$method == "correlation") {
        # Use correlation-based distance
        cor_mat <- cor(X_norm)
        dist_mat <- 1 - abs(cor_mat)

        if (self$dist_strategy == "pam") {
          if (!requireNamespace("cluster", quietly = TRUE)) {
            stop("Package 'cluster' is required for dist_strategy = 'pam'.")
          }
          dis <- as.dist(dist_mat)
          pam_res <- cluster::pam(dis, k = self$k, diss = TRUE)
          # Wrap PAM result to behave similarly to kmeans for downstream usage
          km <- list(cluster = pam_res$clustering, centers = NULL, withinss = NA, tot.withinss = NA, betweenss = NA, pam = pam_res)
        } else if (self$dist_strategy == "mds") {
          # Embed distance matrix into Euclidean space then kmeans
          kdim <- min(max(2, self$k), max(2, ncol(X_norm) - 1))
          coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
          km <- kmeans(coords, centers = self$k, nstart = self$nstart)
        } else {
          stop("Unknown dist_strategy. Use 'pam' or 'mds'.")
        }
      } else if (self$method == "euclidean") {
        # Euclidean distance between variables (variables as rows)
        km <- kmeans(X_t, centers = self$k, nstart = self$nstart)
      } else {
        stop("Method not recognized. Use 'correlation' or 'euclidean'.")
      }

      self$model <- km
      # make sure clustering vector has variable names
      clust_vec <- km$cluster
      if (is.null(names(clust_vec))) names(clust_vec) <- colnames(X)
      self$clusters <- split(names(clust_vec), clust_vec)
      # store centers (may be NULL for PAM)
      self$centers <- km$centers
      self$fitted <- TRUE
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
      cat("KMeansVariablesR6 summary\n")
      cat("Number of clusters:", self$k, "\n")
      cat("Distance method:", self$method, "\n")
      if (self$fitted) {
        cat("Variables per cluster:\n")
        print(self$clusters)
        cat("\nCluster centers / representatives:\n")
        # Show representative variables (medoids or closest variables to centers)
        reps <- tryCatch(self$representative_variables(), error = function(e) NULL)
        if (!is.null(reps)) {
          rep_df <- data.frame(cluster = seq_along(reps), representative = unname(reps), stringsAsFactors = FALSE)
          print(rep_df)
        } else if (!is.null(self$centers)) {
          print(self$centers)
        } else if (!is.null(self$model$pam)) {
          cat("PAM medoids (representative variables):\n")
          print(self$model$pam$id.med)
        } else {
          cat("Centers not available for this strategy (e.g. PAM without explicit centers).\n")
        }
        # Show within-cluster dissimilarities / sums
        cat("\nCluster dissimilarities / within-cluster metrics:\n")
        diss <- tryCatch(self$get_cluster_diss(), error = function(e) NULL)
        if (!is.null(diss)) {
          print(diss)
        } else if (!is.null(self$model$withinss)) {
          print(self$model$withinss)
        } else {
          cat("No within-cluster metrics available for this strategy.\n")
        }
        cat("\nCluster sizes:\n")
        print(sapply(self$clusters, length))
        cat("\nTotal within-cluster dissimilarity / inertia:\n")
        total <- tryCatch(self$get_total_within_diss(), error = function(e) NA)
        print(total)
        if (!is.null(self$model$betweenss)) cat("Between-cluster sum of squares:", self$model$betweenss, "\n")
      } else {
        cat("Model not fitted yet.\n")
      }
    },

    #' @description
    #' Return representative variable (medoid or closest-to-center) for each cluster
    representative_variables = function() {
      if (!self$fitted) stop("Model not fitted")
      # PAM: use medoids
      if (!is.null(self$model$pam)) {
        ids <- self$model$pam$id.med
        vars <- colnames(self$data_fit)
        return(vars[ids])
      }

      # If centers are available, compute the nearest variable to each center
      if (!is.null(self$centers)) {
        if (self$method == "euclidean") {
          X_t <- t(self$data_fit)
          centers <- self$centers
          reps <- sapply(seq_len(nrow(centers)), function(k) {
            d <- apply(X_t, 1, function(r) sqrt(sum((r - centers[k, ])^2)))
            names(d)[which.min(d)]
          })
          return(reps)
        } else if (self$method == "correlation") {
          if (self$dist_strategy == "mds") {
            cor_mat <- cor(self$data_fit)
            dist_mat <- 1 - abs(cor_mat)
            # number of dimensions equals centers' columns
            kdim <- if (!is.null(self$centers)) ncol(self$centers) else 2
            coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
            centers <- self$centers
            reps <- sapply(seq_len(nrow(centers)), function(k) {
              d <- apply(coords, 1, function(r) sqrt(sum((r - centers[k, ])^2)))
              names(d)[which.min(d)]
            })
            return(reps)
          }
        }
      }
      return(NULL)
    },

    #' @description
    #' Compute within-cluster dissimilarity vector (one value per cluster)
    get_cluster_diss = function() {
      if (!self$fitted) stop("Model not fitted")
      if (!is.null(self$model$pam)) {
        # compute sum of pairwise dissimilarities within each cluster
        cor_mat <- cor(self$data_fit)
        dm <- as.matrix(1 - abs(cor_mat))
        cl <- self$model$pam$clustering
        res <- numeric(max(cl))
        for (c in unique(cl)) {
          members <- which(cl == c)
          if (length(members) > 1) {
            res[c] <- sum(dm[members, members]) / 2
          } else {
            res[c] <- 0
          }
        }
        return(res)
      } else if (!is.null(self$model$withinss)) {
        return(self$model$withinss)
      }
      return(NULL)
    },

    #' @description
    #' Compute total within-cluster dissimilarity (scalar)
    get_total_within_diss = function() {
      if (!self$fitted) stop("Model not fitted")
      if (!is.null(self$model$pam)) {
        vec <- self$get_cluster_diss()
        return(sum(vec, na.rm = TRUE))
      } else if (!is.null(self$model$tot.withinss)) {
        return(self$model$tot.withinss)
      }
      return(NA)
    },

    #' @description
    #' Elbow method to help choose optimal k
    #' @param X A data.frame or matrix with numeric variables
    #' @param max_k Maximum number of clusters to test (default: 10)
    #' @param plot Whether to plot the results (default: TRUE)
    #' @return Vector of within-cluster sum of squares for each k
    suggest_k = function(X, max_k = 10, plot = TRUE) {
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
      if (n_vars < 2) stop("At least two variables are required to suggest k.")
      max_k <- min(max_k, n_vars - 1)
      if (max_k < 1) stop("Not enough variables to suggest k for the given max_k.")
      wss <- numeric(max_k)

      # Normalize data
      X_norm <- scale(X)

      if (self$method == "correlation") {
        # Correlation-based distance
        cor_mat <- cor(X_norm)
        dist_mat <- 1 - abs(cor_mat)
        if (self$dist_strategy == "mds") {
          # Embed distances via MDS then compute WSS on coordinates
          kdim <- min(max(2, self$k), max(2, ncol(X_norm) - 1))
          coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
          for (k in 1:max_k) {
            km <- kmeans(coords, centers = k, nstart = self$nstart)
            wss[k] <- km$tot.withinss
          }
        } else if (self$dist_strategy == "pam") {
          if (!requireNamespace("cluster", quietly = TRUE)) stop("Package 'cluster' is required for dist_strategy = 'pam'.")
          # For PAM we compute total within-cluster dissimilarity (sum of pairwise distances within clusters)
          dm <- as.matrix(dist_mat)
          for (k in 1:max_k) {
            pam_res <- cluster::pam(as.dist(dist_mat), k = k, diss = TRUE)
            cl <- pam_res$clustering
            total_within <- 0
            for (clu in unique(cl)) {
              members <- which(cl == clu)
              if (length(members) > 1) {
                # sum of pairwise distances inside cluster (divide by 2 because symmetric)
                total_within <- total_within + sum(dm[members, members]) / 2
              }
            }
            wss[k] <- total_within
          }
        } else {
          stop("Unknown dist_strategy in suggest_k.")
        }
      } else if (self$method == "euclidean") {
        # Euclidean distance between variables
        X_t <- t(X_norm)

        for (k in 1:max_k) {
          km <- kmeans(X_t, centers = k, nstart = self$nstart)
          wss[k] <- km$tot.withinss
        }
      } else {
        stop("Method not recognized. Use 'correlation' or 'euclidean'.")
      }

      if (plot) {
        plot(1:max_k, wss,
          type = "b", pch = 19, frame = FALSE,
          xlab = "Number of clusters k",
          ylab = "Within-cluster sum of squares",
          main = "Elbow method for choosing k"
        )
      }
      return(wss)
    },

    #' @description
    #' Automatically suggest optimal k using statistical methods
    #' @param X A data.frame or matrix with numeric variables
    #' @param max_k Maximum number of clusters to test (default: 10)
    #' @param method Method to use: "silhouette" or "gap" (default: "silhouette")
    #' @return Optimal number of clusters
    suggest_k_automatic = function(X, max_k = 10, method = "silhouette") {
      # Basic validations
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("X must be a data.frame or matrix.")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("All variables must be numeric.")
      }
      if (anyNA(X)) {
        stop("Data must not contain missing values (NA).")
      }

      # Prepare data: normalize
      X_norm <- scale(X)
      n_vars <- ncol(X_norm)
      max_k <- min(max_k, n_vars - 1)
      if (max_k < 2) stop("Not enough variables to run automatic k suggestion. Increase data dimensionality or reduce max_k.")
      if (!is.null(self$seed)) set.seed(self$seed)

      if (method == "silhouette") {
        # Silhouette method
        if (!requireNamespace("cluster", quietly = TRUE)) {
          stop("Package 'cluster' is required for silhouette method.")
        }

        sil_widths <- numeric(max_k - 1)

        if (self$method == "correlation") {
          cor_mat <- cor(X_norm)
          dist_mat <- 1 - abs(cor_mat)

          if (self$dist_strategy == "mds") {
            kdim <- min(max(2, self$k), max(2, ncol(X_norm) - 1))
            coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
            for (k in 2:max_k) {
              km <- kmeans(coords, centers = k, nstart = self$nstart)
              sil <- cluster::silhouette(km$cluster, dist(coords))
              sil_widths[k - 1] <- mean(sil[, 3])
            }
          } else if (self$dist_strategy == "pam") {
            for (k in 2:max_k) {
              pam_res <- cluster::pam(as.dist(dist_mat), k = k, diss = TRUE)
              sil <- cluster::silhouette(pam_res$clustering, as.dist(dist_mat))
              sil_widths[k - 1] <- mean(sil[, 3])
            }
          } else {
            stop("Unknown dist_strategy in suggest_k_automatic (silhouette).")
          }
        } else {
          # Euclidean distance
          X_t <- t(X_norm)

          for (k in 2:max_k) {
            km <- kmeans(X_t, centers = k, nstart = self$nstart)
            sil <- cluster::silhouette(km$cluster, dist(X_t))
            sil_widths[k - 1] <- mean(sil[, 3])
          }
        }

        optimal_k <- which.max(sil_widths) + 1

        if (interactive()) {
          plot(2:max_k, sil_widths,
            type = "b", pch = 19, frame = FALSE,
            xlab = "Number of clusters k",
            ylab = "Average silhouette width",
            main = "Silhouette method (variable clustering)"
          )
          abline(v = optimal_k, col = "red", lty = 2)
        }

        message("\u2713 Optimal number of clusters (silhouette): ", optimal_k)
        return(optimal_k)
      } else if (method == "gap") {
        # Gap statistic method
        if (!requireNamespace("cluster", quietly = TRUE)) {
          stop("Package 'cluster' is required for gap statistic method.")
        }

        set.seed(123) # For reproducibility

        if (self$method == "correlation") {
          cor_mat <- cor(X_norm)
          dist_mat <- 1 - abs(cor_mat)

          if (self$dist_strategy == "mds") {
            kdim <- min(max(2, self$k), max(2, ncol(X_norm) - 1))
            coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
            gap_stat <- cluster::clusGap(coords,
              FUN = function(x, k) kmeans(x, centers = k, nstart = 25),
              K.max = max_k, B = 50
            )
          } else if (self$dist_strategy == "pam") {
            # PAM does not directly accept clusGap; approximate via MDS embedding
            kdim <- min(max(2, self$k), max(2, ncol(X_norm) - 1))
            coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
            gap_stat <- cluster::clusGap(coords,
              FUN = function(x, k) kmeans(x, centers = k, nstart = 25),
              K.max = max_k, B = 50
            )
          } else {
            stop("Unknown dist_strategy in suggest_k_automatic (gap).")
          }
        } else {
          X_t <- t(X_norm)

          gap_stat <- cluster::clusGap(X_t,
            FUN = function(x, k) kmeans(x, centers = k, nstart = 25),
            K.max = max_k, B = 50
          )
        }

        optimal_k <- cluster::maxSE(gap_stat$Tab[, "gap"],
          gap_stat$Tab[, "SE.sim"],
          method = "Tibs2001SEmax"
        )

        if (interactive()) {
          plot(gap_stat, main = "Gap statistic method (variable clustering)")
          abline(v = optimal_k, col = "red", lty = 2)
        }

        message("\u2713 Optimal number of clusters (gap statistic): ", optimal_k)
        return(optimal_k)
      } else {
        stop("Method not recognized. Use 'silhouette' or 'gap'.")
      }
    }
  )
)
