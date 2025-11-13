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

        #' @description
        #' Create a new KMeansVariablesR6 object
        #' @param k Number of clusters (default: 2)
        #' @param method Distance method: "correlation" or "euclidean" (default: "correlation")
        #' @return A new `KMeansVariablesR6` object
        initialize = function(k = 2, method = "correlation") {
            self$k <- k
            self$method <- method
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
            
            # Normalize data (center and scale)
            X_norm <- scale(X)
            self$data_fit <- X_norm
            # Store normalization parameters for predict()
            self$scale_center <- attr(X_norm, "scaled:center")
            self$scale_scale <- attr(X_norm, "scaled:scale")
            
            # Transpose to have variables as rows (observations)
            X_t <- t(X_norm)
            
            # Apply K-means on variables
            if (self$method == "correlation") {
                # Use correlation-based distance
                cor_mat <- cor(X_norm)
                dist_mat <- 1 - abs(cor_mat)
                km <- kmeans(dist_mat, centers = self$k, nstart = 25)
            } else if (self$method == "euclidean") {
                # Euclidean distance between variables
                km <- kmeans(X_t, centers = self$k, nstart = 25)
            } else {
                stop("Method not recognized. Use 'correlation' or 'euclidean'.")
            }
            
            self$model <- km
            self$clusters <- split(colnames(X), km$cluster)
            self$centers <- km$centers
            self$fitted <- TRUE
            invisible(self)
        },

        #' @description
        #' Predict cluster membership for new variables
        #' @param X A data.frame or matrix with numeric variables to classify
        #' @return A data.frame with variable names, assigned clusters, and distances
        predict = function(X) {
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
                stop("X must have the same number of observations (rows) as training data. Expected: ", 
                     nrow(self$data_fit), ", got: ", nrow(X))
            }
            
            # Normalize X with its own statistics (not training data stats)
            # This is correct because we're clustering VARIABLES, not observations
            X_norm <- scale(X)
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
                                    0
                                }
                            })
                            dist <- 1 - mean(correlations, na.rm = TRUE)
                            
                            if (dist < min_dist) {
                                min_dist <- dist
                                best_cluster <- k
                            }
                        }
                    }
                    clusters_pred[i] <- best_cluster
                    distances_pred[i] <- min_dist
                }
            } else if (self$method == "euclidean") {
                # For euclidean: calculate distance based on correlation with training variables
                # Since we're in variable space, use correlation-based approach
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
                                    Inf
                                }
                            })
                            dist <- mean(distances_to_cluster, na.rm = TRUE)
                            
                            if (dist < min_dist) {
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
                cat("\nCluster centers:\n")
                print(self$centers)
                cat("\nCluster sizes:\n")
                print(sapply(self$clusters, length))
                cat("\nWithin-cluster sum of squares:\n")
                print(self$model$withinss)
                cat("\nTotal within-cluster sum of squares:", self$model$tot.withinss, "\n")
                cat("Between-cluster sum of squares:", self$model$betweenss, "\n")
            } else {
                cat("Model not fitted yet.\n")
            }
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
            max_k <- min(max_k, n_vars - 1)
            wss <- numeric(max_k)
            
            # Normalize data
            X_norm <- scale(X)
            
            if (self$method == "correlation") {
                # Correlation-based distance
                cor_mat <- cor(X_norm)
                dist_mat <- 1 - abs(cor_mat)
                
                for (k in 1:max_k) {
                    km <- kmeans(dist_mat, centers = k, nstart = 25)
                    wss[k] <- km$tot.withinss
                }
            } else if (self$method == "euclidean") {
                # Euclidean distance between variables
                X_t <- t(X_norm)
                
                for (k in 1:max_k) {
                    km <- kmeans(X_t, centers = k, nstart = 25)
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
            max_k <- min(max_k, floor(n_vars / 2))

            if (method == "silhouette") {
                # Silhouette method
                if (!requireNamespace("cluster", quietly = TRUE)) {
                    stop("Package 'cluster' is required for silhouette method.")
                }

                sil_widths <- numeric(max_k - 1)
                
                if (self$method == "correlation") {
                    # Use correlation-based distance
                    cor_mat <- cor(X_norm)
                    dist_mat <- 1 - abs(cor_mat)
                    
                    for (k in 2:max_k) {
                        km <- kmeans(dist_mat, centers = k, nstart = 25)
                        sil <- cluster::silhouette(km$cluster, as.dist(dist_mat))
                        sil_widths[k - 1] <- mean(sil[, 3])
                    }
                } else {
                    # Euclidean distance
                    X_t <- t(X_norm)
                    
                    for (k in 2:max_k) {
                        km <- kmeans(X_t, centers = k, nstart = 25)
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
                    
                    gap_stat <- cluster::clusGap(dist_mat,
                        FUN = function(x, k) kmeans(x, centers = k, nstart = 25),
                        K.max = max_k, B = 50
                    )
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
