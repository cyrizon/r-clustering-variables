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
        # X must contain the same observations (rows) as the training data, in the same order.
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

        # plot: Draw the hierarchical clustering tree (dendrogram)
        #' @description
        #' Plots the dendrogram resulting from the HAC.
        #' @param K Integer, the number of clusters to highlight on the plot (defaults to the model's K).
        #' @return The object itself (invisibly), generates a plot
        plot = function(K = self$K) {
            if (!self$fitted) stop("Model must be fitted with $fit() before plotting.")

            # The horiz=TRUE argument is passed directly to plot.hclust
            # R may produce warnings when other parameters (like xlab, ylab)
            # are used simultaneously because they are handled differently.

            # For horizontal rendering: use the standard plot.hclust parameter
            # Note: simplify labels to reduce the risk of plotting parameter warnings.

            plot(self$model,
                main = paste("HAC of Variables (Linkage:", self$linkage_method, ")"),
                xlab = "Distance",
                ylab = "Variables",
                sub = "",
                horiz = TRUE
            )

            # Fix for namespace issue (use stats::)
            if (K > 1 && K < length(self$model$labels)) {
                stats::rect.hclust(self$model, k = K, border = 2:(K + 1))
            }
            invisible(self)
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
